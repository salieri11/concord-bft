// Copyright 2019 VMware, all rights reserved

#include "daml_kvb_commands_handler.hpp"

#include <google/protobuf/util/time_util.h>
#include <opentracing/tracer.h>
#include <chrono>
#include <iomanip>
#include <map>
#include <string>
#include "Logger.hpp"
#include "concord_storage.pb.h"
#include "sha_hash.hpp"
#include "sparse_merkle/base_types.h"
#include "storage/kvb_key_types.h"
#include "time/time_contract.hpp"

using std::map;
using std::string;
using std::vector;

using concord::kvbc::BlockId;
using concord::kvbc::IBlocksAppender;
using concord::kvbc::ILocalKeyValueStorageReadOnly;
using concord::kvbc::Key;
using concord::kvbc::KeyValuePair;
using concord::kvbc::SetOfKeyValuePairs;
using concord::kvbc::Value;
using concord::time::TimeContract;
using concordUtils::Sliver;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::DamlResponse;
using com::vmware::concord::kvb::ValueWithTrids;

using google::protobuf::util::TimeUtil;

namespace da_kvbc = com::digitalasset::kvbc;
using namespace concord::kvbc::sparse_merkle;
using namespace concord::util;

namespace concord {
namespace daml {

Sliver CreateSliver(const char* content, size_t size) {
  char* val = new char[size];
  memcpy(val, content, size);
  return Sliver(val, size);
}

Sliver CreateSliver(const string& content) {
  return CreateSliver(content.c_str(), content.size());
}

// TODO: This should be hidden in a separate DAML storage implementation
string PrefixDamlKey(const string& content) {
  string full_key(content);
  full_key.insert(0, &concord::storage::kKvbKeyDaml,
                  sizeof(concord::storage::kKvbKeyDaml));
  return full_key;
}

Sliver CreateDamlKvbKey(const string& content) {
  string full_key = PrefixDamlKey(content);
  return CreateSliver(full_key.c_str(), full_key.size());
}

Sliver CreateDamlKvbValue(const string& value,
                          const vector<string>& trid_list) {
  ValueWithTrids proto;
  proto.set_value(value);
  for (const auto& trid : trid_list) {
    proto.add_trid(trid);
  }

  size_t size = proto.ByteSizeLong();
  char* data = new char[size];
  proto.SerializeToArray(data, size);

  return Sliver(data, size);
}

Sliver CreateDamlKvbValue(const ValueWithTrids& proto) {
  size_t size = proto.ByteSizeLong();
  char* data = new char[size];
  proto.SerializeToArray(data, size);
  return Sliver(data, size);
}

void AccessControlListToThinReplicaIds(
    const com::digitalasset::kvbc::AccessControlList& access_control_list,
    std::vector<string>& thin_replica_ids) {
  assert(thin_replica_ids.empty());

  if (access_control_list.has_restricted() &&
      access_control_list.restricted().participant_id_size() > 0) {
    auto restricted_access = access_control_list.restricted();
    // The assumption here is that that participant IDs have already been
    // sorted deterministically by the sender, so they are not being sorted
    // again
    for (const auto& participant_id : restricted_access.participant_id()) {
      thin_replica_ids.push_back(participant_id);
    }
  }
}

// These constants must be identical to what is expected by participant state
// reader in Ledger API Server.
const std::string kLogFragmentKeyPrefix{"F"};
const std::string kTimeUpdateKeySuffix{"_TIME_UPDATE_KEY"};
std::string DamlKvbCommandsHandler::kTimeUpdateKey =
    kLogFragmentKeyPrefix + kTimeUpdateKeySuffix;

bool DamlKvbCommandsHandler::ExecuteRead(const da_kvbc::ReadCommand& request,
                                         ConcordResponse& response) {
  read_ops_.Increment();
  LOG_WARN(logger_, "NOT IMPLEMENTED");
  return false;
}

std::map<string, std::pair<string, BlockId>>
DamlKvbCommandsHandler::GetFromStorage(
    const google::protobuf::RepeatedPtrField<string>& keys) {
  const BlockId latest_block_id = storage_.getLastBlock();
  std::map<string, std::pair<string, BlockId>> result;
  for (const auto& skey : keys) {
    Value out;
    Key key = CreateDamlKvbKey(skey);
    BlockId actual_block_id = 0;
    if (!storage_.get(latest_block_id, key, out, actual_block_id).isOK()) {
      std::stringstream msg;
      msg << "Couldn't find key " << skey;
      throw std::runtime_error(msg.str());
    }

    if (out.length() == 0) {
      // TODO: Why do we have a DAML key w/ an empty value in the KVB?
      result[skey] = std::make_pair(string(), actual_block_id);
      continue;
    }

    ValueWithTrids proto;
    if (!proto.ParseFromArray(out.data(), out.length())) {
      std::stringstream msg;
      msg << "Couldn't decode ValueWithTrids for " << skey;
      throw std::runtime_error(msg.str());
    }
    if (!proto.has_value()) {
      std::stringstream msg;
      msg << "Couldn't find value in ValueWithTrids for " << skey;
      throw std::runtime_error(msg.str());
    }
    result[skey] = std::make_pair(proto.value(), actual_block_id);
  }
  if (enable_histograms_or_summaries) {
    read_keys_count_.Observe(keys.size());
  }
  return result;
}

bool DamlKvbCommandsHandler::ExecuteCommit(
    const da_kvbc::CommitRequest& commit_req, const uint8_t flags,
    TimeContract* time, const opentracing::Span& parent_span,
    ConcordResponse& concord_response) {
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  if (pre_execute) {
    return PreExecute(commit_req, parent_span, concord_response);
  }

  auto start = std::chrono::steady_clock::now();
  std::string span_name = "daml_execute_commit_pipelined";
  auto execute_commit_span = parent_span.tracer().StartSpan(
      span_name, {opentracing::ChildOf(&parent_span.context())});
  std::string correlation_id = commit_req.correlation_id();
  BlockId current_block_id = storage_.getLastBlock();
  google::protobuf::Timestamp record_time = RecordTimeForTimeContract(time);

  LOG_INFO(logger_,
           "Handle DAML commit command, time: "
               << TimeUtil::ToString(record_time) << ", clock: "
               << std::chrono::steady_clock::now().time_since_epoch().count());

  // The set of keys used during validation. Includes reads of keys
  // cached by the validator.
  vector<string> read_set;
  SetOfKeyValuePairs updates;
  bool success = DoCommitPipelined(commit_req.submission(), record_time,
                                   commit_req.participant_id(), correlation_id,
                                   *execute_commit_span, read_set, updates);

  if (success) {
    auto record_transaction_start = std::chrono::steady_clock::now();
    RecordTransaction(updates, current_block_id, correlation_id,
                      *execute_commit_span, concord_response);
    auto end = std::chrono::steady_clock::now();
    auto total_duration =
        std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    auto record_transaction_duration =
        std::chrono::duration_cast<std::chrono::milliseconds>(
            end - record_transaction_start);
    LOG_INFO(
        logger_,
        "Done handling DAML commit command, time: "
            << TimeUtil::ToString(record_time) << ", totalDuration: "
            << total_duration.count() << ", recordTransactionDuration: "
            << record_transaction_duration.count() << ", clock: "
            << std::chrono::steady_clock::now().time_since_epoch().count());
    execution_time_.Increment((double)total_duration.count());
    if (enable_histograms_or_summaries) {
      daml_hdlr_exec_dur_.Observe(total_duration.count());
    }
    return true;
  } else {
    LOG_INFO(logger_, "Failed handling DAML commit command, time: "
                          << TimeUtil::ToString(record_time));
    return false;
  }
}

void PrefixReadSetKeys(com::vmware::concord::ReadSet* read_set) {
  for (int i = 0; i < read_set->keys_with_fingerprints_size(); ++i) {
    auto element = read_set->keys_with_fingerprints(i);
    std::string prefixed_key = PrefixDamlKey(element.key());
    read_set->mutable_keys_with_fingerprints(i)->set_key(
        std::move(prefixed_key));
  }
}

bool DamlKvbCommandsHandler::PreExecute(
    const da_kvbc::CommitRequest& commit_request,
    const opentracing::Span& parent_span, ConcordResponse& concord_response) {
  // Callback for reading from storage.
  KeyValueWithFingerprintReaderFunc storage_reader = [&](const auto& keys) {
    return ReadKeys(keys);
  };

  auto start = std::chrono::steady_clock::now();

  auto pre_execution_result = concord_response.mutable_pre_execution_result();
  grpc::Status status = validator_client_->PreExecute(
      commit_request.submission(), commit_request.participant_id(),
      commit_request.correlation_id(), parent_span, storage_reader,
      pre_execution_result);

  auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                      std::chrono::steady_clock::now() - start)
                      .count();
  if (enable_histograms_or_summaries) {
    daml_exec_eng_dur_.Observe(duration);
  }
  LOG_INFO(logger_, "DAML external PreExecute duration [" << duration << "ms]");
  if (!status.ok()) {
    LOG_ERROR(logger_, "Pre-execution failed " << status.error_code() << ": "
                                               << status.error_message());
    pre_execution_result->set_request_correlation_id(
        commit_request.correlation_id());
    return false;
  } else {
    PrefixReadSetKeys(pre_execution_result->mutable_read_set());
    if (logger_.getChainedLogLevel() <= log4cplus::DEBUG_LOG_LEVEL) {
      std::string output_bytes = pre_execution_result->output();
      LOG_DEBUG(logger_, "Hash of pre-execution output ["
                             << Hash(SHA3_256().digest(output_bytes.c_str(),
                                                       output_bytes.size()))
                                    .toString()
                             << "]");
    }
    return true;
  }
}

std::map<std::string, ValueFingerprintPair> DamlKvbCommandsHandler::ReadKeys(
    const google::protobuf::RepeatedPtrField<string>& keys) {
  auto values = GetFromStorage(keys);
  std::map<std::string, ValueFingerprintPair> result;
  for (auto& entry : values) {
    result[entry.first] =
        std::make_pair(std::move(entry.second.first),
                       SerializeFingerprint(entry.second.second));
  }
  return result;
}

bool DamlKvbCommandsHandler::PostExecute(
    const com::vmware::concord::PreExecutionResult& pre_execution_result,
    TimeContract* time, const opentracing::Span& parent_span,
    ConcordResponse& concord_response) {
  std::string span_name = "daml_post_execute";
  auto post_execute_span = parent_span.tracer().StartSpan(
      span_name, {opentracing::ChildOf(&parent_span.context())});
  google::protobuf::Timestamp record_time = RecordTimeForTimeContract(time);
  const string correlation_id = pre_execution_result.request_correlation_id();

  LOG_INFO(logger_,
           "Handle DAML post execution, time: "
               << TimeUtil::ToString(record_time) << ", clock: "
               << std::chrono::steady_clock::now().time_since_epoch().count());

  da_kvbc::PreExecutionOutput pre_execution_output;
  if (!pre_execution_output.ParseFromString(pre_execution_result.output())) {
    LOG_ERROR(logger_, "Failed parsing pre-execution output");
    return false;
  } else {
    SetOfKeyValuePairs raw_write_set;
    GenerateWriteSetForPreExecution(pre_execution_output, record_time,
                                    raw_write_set);
    if (enable_histograms_or_summaries) {
      pre_execution_write_kv_set_size_summary_.Observe(raw_write_set.size());
      for (auto& kv : raw_write_set) {
        pre_execution_write_keys_size_summary_.Observe(kv.first.length());
        pre_execution_write_values_size_summary_.Observe(kv.second.length());
      }
    }
    auto start = std::chrono::steady_clock::now();
    RecordTransaction(raw_write_set, storage_.getLastBlock(), correlation_id,
                      parent_span, concord_response);
    auto record_transaction_duration =
        std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - start);
    LOG_INFO(
        logger_,
        "Done handling DAML post execution, time: "
            << TimeUtil::ToString(record_time)
            << ", recordTransactionDuration: "
            << record_transaction_duration.count() << ", clock: "
            << std::chrono::steady_clock::now().time_since_epoch().count());
    return true;
  }
}

bool CheckIfWithinTimeBounds(
    const com::digitalasset::kvbc::PreExecutionOutput& pre_execution_output,
    const google::protobuf::Timestamp& record_time) {
  int64_t record_time_micros = TimeUtil::TimestampToMicroseconds(record_time);
  if (pre_execution_output.has_min_record_time()) {
    int64_t min_record_time_micros = TimeUtil::TimestampToMicroseconds(
        pre_execution_output.min_record_time());
    if (min_record_time_micros > record_time_micros) {
      return false;
    }
  }
  if (pre_execution_output.has_max_record_time()) {
    int64_t max_record_time_micros = TimeUtil::TimestampToMicroseconds(
        pre_execution_output.max_record_time());
    if (record_time_micros > max_record_time_micros) {
      return false;
    }
  }
  return true;
}

void AddTimeUpdate(const google::protobuf::Timestamp& record_time,
                   const std::vector<std::string>& thin_replica_ids,
                   SetOfKeyValuePairs& write_set) {
  auto key = CreateDamlKvbKey(DamlKvbCommandsHandler::kTimeUpdateKey);
  da_kvbc::TimeUpdateLogEntry time_update_log_entry;
  time_update_log_entry.mutable_record_time()->CopyFrom(record_time);
  auto value = CreateDamlKvbValue(time_update_log_entry.SerializeAsString(),
                                  thin_replica_ids);
  write_set[key] = value;
}

bool DamlKvbCommandsHandler::GenerateWriteSetForPreExecution(
    const com::digitalasset::kvbc::PreExecutionOutput& pre_execution_output,
    const google::protobuf::Timestamp& record_time,
    SetOfKeyValuePairs& write_set) const {
  bool within_time_bounds =
      CheckIfWithinTimeBounds(pre_execution_output, record_time);
  if (within_time_bounds) {
    LOG_DEBUG(logger_,
              "Committing pre-execution success write set of size "
                  << pre_execution_output.success_write_set().writes().size());
    WriteSetToRawUpdates(pre_execution_output.success_write_set(), write_set);
    auto thin_replica_ids = std::vector<std::string>(
        pre_execution_output.informee_success_thin_replica_ids().begin(),
        pre_execution_output.informee_success_thin_replica_ids().end());
    AddTimeUpdate(record_time, thin_replica_ids, write_set);
    post_execution_success_.Increment();
  } else {
    LOG_DEBUG(logger_,
              "Committing pre-execution out-of-time-bounds write set of size "
                  << pre_execution_output.out_of_time_bounds_write_set()
                         .writes()
                         .size());
    WriteSetToRawUpdates(pre_execution_output.out_of_time_bounds_write_set(),
                         write_set);
    AddTimeUpdate(record_time,
                  {pre_execution_output.submitting_participant_id()},
                  write_set);
    post_execution_timeout_.Increment();
  }
  return true;
}

void DamlKvbCommandsHandler::WriteSetToRawUpdates(
    const com::digitalasset::kvbc::WriteSet& input_write_set,
    SetOfKeyValuePairs& updates) const {
  for (const auto& entry : input_write_set.writes()) {
    auto key = CreateDamlKvbKey(entry.key());
    std::vector<string> thin_replica_ids;
    AccessControlListToThinReplicaIds(entry.access(), thin_replica_ids);
    auto value = CreateDamlKvbValue(entry.value(), thin_replica_ids);
    if (enable_histograms_or_summaries) {
      daml_kv_size_summary_.Observe(value.length());
    }
    updates.insert(std::make_pair(std::move(key), std::move(value)));
  }
}

void HashAndLogWriteSet(
    const std::vector<KeyValuePairWithThinReplicaIds>& write_set,
    const SetOfKeyValuePairs& updates, logging::Logger& logger) {
  if (logger.getChainedLogLevel() <= log4cplus::DEBUG_LOG_LEVEL) {
    std::string input_bytes;
    input_bytes.reserve(1000);
    for (const auto& entry : write_set) {
      input_bytes += entry.first;
      input_bytes += entry.second.value();
    }

    std::string stored_bytes;
    stored_bytes.reserve(1000);
    for (const auto& entry : updates) {
      stored_bytes += entry.first.toString();
      stored_bytes += entry.second.toString();
    }

    LOG_DEBUG(logger, "Hash of raw input ["
                          << Hash(SHA3_256().digest(input_bytes.c_str(),
                                                    input_bytes.size()))
                                 .toString()
                          << "]");
    LOG_DEBUG(logger, "Hash of serialized input ["
                          << Hash(SHA3_256().digest(stored_bytes.c_str(),
                                                    stored_bytes.size()))
                                 .toString()
                          << "]");
  }
}

bool DamlKvbCommandsHandler::DoCommitPipelined(
    const std::string& submission,
    const google::protobuf::Timestamp& record_time,
    const std::string& participant_id, const std::string& correlation_id,
    const opentracing::Span& parent_span, std::vector<std::string>& read_set,
    SetOfKeyValuePairs& updates) {
  // Callback for reading from storage.
  DamlKvbReadFunc kvb_read = [&](const auto& keys) {
    auto values = GetFromStorage(keys);
    std::map<std::string, std::string> adapted_result;
    for (auto& entry : values) {
      adapted_result[entry.first] = std::move(entry.second.first);
    }
    return adapted_result;
  };

  auto start = std::chrono::steady_clock::now();

  std::vector<KeyValuePairWithThinReplicaIds> write_set;
  grpc::Status status = validator_client_->Validate(
      submission, record_time, participant_id, correlation_id, parent_span,
      kvb_read, &read_set, &write_set);

  auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                      std::chrono::steady_clock::now() - start)
                      .count();
  if (enable_histograms_or_summaries) {
    daml_exec_eng_dur_.Observe(duration);
  }
  LOG_INFO(logger_, "DAML external Validate (Pipelined) duration [" << duration
                                                                    << "ms]");

  // Wrap key/value pairs appropriately for storage.
  for (const auto& entry : write_set) {
    auto key = CreateDamlKvbKey(entry.first);
    auto value = CreateDamlKvbValue(entry.second);
    if (enable_histograms_or_summaries) {
      daml_kv_size_summary_.Observe(value.length());
    }
    LOG_DEBUG(logger_, "KV_metrics Key length: " << key.length()
                                                 << ", Value length: "
                                                 << value.length());
    updates.insert(std::make_pair(std::move(key), std::move(value)));
  }
  HashAndLogWriteSet(write_set, updates, determinism_logger_);

  if (!status.ok()) {
    LOG_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                            << status.error_message());
    return false;
  } else {
    return true;
  }
}

void DamlKvbCommandsHandler::RecordTransaction(
    const SetOfKeyValuePairs& updates, const BlockId current_block_id,
    const string& correlation_id, const opentracing::Span& parent_span,
    ConcordResponse& concord_response) {
  auto record_transaction = concordUtils::startChildSpanFromContext(
      parent_span.context(), "record_transaction");
  SetOfKeyValuePairs amended_updates(updates);
  auto copied_correlation_id = correlation_id;
  auto cid_val = kvbc::Value(std::move(copied_correlation_id));
  if (enable_histograms_or_summaries) {
    internal_kv_size_summary_.Observe(cid_val.length());
    written_keys_count_.Observe(updates.size());
  }
  amended_updates.insert({cid_key_, std::move(cid_val)});
  BlockId new_block_id = 0;
  auto status = addBlock(amended_updates, new_block_id, record_transaction);
  assert(status.isOK());
  assert(new_block_id == current_block_id + 1);

  da_kvbc::CommandReply command_reply;
  da_kvbc::CommitResponse* commit_response = command_reply.mutable_commit();
  commit_response->set_status(da_kvbc::CommitResponse_CommitStatus_OK);
  commit_response->set_block_id(new_block_id);
  string cmd_string;
  command_reply.SerializeToString(&cmd_string);
  DamlResponse* daml_response = concord_response.mutable_daml_response();
  daml_response->set_command_reply(cmd_string.c_str(), cmd_string.size());
  write_ops_.Increment();
}

bool DamlKvbCommandsHandler::ExecuteCommand(
    const ConcordRequest& concord_req, const uint8_t flags,
    TimeContract* time_contract, const opentracing::Span& parent_span,
    ConcordResponse& response) {
  if (!concord_req.has_daml_request() &&
      !concord_req.has_pre_execution_result()) {
    // we have to ignore this, to allow time-only updates
    return true;
  }

  bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
  if (has_pre_executed && concord_req.has_pre_execution_result()) {
    auto pre_execution_result = concord_req.pre_execution_result();
    const std::string correlation_id =
        pre_execution_result.request_correlation_id();
    SCOPED_MDC_CID(correlation_id);

    if (!pre_execution_result.has_read_set()) {
      LOG_WARN(logger_, "Post-execution failed due to missing read set.");
      return false;
    } else if (HasPreExecutionConflicts(pre_execution_result.read_set())) {
      LOG_INFO(logger_, "Post-execution failed due to conflicts.");
      return false;
    } else {
      return PostExecute(pre_execution_result, time_contract, parent_span,
                         response);
    }
  } else {
    auto daml_req = concord_req.daml_request();
    if (!daml_req.has_command()) {
      LOG_ERROR(logger_, "No Command specified in DAML request");
      return false;
    }

    da_kvbc::Command cmd;
    if (!cmd.ParseFromString(daml_req.command())) {
      LOG_ERROR(logger_, "Failed to parse DAML/Command request");
      return false;
    }

    switch (cmd.cmd_case()) {
      case da_kvbc::Command::kRead:
        return ExecuteRead(cmd.read(), response);

      case da_kvbc::Command::kCommit: {
        const auto& commit_req = cmd.commit();
        SCOPED_MDC_CID(commit_req.correlation_id());
        bool result = ExecuteCommit(commit_req, flags, time_contract,
                                    parent_span, response);
        return result;
      }

      default:
        LOG_ERROR(logger_, "Neither commit nor read command");
        return false;
    }
  }
}

bool DamlKvbCommandsHandler::ExecuteReadOnlyCommand(
    const ConcordRequest& concord_req, ConcordResponse& response) {
  if (!concord_req.has_daml_request()) {
    LOG_ERROR(logger_, "No legit Concord / DAML request");
    return false;
  }

  auto daml_req = concord_req.daml_request();
  if (!daml_req.has_command()) {
    LOG_ERROR(logger_, "No Command specified in DAML request");
    return false;
  }

  da_kvbc::Command cmd;
  if (!cmd.ParseFromString(daml_req.command())) {
    LOG_ERROR(logger_, "Failed to parse DAML/Command request");
    return false;
  }

  switch (cmd.cmd_case()) {
    case da_kvbc::Command::kRead:
      return ExecuteRead(cmd.read(), response);
    case da_kvbc::Command::kCommit:
      LOG_ERROR(logger_, "ExecuteReadOnlyCommand got write command!");
      return false;
    default:
      return false;
  }
}

bool DamlKvbCommandsHandler::Execute(
    const ConcordRequest& request,
    const concord::consensus::ConcordRequestContext& request_context,
    const uint8_t flags, TimeContract* time_contract,
    opentracing::Span& parent_span, ConcordResponse& response) {
  bool read_only = flags & bftEngine::MsgFlag::READ_ONLY_FLAG;
  if (read_only) {
    return ExecuteReadOnlyCommand(request, response);
  } else {
    auto success =
        ExecuteCommand(request, flags, time_contract, parent_span, response);
    if (!success) {
      failed_ops_.Increment();
    }
    return success;
  }
}

void DamlKvbCommandsHandler::WriteEmptyBlock(
    TimeContract* time_contract, const opentracing::Span& parent_span) {
  auto span = concordUtils::startChildSpanFromContext(parent_span.context(),
                                                      "write_empty_block");
  BlockId currentBlockId = storage_.getLastBlock();
  SetOfKeyValuePairs empty_updates;
  BlockId newBlockId = 0;
  auto status = addBlock(empty_updates, newBlockId, span);
  assert(status.isOK());
  assert(newBlockId == currentBlockId + 1);
}

google::protobuf::Timestamp DamlKvbCommandsHandler::RecordTimeForTimeContract(
    TimeContract* time_contract) {
  google::protobuf::Timestamp record_time;
  if (time_contract) {
    record_time = time_contract->GetTime();
    LOG_DEBUG(logger_, "Using time service time " << record_time);
  } else {
    record_time = google::protobuf::util::TimeUtil::GetEpoch();
    LOG_DEBUG(logger_, "Using epoch time " << record_time);
  }
  return record_time;
}

}  // namespace daml
}  // namespace concord
