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
#include "sha3_256.h"
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

Sliver CreateDamlKvbKey(const string& content) {
  // TODO: This should be hidden in a separate DAML storage implementation
  string full_key(content);
  full_key.insert(0, &concord::storage::kKvbKeyDaml,
                  sizeof(concord::storage::kKvbKeyDaml));
  return CreateSliver(full_key.c_str(), full_key.size());
}

Sliver CreateDamlKvbValue(const string& value, vector<string> trid_list) {
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

bool DamlKvbCommandsHandler::ExecuteRead(const da_kvbc::ReadCommand& request,
                                         ConcordResponse& response) {
  read_ops_.Increment();
  LOG_WARN(logger_, "NOT IMPLEMENTED");
  return false;
}

std::map<string, string> DamlKvbCommandsHandler::GetFromStorage(
    const google::protobuf::RepeatedPtrField<string>& keys) {
  std::map<string, string> result;
  for (const auto& skey : keys) {
    Value out;
    Key key = CreateDamlKvbKey(skey);
    if (!storage_.get(key, out).isOK()) {
      std::stringstream msg;
      msg << "Couldn't find key " << skey;
      throw std::runtime_error(msg.str());
    }

    if (out.length() == 0) {
      // TODO: Why do we have a DAML key w/ an empty value in the KVB?
      result[skey] = string();
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
    result[skey] = proto.value();
  }
  return result;
}

bool DamlKvbCommandsHandler::ExecuteCommit(
    const da_kvbc::CommitRequest& commit_req, const uint8_t flags,
    TimeContract* time, opentracing::Span& parent_span,
    ConcordResponse& concord_response) {
  auto start = std::chrono::steady_clock::now();
  std::string span_name = "daml_execute_commit_pipelined";
  auto execute_commit_span = parent_span.tracer().StartSpan(
      span_name, {opentracing::ChildOf(&parent_span.context())});
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
  std::string correlation_id = commit_req.correlation_id();
  BlockId current_block_id = storage_.getLastBlock();
  google::protobuf::Timestamp record_time = RecordTimeForTimeContract(time);

  LOG_INFO(logger_,
           "Handle DAML commit command, cid: "
               << correlation_id
               << ", time: " << TimeUtil::ToString(record_time) << ", clock: "
               << std::chrono::steady_clock::now().time_since_epoch().count());

  if (has_pre_executed && request_.has_pre_execution_result()) {
    return CommitPreExecutionResult(current_block_id, record_time,
                                    correlation_id, *execute_commit_span,
                                    concord_response);
  }

  SetOfKeyValuePairs updates;
  SetOfKeyValuePairs updates_on_timeout;
  SetOfKeyValuePairs updates_on_conflict;

  // The set of keys used during validation. Includes reads of keys
  // cached by the validator.
  vector<string> read_set;
  bool success = DoCommitPipelined(
      commit_req.submission(), record_time, commit_req.participant_id(),
      correlation_id, *execute_commit_span, read_set, updates, pre_execute);

  if (success) {
    if (pre_execute) {
      // Maximum record time to use when calling pre-execution
      std::optional<google::protobuf::Timestamp> max_record_time = std::nullopt;
      BuildPreExecutionResult(updates, updates_on_timeout, updates_on_conflict,
                              current_block_id, max_record_time, correlation_id,
                              read_set, *execute_commit_span, concord_response);
      LOG_DEBUG(logger_, "Done: Pre-execution of DAML command.");
    } else {
      auto start1 = std::chrono::steady_clock::now();
      RecordTransaction(updates, current_block_id, correlation_id,
                        *execute_commit_span, concord_response);
      auto end = std::chrono::steady_clock::now();
      auto dur =
          std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
      auto recordDur =
          std::chrono::duration_cast<std::chrono::milliseconds>(end - start1);
      LOG_INFO(
          logger_,
          "Done handle DAML commit command, time: "
              << TimeUtil::ToString(record_time) << ", execDur: " << dur.count()
              << ", recordTransactionDur: " << recordDur.count() << ", clock: "
              << std::chrono::steady_clock::now().time_since_epoch().count());
      execution_time_.Increment((double)dur.count());
      daml_hdlr_exec_dur_.Observe(dur.count());
    }
    return true;
  } else {
    LOG_INFO(logger_, "Failed handle DAML commit command, cid: "
                          << correlation_id
                          << ", time: " << TimeUtil::ToString(record_time));
    return false;
  }
}

bool DamlKvbCommandsHandler::DoCommitPipelined(
    const std::string& submission,
    const google::protobuf::Timestamp& record_time,
    const std::string& participant_id, const std::string& correlation_id,
    opentracing::Span& parent_span, std::vector<std::string>& read_set,
    SetOfKeyValuePairs& updates, bool isPreExecution) {
  // Callback for reading from storage.
  DamlKvbReadFunc kvb_read = [&](const auto& keys) {
    return GetFromStorage(keys);
  };
  std::vector<KeyValuePairWithThinReplicaIds> write_set;

  auto start = std::chrono::steady_clock::now();

  grpc::Status status = validator_client_->Validate(
      submission, record_time, participant_id, correlation_id, parent_span,
      kvb_read, &read_set, &write_set);

  auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                      std::chrono::steady_clock::now() - start)
                      .count();
  daml_exec_eng_dur_.Observe(duration);

  LOG_INFO(logger_, "DAML external Validate (Pipelined) duration [" << duration
                                                                    << "ms]");

  // Hash and log
  std::string raw;
  raw.reserve(1000);
  std::string ser;
  ser.reserve(1000);

  // Wrap key/value pairs appropriately for storage.
  for (const auto& entry : write_set) {
    raw += entry.first;
    raw += entry.second.value();

    auto key = CreateDamlKvbKey(entry.first);
    auto val = CreateDamlKvbValue(entry.second);

    ser += key.toString();
    ser += val.toString();

    if (!isPreExecution) daml_kv_size_summary_.Observe(val.length());
    updates.insert(std::make_pair(std::move(key), std::move(val)));
  }

  LOG_DEBUG(dtrmnsm_logger_,
            "Hash of raw input ["
                << Hash(SHA3_256().digest(raw.c_str(), raw.size())).toString()
                << "]");

  LOG_DEBUG(dtrmnsm_logger_,
            "Hash serialized input ["
                << Hash(SHA3_256().digest(ser.c_str(), ser.size())).toString()
                << "]");

  if (!status.ok()) {
    LOG_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                            << status.error_message());
    return false;
  } else {
    return true;
  }
}

void DamlKvbCommandsHandler::BuildPreExecutionResult(
    const SetOfKeyValuePairs& updates,
    const SetOfKeyValuePairs& updates_on_timeout,
    const SetOfKeyValuePairs& updates_on_conflict, BlockId current_block_id,
    const std::optional<google::protobuf::Timestamp>& max_record_time,
    const string& correlation_id, const vector<string>& validation_read_set,
    opentracing::Span& parent_span, ConcordResponse& concord_response) const {
  auto build_pre_execution_result = opentracing::Tracer::Global()->StartSpan(
      "build_pre_execution_result",
      {opentracing::ChildOf(&parent_span.context())});
  auto* pre_execution_result = concord_response.mutable_pre_execution_result();

  if (max_record_time) {
    pre_execution_result->mutable_max_record_time()->CopyFrom(*max_record_time);
  }

  pre_execution_result->set_read_set_version(current_block_id);

  BuildWriteSetsFromUpdates(updates, updates_on_timeout, updates_on_conflict,
                            pre_execution_result);

  auto* read_set = pre_execution_result->mutable_read_set();
  for (const auto& k : validation_read_set) {
    const auto& key = CreateDamlKvbKey(k);
    read_set->add_keys(key.data(), key.length());
  }

  pre_execution_result->set_request_correlation_id(correlation_id.c_str(),
                                                   correlation_id.size());
}

void DamlKvbCommandsHandler::BuildWriteSetsFromUpdates(
    const SetOfKeyValuePairs& updates,
    const SetOfKeyValuePairs& timeout_updates,
    const SetOfKeyValuePairs& conflict_updates,
    com::vmware::concord::PreExecutionResult* result) const {
  for (const auto& kv : updates) {
    auto* new_kv = result->mutable_write_set()->add_kv_writes();
    new_kv->set_key(kv.first.data(), kv.first.length());
    new_kv->set_value(kv.second.data(), kv.second.length());
  }

  for (const auto& kv : timeout_updates) {
    auto* new_kv = result->mutable_timeout_write_set()->add_kv_writes();
    new_kv->set_key(kv.first.data(), kv.first.length());
    new_kv->set_value(kv.second.data(), kv.second.length());
  }

  for (const auto& kv : conflict_updates) {
    auto* new_kv = result->mutable_conflict_write_set()->add_kv_writes();
    new_kv->set_key(kv.first.data(), kv.first.length());
    new_kv->set_value(kv.second.data(), kv.second.length());
  }
}

bool DamlKvbCommandsHandler::CommitPreExecutionResult(
    BlockId current_block_id, google::protobuf::Timestamp& record_time,
    string& correlation_id, opentracing::Span& parent_span,
    ConcordResponse& concord_response) {
  auto commit_pre_execution_result_span =
      opentracing::Tracer::Global()->StartSpan(
          "commit_pre_execution_result_span",
          {opentracing::ChildOf(&parent_span.context())});
  bool commit_pre_execution_result = false;
  SetOfKeyValuePairs updates;
  auto* pre_execution_result = request_.mutable_pre_execution_result();
  auto max_record_time = GetPreExecutionMaxRecordTime();
  correlation_id = pre_execution_result->request_correlation_id();

  com::vmware::concord::WriteSet* write_set = nullptr;
  if (max_record_time.has_value() && record_time > max_record_time.value()) {
    LOG_DEBUG(logger_, "Failed to commit pre-executed command "
                           << correlation_id << " due to timeout.");
    write_set = pre_execution_result->mutable_timeout_write_set();
  } else if (HasPreExecutionConflicts(request_.pre_execution_result())) {
    LOG_DEBUG(logger_, "Failed to commit pre-executed command "
                           << correlation_id << " due to conflicts.");
    write_set = pre_execution_result->mutable_conflict_write_set();
  } else {
    LOG_DEBUG(logger_, "Recording successfully pre-executed DAML command "
                           << correlation_id << ".");
    write_set = pre_execution_result->mutable_write_set();
    commit_pre_execution_result = true;
  }

  auto& kv_writes = *write_set->mutable_kv_writes();
  for (auto& kv : kv_writes) {
    auto key =
        Sliver{std::move(*std::unique_ptr<std::string>{kv.release_key()})};
    auto val =
        Sliver{std::move(*std::unique_ptr<std::string>{kv.release_value()})};
    updates.insert(kvbc::KeyValuePair(key, val));
    daml_kv_size_summary_.Observe(val.length());
  }

  RecordTransaction(updates, current_block_id, correlation_id,
                    *commit_pre_execution_result_span, concord_response);
  return commit_pre_execution_result;
}

void DamlKvbCommandsHandler::RecordTransaction(
    const SetOfKeyValuePairs& updates, BlockId current_block_id,
    const string& correlation_id, opentracing::Span& parent_span,
    ConcordResponse& concord_response) {
  auto record_transaction = opentracing::Tracer::Global()->StartSpan(
      "record_transaction", {opentracing::ChildOf(&parent_span.context())});
  // Commit the block, if there were no conflicts.
  DamlResponse* daml_response = concord_response.mutable_daml_response();

  da_kvbc::CommandReply command_reply;
  da_kvbc::CommitResponse* commit_response = command_reply.mutable_commit();
  auto cid = correlation_id;
  SetOfKeyValuePairs amended_updates(updates);
  auto cid_val = kvbc::Value(std::move(cid));
  internal_kv_size_summary_.Observe(cid_val.length());
  amended_updates.insert({cid_key_, std::move(cid_val)});
  BlockId new_block_id = 0;
  concordUtils::Status res = addBlock(amended_updates, new_block_id);
  assert(res.isOK());
  assert(new_block_id == current_block_id + 1);
  commit_response->set_status(da_kvbc::CommitResponse_CommitStatus_OK);
  commit_response->set_block_id(new_block_id);

  string cmd_string;
  command_reply.SerializeToString(&cmd_string);
  daml_response->set_command_reply(cmd_string.c_str(), cmd_string.size());
  write_ops_.Increment();
}

std::optional<google::protobuf::Timestamp>
DamlKvbCommandsHandler::GetPreExecutionMaxRecordTime() {
  const auto& pre_execution_result = request_.pre_execution_result();
  if (pre_execution_result.has_max_record_time()) {
    return std::optional<google::protobuf::Timestamp>(
        pre_execution_result.max_record_time());
  } else {
    return std::nullopt;
  }
}

bool DamlKvbCommandsHandler::ExecuteCommand(const ConcordRequest& concord_req,
                                            const uint8_t flags,
                                            TimeContract* time_contract,
                                            opentracing::Span& parent_span,
                                            ConcordResponse& response) {
  DamlRequest daml_req;

  if (!concord_req.has_daml_request() &&
      !concord_req.has_pre_execution_result()) {
    // we have to ignore this, to allow time-only updates
    return true;
  }

  if (concord_req.has_pre_execution_result()) {
    da_kvbc::CommitRequest commit_req;
    const std::string correlation_id =
        concord_req.pre_execution_result().request_correlation_id();
    commit_req.set_correlation_id(correlation_id);
    SCOPED_MDC_CID(correlation_id);
    return ExecuteCommit(commit_req, flags, time_contract, parent_span,
                         response);
  } else {
    daml_req = concord_req.daml_request();
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
  DamlRequest daml_req;

  if (!concord_req.has_daml_request()) {
    LOG_ERROR(logger_, "No legit Concord / DAML request");
    return false;
  }

  daml_req = concord_req.daml_request();
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

bool DamlKvbCommandsHandler::Execute(const ConcordRequest& request,
                                     const uint8_t flags,
                                     TimeContract* time_contract,
                                     opentracing::Span& parent_span,
                                     ConcordResponse& response) {
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

void DamlKvbCommandsHandler::WriteEmptyBlock(TimeContract* time_contract) {
  BlockId currentBlockId = storage_.getLastBlock();
  SetOfKeyValuePairs empty_updates;
  BlockId newBlockId = 0;
  auto status = addBlock(empty_updates, newBlockId);
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
