// Copyright 2019 VMware, all rights reserved

#include "daml_kvb_commands_handler.hpp"

#include <google/protobuf/util/time_util.h>
#include <log4cplus/loggingmacros.h>
#include <opentracing/tracer.h>
#include <map>
#include <string>
#include "utils/concord_logging.hpp"

#include "concord_storage.pb.h"
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
  proto.SerializeWithCachedSizesToArray(reinterpret_cast<unsigned char*>(data));

  return Sliver(data, size);
}

bool DamlKvbCommandsHandler::ExecuteRead(const da_kvbc::ReadCommand& request,
                                         ConcordResponse& response) {
  read_ops_.Increment();
  LOG4CPLUS_WARN(logger_, "NOT IMPLEMENTED");
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
  std::string span_name = enable_pipelined_commits_
                              ? "daml_execute_commit_pipelined"
                              : "daml_execute_commit";
  auto execute_commit_span = parent_span.tracer().StartSpan(
      span_name, {opentracing::ChildOf(&parent_span.context())});
  LOG4CPLUS_DEBUG(logger_, "Handle DAML commit command");
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
  std::string correlation_id = commit_req.correlation_id();
  BlockId current_block_id = storage_.getLastBlock();
  google::protobuf::Timestamp record_time = RecordTimeForTimeContract(time);
  if (has_pre_executed && request_.has_pre_execution_result()) {
    return CommitPreExecutionResult(current_block_id, record_time,
                                    correlation_id, *execute_commit_span,
                                    concord_response);
  }

  SetOfKeyValuePairs updates;
  // The set of keys used during validation. Includes reads of keys
  // cached by the validator.
  vector<string> read_set;
  // Maximum record time to use when calling pre-execution.
  std::optional<google::protobuf::Timestamp> max_record_time;
  bool success = false;
  if (enable_pipelined_commits_) {
    success = DoCommitPipelined(commit_req.submission(), record_time,
                                commit_req.participant_id(), correlation_id,
                                *execute_commit_span, read_set, updates);
  } else {
    success = DoCommitSingle(commit_req, current_block_id, record_time,
                             *execute_commit_span, &max_record_time, read_set,
                             updates);
  }

  if (success) {
    if (pre_execute) {
      BuildPreExecutionResult(updates, current_block_id, max_record_time,
                              correlation_id, read_set, *execute_commit_span,
                              concord_response);
      LOG4CPLUS_DEBUG(logger_, "Done: Pre-execution of DAML command.");
    } else {
      RecordTransaction(updates, current_block_id, correlation_id,
                        *execute_commit_span, concord_response);
      LOG4CPLUS_DEBUG(logger_, "Done: Handle DAML commit command.");
    }
    return true;
  } else {
    LOG4CPLUS_DEBUG(logger_, "DAML commit execution has failed.");
    return false;
  }
}

bool DamlKvbCommandsHandler::DoCommitSingle(
    const da_kvbc::CommitRequest& commit_request,
    const BlockId current_block_id,
    const google::protobuf::Timestamp& record_time,
    opentracing::Span& parent_span,
    std::optional<google::protobuf::Timestamp>* max_record_time,
    std::vector<std::string>& read_set, SetOfKeyValuePairs& updates) {
  // Since we're not batching, lets keep it simple and use the next block id as
  // the DAML log entry id.
  string entry_id = std::to_string(current_block_id + 1);
  da_kvbc::ValidateResponse response;
  da_kvbc::ValidatePendingSubmissionResponse response2;
  if (RunDamlExecution(commit_request, entry_id, record_time, parent_span,
                       response, response2)) {
    const da_kvbc::Result& result =
        response.has_result() ? response.result() : response2.result();
    *max_record_time = result.has_max_record_time()
                           ? std::make_optional(result.max_record_time())
                           : std::nullopt;
    std::copy(result.read_set().begin(), result.read_set().end(),
              std::back_inserter(read_set));
    GetUpdatesFromExecutionResult(result, entry_id, updates);
    return true;
  } else {
    return false;
  }
}

bool DamlKvbCommandsHandler::DoCommitPipelined(
    const std::string& submission,
    const google::protobuf::Timestamp& record_time,
    const std::string& participant_id, const std::string& correlation_id,
    opentracing::Span& parent_span, std::vector<std::string>& read_set,
    SetOfKeyValuePairs& updates) {
  // Callback for reading from storage.
  DamlKvbReadFunc kvb_read = [&](const auto& keys) {
    return GetFromStorage(keys);
  };
  WriteCollectingStorageOperations storage_operations(kvb_read);
  grpc::Status status = validator_client_->Validate(
      submission, record_time, participant_id, correlation_id, parent_span,
      read_set, storage_operations);
  // Wrap key/value pairs appropriately for storage.
  for (const auto& entry : storage_operations.get_updates()) {
    updates.insert(std::make_pair(
        CreateDamlKvbKey(entry.first),
        CreateDamlKvbValue(entry.second.value, entry.second.thin_replica_ids)));
  }
  if (!status.ok()) {
    LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                                  << status.error_message());
    return false;
  } else {
    return true;
  }
}

bool DamlKvbCommandsHandler::RunDamlExecution(
    const da_kvbc::CommitRequest& commit_req, const string& entryId,
    const google::protobuf::Timestamp& record_time,
    opentracing::Span& parent_span, da_kvbc::ValidateResponse& response,
    da_kvbc::ValidatePendingSubmissionResponse& response2) {
  auto run_daml_execution = parent_span.tracer().StartSpan(
      "run_daml_execution", {opentracing::ChildOf(&parent_span.context())});
  bool daml_execution_success = true;
  // Send the submission for validation.
  grpc::Status status = validator_client_->ValidateSubmission(
      entryId, commit_req.submission(), record_time,
      commit_req.participant_id(), commit_req.correlation_id(),
      *run_daml_execution, &response);
  if (!status.ok()) {
    LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                                  << status.error_message());
    daml_execution_success = false;
  } else {
    if (response.has_need_state()) {
      LOG4CPLUS_DEBUG(logger_, "Validator requested input state");
      // The submission failed due to missing input state, retrieve the inputs
      // and retry.
      std::map<string, string> input_state_entries;
      try {
        auto get_from_storage = run_daml_execution->tracer().StartSpan(
            "get_from_storage",
            {opentracing::ChildOf(&run_daml_execution->context())});
        input_state_entries = GetFromStorage(response.need_state().keys());
      } catch (const std::exception& e) {
        LOG4CPLUS_ERROR(logger_, e.what());
        daml_execution_success = false;
      }

      if (daml_execution_success) {
        validator_client_->ValidatePendingSubmission(
            entryId, input_state_entries, commit_req.correlation_id(),
            *run_daml_execution, &response2);
        if (!status.ok()) {
          LOG4CPLUS_ERROR(logger_, "Validation failed "
                                       << status.error_code() << ": "
                                       << status.error_message());
          daml_execution_success = false;
        } else {
          if (!response2.has_result()) {
            daml_execution_success = false;
          }
        }
      }
    } else if (!response.has_result()) {
      LOG4CPLUS_ERROR(logger_, "Validation missing result!");
      daml_execution_success = false;
    }
  }

  return daml_execution_success;
}

void DamlKvbCommandsHandler::GetUpdatesFromExecutionResult(
    const da_kvbc::Result& result, const string& entryId,
    SetOfKeyValuePairs& updates) const {
  vector<string> no_trids;

  // Insert the key-value updates into the store.
  for (const auto& kv : result.updates()) {
    const auto& protoTrids = kv.trids();
    vector<string> trids(protoTrids.begin(), protoTrids.end());
    updates.insert(KeyValuePair(CreateDamlKvbKey(kv.key()),
                                CreateDamlKvbValue(kv.value(), trids)));
  }
}

void DamlKvbCommandsHandler::BuildPreExecutionResult(
    const SetOfKeyValuePairs& updates, BlockId current_block_id,
    const std::optional<google::protobuf::Timestamp>& max_record_time,
    string& correlation_id, const vector<string>& validation_read_set,
    opentracing::Span& parent_span, ConcordResponse& concord_response) const {
  auto build_pre_execution_result = opentracing::Tracer::Global()->StartSpan(
      "build_pre_execution_result",
      {opentracing::ChildOf(&parent_span.context())});
  auto* pre_execution_result = concord_response.mutable_pre_execution_result();

  if (max_record_time) {
    pre_execution_result->mutable_max_record_time()->CopyFrom(*max_record_time);
  }

  pre_execution_result->set_read_set_version(current_block_id);

  auto* write_set = pre_execution_result->mutable_write_set();
  for (const auto& kv : updates) {
    auto* new_kv = write_set->add_kv_writes();
    new_kv->set_key(kv.first.data(), kv.first.length());
    new_kv->set_value(kv.second.data(), kv.second.length());
  }

  auto* read_set = pre_execution_result->mutable_read_set();
  for (const auto& k : validation_read_set) {
    const auto& key = CreateDamlKvbKey(k);
    read_set->add_keys(key.data(), key.length());
  }

  const DamlRequest& daml_request = request_.daml_request();
  const auto optional_cid = GetCorrelationId(daml_request);
  if (optional_cid) {
    correlation_id = optional_cid.value();
  } else {
    LOG4CPLUS_ERROR(logger_,
                    "Failed to retrieve the correlation ID from the "
                    "pre-executed DAML commit.");
  }

  pre_execution_result->set_request_correlation_id(correlation_id.c_str(),
                                                   correlation_id.size());
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
  if (max_record_time.has_value() && record_time > max_record_time.value()) {
    // TODO commit a block using the timeout_writeset (future)
    LOG4CPLUS_DEBUG(logger_, "Failed to commit pre-executed command "
                                 << correlation_id << " due to timeout.");
  } else if (HasPreExecutionConflicts(request_.pre_execution_result())) {
    // TODO commit a block using the conflict_writeset (future)
    LOG4CPLUS_DEBUG(logger_, "Failed to commit pre-executed command "
                                 << correlation_id << " due to conflicts.");
  } else {
    auto* write_set = pre_execution_result->mutable_write_set();
    auto& kv_writes = *write_set->mutable_kv_writes();
    for (auto& kv : kv_writes) {
      auto key = std::unique_ptr<std::string>{kv.release_key()};
      auto val = std::unique_ptr<std::string>{kv.release_value()};

      updates.insert(
          kvbc::KeyValuePair(Sliver{std::move(*key)}, Sliver{std::move(*val)}));
    }

    RecordTransaction(updates, current_block_id, correlation_id,
                      *commit_pre_execution_result_span, concord_response);
    LOG4CPLUS_DEBUG(
        logger_,
        "Done: Successfully validated and recorded pre-executed DAML command.");
    commit_pre_execution_result = true;
  }
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
  amended_updates.insert({cid_key_, kvbc::Value(std::move(cid))});
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

std::optional<std::string> DamlKvbCommandsHandler::GetCorrelationId(
    const DamlRequest& daml_request) const {
  da_kvbc::Command cmd;
  if (!cmd.ParseFromString(daml_request.command())) {
    return std::nullopt;
  }
  if (cmd.cmd_case() == da_kvbc::Command::kCommit) {
    const auto& original_commit_req = cmd.commit();
    return std::optional<std::string>(original_commit_req.correlation_id());
  }
  return std::nullopt;
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
    concord::utils::RAIIMDC mdc("cid", correlation_id);
    return ExecuteCommit(commit_req, flags, time_contract, parent_span,
                         response);
  } else {
    daml_req = concord_req.daml_request();
    if (!daml_req.has_command()) {
      LOG4CPLUS_ERROR(logger_, "No Command specified in DAML request");
      return false;
    }

    da_kvbc::Command cmd;
    if (!cmd.ParseFromString(daml_req.command())) {
      LOG4CPLUS_ERROR(logger_, "Failed to parse DAML/Command request");
      return false;
    }

    switch (cmd.cmd_case()) {
      case da_kvbc::Command::kRead:
        return ExecuteRead(cmd.read(), response);

      case da_kvbc::Command::kCommit: {
        const auto& commit_req = cmd.commit();
        concord::utils::RAIIMDC mdc("cid", commit_req.correlation_id());
        bool result = ExecuteCommit(commit_req, flags, time_contract,
                                    parent_span, response);
        return result;
      }

      default:
        LOG4CPLUS_ERROR(logger_, "Neither commit nor read command");
        return false;
    }
  }
}

bool DamlKvbCommandsHandler::ExecuteReadOnlyCommand(
    const ConcordRequest& concord_req, ConcordResponse& response) {
  DamlRequest daml_req;

  if (!concord_req.has_daml_request()) {
    LOG4CPLUS_ERROR(logger_, "No legit Concord / DAML request");
    return false;
  }

  daml_req = concord_req.daml_request();
  if (!daml_req.has_command()) {
    LOG4CPLUS_ERROR(logger_, "No Command specified in DAML request");
    return false;
  }

  da_kvbc::Command cmd;
  if (!cmd.ParseFromString(daml_req.command())) {
    LOG4CPLUS_ERROR(logger_, "Failed to parse DAML/Command request");
    return false;
  }

  switch (cmd.cmd_case()) {
    case da_kvbc::Command::kRead:
      return ExecuteRead(cmd.read(), response);
    case da_kvbc::Command::kCommit:
      LOG4CPLUS_ERROR(logger_, "ExecuteReadOnlyCommand got write command!");
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
    return ExecuteCommand(request, flags, time_contract, parent_span, response);
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
    LOG4CPLUS_DEBUG(logger_, "Using time service time " << record_time);
  } else {
    record_time = google::protobuf::util::TimeUtil::GetEpoch();
    LOG4CPLUS_DEBUG(logger_, "Using epoch time " << record_time);
  }
  return record_time;
}

const char* DamlKvbCommandsHandler::kFeaturePipelinedCommitExecution =
    "FEATURE_daml_pipelined_commits";

bool DamlKvbCommandsHandler::IsPipelinedCommitExecutionEnabled(
    const config::ConcordConfiguration& config) {
  return config.hasValue<bool>(kFeaturePipelinedCommitExecution) &&
         config.getValue<bool>(kFeaturePipelinedCommitExecution);
}

std::map<std::string, std::string> WriteCollectingStorageOperations::Read(
    const google::protobuf::RepeatedPtrField<std::string>& keys) {
  std::map<std::string, std::string> from_updates;
  google::protobuf::RepeatedPtrField<std::string> keys_read_from_storage;
  for (int i = 0; i < keys.size(); ++i) {
    const std::string& requested_key = keys[i];
    auto find_it = updates_.find(requested_key);
    if (find_it != updates_.end()) {
      from_updates[find_it->first] = find_it->second.value;
    } else {
      *keys_read_from_storage.Add() = requested_key;
    }
  }
  if (keys_read_from_storage.size() > 0) {
    std::map<std::string, std::string> from_storage =
        kvb_read_(keys_read_from_storage);
    std::map<std::string, std::string> from_both;
    // Copy entries from the map that has less elements.
    if (from_updates.size() > from_storage.size()) {
      from_updates.swap(from_both);
      from_both.insert(from_storage.begin(), from_storage.end());
    } else {
      from_storage.swap(from_both);
      from_both.insert(from_updates.begin(), from_updates.end());
    }
    return from_both;
  } else {
    return from_updates;
  }
}

void WriteCollectingStorageOperations::Write(
    const std::string& key, const std::string& value,
    const std::vector<std::string>& thin_replica_ids) {
  updates_[key] = ValueWithThinReplicaIds(value, thin_replica_ids);
}

}  // namespace daml
}  // namespace concord
