// Copyright 2019 VMware, all rights reserved

#include "daml_kvb_commands_handler.hpp"

#include <google/protobuf/util/time_util.h>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/mdc.h>
#include <map>
#include <string>

#include "concord_storage.pb.h"
#include "storage/kvb_key_types.h"
#include "time/time_contract.hpp"

using std::map;
using std::string;
using std::vector;

using concord::storage::blockchain::IBlocksAppender;
using concord::storage::blockchain::ILocalKeyValueStorageReadOnly;
using concord::time::TimeContract;
using concordUtils::BlockId;
using concordUtils::Key;
using concordUtils::KeyValuePair;
using concordUtils::SetOfKeyValuePairs;
using concordUtils::Sliver;
using concordUtils::Value;

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
  LOG4CPLUS_DEBUG(logger_, "Handle DAML commit command");
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;

  string prefix = "daml";
  BlockId current_block_id = storage_.getLastBlock();
  SetOfKeyValuePairs updates;

  // Since we're not batching, lets keep it simple and use the next block id as
  // the DAML log entry id.
  string entryId = prefix + "/" + std::to_string(current_block_id + 1);

  google::protobuf::Timestamp record_time;
  if (time) {
    record_time = time->GetTime();
    LOG4CPLUS_DEBUG(logger_, "Using time service time " << record_time);
  } else {
    record_time = google::protobuf::util::TimeUtil::GetEpoch();
    LOG4CPLUS_DEBUG(logger_, "Using epoch time " << record_time);
  }

  std::string correlation_id = commit_req.correlation_id();

  if (has_pre_executed && request_.has_pre_execution_result()) {
    auto* pre_execution_result = request_.mutable_pre_execution_result();
    if (HasPreExecutionConflicts(request_.pre_execution_result())) {
      // TODO in the future, we should create a block based on a
      // write set specific for the case of conflicts
      return false;
    }

    correlation_id = pre_execution_result->request_correlation_id();

    auto* write_set = pre_execution_result->mutable_write_set();
    auto& kv_writes = *write_set->mutable_kv_writes();
    for (auto& kv : kv_writes) {
      auto key = std::unique_ptr<std::string>{kv.release_key()};
      auto val = std::unique_ptr<std::string>{kv.release_value()};

      updates.insert(concordUtils::KeyValuePair(Sliver{std::move(*key)},
                                                Sliver{std::move(*val)}));
    }

    RecordTransaction(entryId, updates, current_block_id, correlation_id,
                      concord_response);
    LOG4CPLUS_DEBUG(
        logger_,
        "Done: Successfully validated and recorded pre-executed DAML command.");
    return true;
  }

  // Send the submission for validation.
  da_kvbc::ValidateResponse response;
  grpc::Status status = validator_client_->ValidateSubmission(
      entryId, commit_req.submission(), record_time,
      commit_req.participant_id(), commit_req.correlation_id(), parent_span,
      &response);
  if (!status.ok()) {
    LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                                  << status.error_message());
    return false;
  }

  da_kvbc::ValidatePendingSubmissionResponse response2;
  if (response.has_need_state()) {
    LOG4CPLUS_DEBUG(logger_, "Validator requested input state");
    // The submission failed due to missing input state, retrieve the inputs and
    // retry.
    std::map<string, string> input_state_entries;
    try {
      input_state_entries = GetFromStorage(response.need_state().keys());
    } catch (const std::exception& e) {
      LOG4CPLUS_ERROR(logger_, e.what());
      return false;
    }
    validator_client_->ValidatePendingSubmission(entryId, input_state_entries,
                                                 commit_req.correlation_id(),
                                                 parent_span, &response2);
    if (!status.ok()) {
      LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code()
                                                    << ": "
                                                    << status.error_message());
      return false;
    }
    assert(response2.has_result());
  } else if (!response.has_result()) {
    LOG4CPLUS_ERROR(logger_, "Validation missing result!");
    return false;
  }
  auto result = response.has_result() ? response.result() : response2.result();
  vector<string> trids{};

  // Insert the DAML log entry into the store.
  auto& logEntry = result.log_entry();
  updates.insert(KeyValuePair(CreateDamlKvbKey(entryId),
                              CreateDamlKvbValue(logEntry, trids)));

  // Insert the DAML state updates into the store.
  // Currently just using the serialization of the DamlStateKey as the key
  // without any prefix.
  for (const auto& kv : result.state_updates()) {
    updates.insert(KeyValuePair(CreateDamlKvbKey(kv.key()),
                                CreateDamlKvbValue(kv.value(), trids)));
  }

  if (pre_execute) {
    auto* pre_execution_result =
        concord_response.mutable_pre_execution_result();

    pre_execution_result->set_read_set_version(current_block_id);

    auto* write_set = pre_execution_result->mutable_write_set();
    for (const auto& kv : updates) {
      auto* new_kv = write_set->add_kv_writes();
      new_kv->set_key(kv.first.data(), kv.first.length());
      new_kv->set_value(kv.second.data(), kv.second.length());
    }

    auto* read_set = pre_execution_result->mutable_read_set();
    for (const auto& k : result.read_set()) {
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

    LOG4CPLUS_DEBUG(logger_, "Done: Pre-execution of DAML command.");
    return true;
  }

  RecordTransaction(entryId, updates, current_block_id, correlation_id,
                    concord_response);
  LOG4CPLUS_DEBUG(logger_, "Done: Handle DAML commit command.");
  return true;
}

void DamlKvbCommandsHandler::RecordTransaction(
    const string& entryId, const SetOfKeyValuePairs& updates,
    BlockId current_block_id, const string& correlation_id,
    ConcordResponse& concord_response) {
  // Commit the block, if there were no conflicts.
  DamlResponse* daml_response = concord_response.mutable_daml_response();

  da_kvbc::CommandReply command_reply;
  da_kvbc::CommitResponse* commit_response = command_reply.mutable_commit();

  BlockId new_block_id = 0;
  concordUtils::Status res = addBlock(updates, new_block_id);
  assert(res.isOK());
  assert(new_block_id == current_block_id + 1);

  // Inform consumers that a transaction has been committed.
  da_kvbc::CommittedTx commited_tx;
  commited_tx.set_transaction_id(entryId);
  commited_tx.set_block_id(new_block_id);
  commited_tx.set_correlation_id(correlation_id);
  committed_txs_.push(commited_tx);

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
    log4cplus::getMDC().put("cid", correlation_id);
    bool result =
        ExecuteCommit(commit_req, flags, time_contract, parent_span, response);
    log4cplus::getMDC().remove("cid");
    return result;
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
        auto commit_req = cmd.commit();
        log4cplus::getMDC().put("cid", commit_req.correlation_id());
        bool result = ExecuteCommit(commit_req, flags, time_contract,
                                    parent_span, response);
        log4cplus::getMDC().clear();
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
  assert(addBlock(empty_updates, newBlockId).isOK());
  assert(newBlockId == currentBlockId + 1);
}

}  // namespace daml
}  // namespace concord
