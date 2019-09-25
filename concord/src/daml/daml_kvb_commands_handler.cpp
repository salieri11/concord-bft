// Copyright 2019 VMware, all rights reserved

#include "daml_kvb_commands_handler.hpp"

#include <google/protobuf/util/time_util.h>
#include <log4cplus/loggingmacros.h>
#include <map>
#include <string>

#include "time/time_contract.hpp"

using std::map;
using std::string;

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

bool DamlKvbCommandsHandler::ExecuteRead(const da_kvbc::ReadCommand& request,
                                         ConcordResponse& response) {
  LOG4CPLUS_WARN(logger_, "NOT IMPLEMENTED");
  return false;
}

std::map<string, string> DamlKvbCommandsHandler::GetFromStorage(
    const google::protobuf::RepeatedPtrField<std::string>& keys) {
  std::map<string, string> result;
  for (const auto& skey : keys) {
    Value out;
    Key key = CreateSliver(skey);
    if (storage_.get(key, out).isOK()) {
      result[skey] = string((const char*)out.data(), out.length());
    } else {
      // FIXME(JM): We likely should construct a command rejection from this,
      // for which we again would need the C++ protobuf definitions of kvutils.
      LOG4CPLUS_ERROR(logger_, "input '" << skey << "' not found in storage!");
    }
  }
  return result;
}

bool DamlKvbCommandsHandler::ExecuteCommit(
    const da_kvbc::CommitRequest& commit_req, TimeContract* time,
    ConcordResponse& concord_response) {
  LOG4CPLUS_DEBUG(logger_, "Handle DAML commit command");

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

  // Send the submission for validation.
  da_kvbc::ValidateResponse response;
  grpc::Status status =
      validator_client_->Validate(entryId, commit_req.submission(), record_time,
                                  commit_req.participant_id(), &response);
  if (!status.ok()) {
    LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                                  << status.error_message());
    return false;
  }

  da_kvbc::ProvideStateResponse provide_state_response;
  if (response.has_need_state()) {
    LOG4CPLUS_DEBUG(logger_, "Validator requested input state");
    // The submission failed due to missing input state, retrieve the inputs and
    // retry.
    std::map<string, string> input_state_entries =
        GetFromStorage(response.need_state().keys());
    validator_client_->ProvideState(entryId, input_state_entries,
                                    &provide_state_response);
    if (!status.ok()) {
      LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code()
                                                    << ": "
                                                    << status.error_message());
      return false;
    }
    assert(provide_state_response.has_result());
  } else if (!response.has_result()) {
    LOG4CPLUS_ERROR(logger_, "Validation missing result!");
    return false;
  }
  auto result = response.has_result() ? response.result()
                                      : provide_state_response.result();

  // Insert the DAML log entry into the store.
  auto logEntry = result.log_entry();
  updates.insert(KeyValuePair(CreateSliver(entryId), CreateSliver(logEntry)));

  // Insert the DAML state updates into the store.
  // Currently just using the serialization of the DamlStateKey as the key
  // without any prefix.
  for (auto kv : result.state_updates()) {
    updates.insert(
        KeyValuePair(CreateSliver(kv.key()), CreateSliver(kv.value())));
  }

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
  committed_txs_.push(commited_tx);

  commit_response->set_status(da_kvbc::CommitResponse_CommitStatus_OK);
  commit_response->set_block_id(new_block_id);

  string cmd_string;
  command_reply.SerializeToString(&cmd_string);
  daml_response->set_command_reply(cmd_string.c_str(), cmd_string.size());

  LOG4CPLUS_DEBUG(logger_, "Done: Handle DAML commit command.");
  return true;
}

bool DamlKvbCommandsHandler::ExecuteCommand(const ConcordRequest& concord_req,
                                            TimeContract* time_contract,
                                            ConcordResponse& response) {
  DamlRequest daml_req;

  if (!concord_req.has_daml_request()) {
    // we have to ignore this, to allow time-only updates
    return true;
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
      return ExecuteCommit(cmd.commit(), time_contract, response);

    default:
      return false;
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
                                     bool read_only,
                                     TimeContract* time_contract,
                                     ConcordResponse& response) {
  if (read_only) {
    return ExecuteReadOnlyCommand(request, response);
  } else {
    return ExecuteCommand(request, time_contract, response);
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
