// Copyright 2019 VMware, all rights reserved

#include "cmd_handler.hpp"

#include <google/protobuf/timestamp.pb.h>
#include <google/protobuf/util/time_util.h>
#include <grpcpp/grpcpp.h>
#include <inttypes.h>
#include <log4cplus/loggingmacros.h>
#include <list>
#include <map>
#include <set>
#include <string>

#include "concord.pb.h"
#include "daml_commit.grpc.pb.h"
#include "daml_data.grpc.pb.h"
#include "daml_validator.grpc.pb.h"

using std::list;
using std::map;
using std::set;
using std::string;

using concord::consensus::Sliver;
using concord::storage::BlockId;
using concord::storage::IBlocksAppender;
using concord::storage::ILocalKeyValueStorageReadOnly;
using concord::storage::Key;
using concord::storage::KeyValuePair;
using concord::storage::SetOfKeyValuePairs;
using concord::storage::Value;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::DamlResponse;

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

grpc::Status KVBCValidatorClient::Validate(
    string entryId, string submission, google::protobuf::Timestamp& record_time,
    const map<string, string>& input_log_entries,
    const map<string, string>& input_state_entries,
    da_kvbc::ValidateResponse* out) {
  da_kvbc::ValidateRequest req;
  req.set_submission(submission);
  req.set_entry_id(entryId);
  *req.mutable_record_time() = record_time;
  for (auto const& entry : input_log_entries) {
    da_kvbc::KeyValuePair* kvpair = req.add_input_log_entries();
    kvpair->set_key(entry.first);
    kvpair->set_value(entry.second);
  }
  for (auto const& entry : input_state_entries) {
    da_kvbc::KeyValuePair* kvpair = req.add_input_state();
    kvpair->set_key(entry.first);
    kvpair->set_value(entry.second);
  }

  grpc::ClientContext context;
  return stub_->ValidateSubmission(&context, req, out);
}

bool KVBCCommandsHandler::ExecuteKVBCRead(const da_kvbc::ReadCommand& request,
                                          const size_t maxReplySize,
                                          char* outReply,
                                          uint32_t& outReplySize) {
  LOG4CPLUS_INFO(logger_, "NOT IMPLEMENTED");
  return false;
}

std::map<string, string> KVBCCommandsHandler::GetFromStorage(
    const google::protobuf::RepeatedPtrField<da_kvbc::KVKey>& keys) {
  std::map<string, string> result;
  for (const auto& kv_key : keys) {
    Value out;
    Key key = CreateSliver(kv_key.key());
    if (ro_storage_->get(key, out).isOK()) {
      result[kv_key.key()] = string((const char*)out.data(), out.length());
    } else {
      // FIXME(JM): We likely should construct a command rejection from this,
      // for which we again would need the C++ protobuf definitions of kvutils.
      LOG4CPLUS_ERROR(logger_,
                      "input '" << kv_key.key() << "' not found in storage!");
    }
  }
  return result;
}

bool KVBCCommandsHandler::ExecuteKVBCCommit(
    const da_kvbc::CommitRequest& commit_req, const size_t max_reply_size,
    char* out_reply, uint32_t& out_reply_size) {
  LOG4CPLUS_DEBUG(logger_, "Handle DAML commit command");

  string prefix = "daml";
  BlockId current_block_id = ro_storage_->getLastBlock();
  SetOfKeyValuePairs updates;

  // Since we're not batching, lets keep it simple and use the next block id as
  // the DAML log entry id.
  string entryId = prefix + "/" + std::to_string(current_block_id + 1);

  google::protobuf::Timestamp record_time =
      google::protobuf::util::TimeUtil::GetEpoch();

  // Resolve the inputs
  std::map<string, string> input_log_entries =
      GetFromStorage(commit_req.input_log_entries());
  std::map<string, string> input_state_entries =
      GetFromStorage(commit_req.input_state());

  // Send the submission for validation.
  da_kvbc::ValidateResponse response;
  grpc::Status status = validator_client_->Validate(
      entryId, commit_req.submission(), record_time, input_log_entries,
      input_state_entries, &response);
  if (!status.ok()) {
    LOG4CPLUS_ERROR(logger_, "Validation failed " << status.error_code() << ": "
                                                  << status.error_message());
    return false;
  }

  // Insert the DAML log entry into the store.
  auto logEntry = response.log_entry();
  updates.insert(KeyValuePair(CreateSliver(entryId), CreateSliver(logEntry)));

  // Insert the DAML state updates into the store.
  // Currently just using the serialization of the DamlStateKey as the key
  // without any prefix.
  for (auto kv : response.state_updates()) {
    updates.insert(
        KeyValuePair(CreateSliver(kv.key()), CreateSliver(kv.value())));
  }

  // Commit the block, if there were no conflicts.
  ConcordResponse concord_response;
  DamlResponse* daml_response = concord_response.mutable_daml_response();

  da_kvbc::CommandReply command_reply;
  da_kvbc::CommitResponse* commit_response = command_reply.mutable_commit();

  BlockId new_block_id = 0;
  Status res = blocks_appender_->addBlock(updates, new_block_id);
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

  string out;
  concord_response.SerializeToString(&out);
  assert(out.size() <= max_reply_size);
  memcpy(out_reply, out.data(), out.size());
  out_reply_size = out.size();

  LOG4CPLUS_DEBUG(logger_, "Done: Handle DAML commit command.");
  return true;
}

bool KVBCCommandsHandler::ExecuteCommand(uint32_t requestSize,
                                         const char* request,
                                         const size_t maxReplySize,
                                         char* outReply,
                                         uint32_t& outReplySize) {
  LOG4CPLUS_INFO(logger_, "Got message of size " << requestSize);
  ConcordRequest concord_req;
  DamlRequest daml_req;

  if (!concord_req.ParseFromArray(request, requestSize) ||
      !concord_req.has_daml_request()) {
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
      return ExecuteKVBCRead(cmd.read(), maxReplySize, outReply, outReplySize);

    case da_kvbc::Command::kCommit:
      return ExecuteKVBCCommit(cmd.commit(), maxReplySize, outReply,
                               outReplySize);

    default:
      return false;
  }
}

bool KVBCCommandsHandler::ExecuteReadOnlyCommand(uint32_t requestSize,
                                                 const char* request,
                                                 const size_t maxReplySize,
                                                 char* outReply,
                                                 uint32_t& outReplySize) {
  LOG4CPLUS_INFO(logger_, "Got message of size " << requestSize);

  ConcordRequest concord_req;
  DamlRequest daml_req;

  if (!concord_req.ParseFromArray(request, requestSize) ||
      !concord_req.has_daml_request()) {
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
      return ExecuteKVBCRead(cmd.read(), maxReplySize, outReply, outReplySize);
    case da_kvbc::Command::kCommit:
      LOG4CPLUS_ERROR(logger_, "ExecuteReadOnlyCommand got write command!");
      return false;
    default:
      return false;
  }
}

int KVBCCommandsHandler::execute(uint16_t clientId, uint64_t sequenceNum,
                                 bool readOnly, uint32_t requestSize,
                                 const char* request, uint32_t maxReplySize,
                                 char* outReply, uint32_t& outActualReplySize) {
  bool res;
  if (readOnly) {
    res = ExecuteReadOnlyCommand(requestSize, request, maxReplySize, outReply,
                                 outActualReplySize);
  } else {
    res = ExecuteCommand(requestSize, request, maxReplySize, outReply,
                         outActualReplySize);
  }

  return res ? 0 : 1;
}

}  // namespace daml
}  // namespace concord
