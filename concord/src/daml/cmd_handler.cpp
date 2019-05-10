// Copyright 2019 VMware, all rights reserved

#include "cmd_handler.hpp"

#include <inttypes.h>
#include <log4cplus/loggingmacros.h>
#include <list>
#include <map>
#include <set>

#include "concord.pb.h"
#include "daml_commit.grpc.pb.h"
#include "daml_data.grpc.pb.h"

using std::list;
using std::map;
using std::set;

using namespace com::digitalasset;

using Blockchain::BlockId;
using Blockchain::IBlocksAppender;
using Blockchain::ILocalKeyValueStorageReadOnly;
using Blockchain::Key;
using Blockchain::KeyValuePair;
using Blockchain::SetOfKeyValuePairs;
using Blockchain::Sliver;
using Blockchain::Value;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::DamlResponse;

namespace concord {
namespace daml {

Sliver createSliver(char* content, const size_t size) {
  char* val = new char[size];
  memcpy(val, content, size);
  return Sliver(val, size);
}

Sliver createSliver(const std::string& content) {
  return createSliver(const_cast<char*>(content.c_str()), content.size());
}

bool KVBCCommandsHandler::executeKVBCRead(
    const kvbc::ReadTransactionRequest& request, const size_t maxReplySize,
    char* outReply, uint32_t& outReplySize) {
  LOG4CPLUS_INFO(logger_, "NOT IMPLEMENTED");
  return false;
}

Key KVBCCommandsHandler::absContractIdToKey(std::string prefix,
                                            kvbc::AbsContractId coid) {
  std::string cid = prefix + "/activeContracts/" + coid.tx_id() + "/" +
                    std::to_string(coid.rel_id().node_id());
  LOG4CPLUS_INFO(logger_, "KEY '" << cid << "'");
  return createSliver(cid);
}

Key KVBCCommandsHandler::relContractIdToKey(std::string prefix, int64_t txId,
                                            kvbc::RelContractId coid) {
  std::string cid = prefix + "/activeContracts/" + std::to_string(txId) + "/" +
                    std::to_string(coid.node_id());
  LOG4CPLUS_INFO(logger_, "KEY '" << cid << "'");
  return createSliver(cid);
}

bool KVBCCommandsHandler::contractIsActive(const Key& coid) {
  Value out;
  if (ro_storage_->get(coid, out).isOK()) {
    if (!strncmp((const char*)out.data(), "active", out.length())) {
      return true;
    }
  }
  return false;
}

KeyValuePair KVBCCommandsHandler::contractActiveKV(const Key& coid) {
  std::string active = std::string{"active"};
  return KeyValuePair(coid, createSliver(active));
}

KeyValuePair KVBCCommandsHandler::contractArchivedKV(const Key& coid) {
  std::string archived = std::string{"archived"};
  return KeyValuePair(coid, createSliver(archived));
}

bool KVBCCommandsHandler::executeKVBCCommit(
    const kvbc::CommitRequest& commitReq, const size_t maxReplySize,
    char* outReply, uint32_t& outReplySize) {
  std::string prefix = "daml";
  BlockId currentBlockId = ro_storage_->getLastBlock();
  SetOfKeyValuePairs updates;
  bool hasConflicts = false;

  // Construct the updates to set the head to new value
  // and to set the transaction. We use the following key schema:
  // XYZ-HEAD records the pointer to last added transaction (64-bit unsigned
  // int, as decimal string) XYZ/0 is the first transaction, and so on.
  std::string headKeyStr = std::string(prefix + "-HEAD");
  Key headKey = createSliver(headKeyStr);

  // Try to read the head pointer, otherwise default to 0.
  int64_t newHead = 0;
  Value headOut;
  if (ro_storage_->get(headKey, headOut).isOK()) {
    std::string headStr((const char*)headOut.data(), headOut.length());
    try {
      newHead = std::stoll(headStr) + 1;
    } catch (std::invalid_argument) {
      LOG4CPLUS_ERROR(logger_, "INVALID ARGUMENT for " << headStr);
      newHead = 0;
    }
  }

  // Add the update to set the head to new value.
  std::string newHeadStr = std::to_string(newHead);
  LOG4CPLUS_INFO(logger_,
                 "UPDATE k'" << headKeyStr << "' v'" << newHeadStr << "'");
  updates.insert(KeyValuePair(headKey, createSliver(newHeadStr)));

  // Add the update to insert the transaction.
  std::string txKeyStr =
      std::string(prefix + "/transactions/" + std::to_string(newHead));
  Key txKey = createSliver(txKeyStr);
  auto txBlob = commitReq.tx();
  Value txBlobValue =
      createSliver(const_cast<char*>(txBlob.data()), txBlob.size());
  LOG4CPLUS_INFO(logger_, "UPDATE k'" << txKeyStr << "' v TX");
  updates.insert(KeyValuePair(txKey, txBlobValue));

  // Check that input contracts to the transaction are active.
  // FIXME(JM): This does not verify the activeness of the contract at the
  // ledger effective time of the transaction!
  for (int i = 0; i < commitReq.input_contracts_size() && !hasConflicts; i++) {
    const kvbc::AbsContractId& coid = commitReq.input_contracts(i);
    hasConflicts = !contractIsActive(absContractIdToKey(prefix, coid));
  }

  // Mark consumed contracts inactive.
  for (int i = 0; i < commitReq.consumed_contracts_size() && !hasConflicts;
       i++) {
    Key coidKey = absContractIdToKey(prefix, commitReq.consumed_contracts(i));
    updates.insert(contractArchivedKV(coidKey));
  }

  // Mark created contracts active.
  for (int i = 0; i < commitReq.created_contracts_size() && !hasConflicts;
       i++) {
    Key coidKey =
        relContractIdToKey(prefix, newHead, commitReq.created_contracts(i));
    updates.insert(contractActiveKV(coidKey));
  }

  // Commit the block, if there were no conflicts.
  ConcordResponse concord_response;
  DamlResponse* daml_response = concord_response.mutable_daml_response();

  kvbc::CommandReply reply;
  kvbc::CommitResponse* commitResponse = reply.mutable_commit();

  if (!hasConflicts) {
    LOG4CPLUS_INFO(logger_, "No conflicts, committing...");

    BlockId newBlockId = 0;
    assert(blocks_appender_->addBlock(updates, newBlockId).isOK());
    assert(newBlockId == currentBlockId + 1);

    // Inform consumers that a transaction has been committed.
    kvbc::CommittedTx committedTx;
    committedTx.set_transaction_id(txKeyStr);
    committedTx.set_block_id(newBlockId);
    committedTxs_.push(committedTx);

    commitResponse->set_status(kvbc::CommitResponse_CommitStatus_OK);
    commitResponse->set_block_id(newBlockId);
  } else {
    LOG4CPLUS_ERROR(logger_, "Conflict!");
    commitResponse->set_status(kvbc::CommitResponse_CommitStatus_CONFLICT);
  }

  std::string cmd_string;
  reply.SerializeToString(&cmd_string);
  daml_response->set_command_reply(cmd_string.c_str(), cmd_string.size());

  std::string out;
  concord_response.SerializeToString(&out);
  assert(out.size() <= maxReplySize);
  memcpy(outReply, out.data(), out.size());
  outReplySize = out.size();

  LOG4CPLUS_INFO(logger_, "COMMIT CMD RETURN");
  return true;
}

bool KVBCCommandsHandler::executeCommand(uint32_t requestSize,
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

  kvbc::Command cmd;
  if (!cmd.ParseFromString(daml_req.command())) {
    LOG4CPLUS_ERROR(logger_, "Failed to parse DAML/Command request");
    return false;
  }

  switch (cmd.cmd_case()) {
    case kvbc::Command::kRead:
      return executeKVBCRead(cmd.read(), maxReplySize, outReply, outReplySize);

    case kvbc::Command::kCommit:
      return executeKVBCCommit(cmd.commit(), maxReplySize, outReply,
                               outReplySize);

    default:
      return false;
  }
}

bool KVBCCommandsHandler::executeReadOnlyCommand(uint32_t requestSize,
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

  kvbc::Command cmd;
  if (!cmd.ParseFromString(daml_req.command())) {
    LOG4CPLUS_ERROR(logger_, "Failed to parse DAML/Command request");
    return false;
  }

  switch (cmd.cmd_case()) {
    case kvbc::Command::kRead:
      return executeKVBCRead(cmd.read(), maxReplySize, outReply, outReplySize);
    case kvbc::Command::kCommit:
      LOG4CPLUS_ERROR(logger_, "executeReadOnlyCommand got write command!");
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
    res = executeReadOnlyCommand(requestSize, request, maxReplySize, outReply,
                                 outActualReplySize);
  } else {
    res = executeCommand(requestSize, request, maxReplySize, outReply,
                         outActualReplySize);
  }

  return res ? 0 : 1;
}

}  // namespace daml
}  // namespace concord
