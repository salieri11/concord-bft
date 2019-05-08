// Copyright 2018-2019 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.
//
// This is where the replica side of concord starts. Commands that arrive here
// were sent from KVBClient (which was probably used by api_connection).
//
// KVBlockchain calls either executeCommand or executeReadOnlyCommand, depending
// on whether the request was marked read-only. This handler knows which
// requests are which, and therefore only handles request types that are
// properly marked (that is, if this handler thinks a request is read-only, it
// will not accept it as a read-write command).

#include "kvb_commands_handler.hpp"

#include <boost/predef/detail/endian_compat.h>
#include <google/protobuf/text_format.h>
#include <iterator>
#include <vector>
#include "common/concord_exception.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "consensus/kvb/HexTools.h"
#include "ethereum/concord_evm.hpp"
#include "time/time_contract.hpp"
#include "time/time_reading.hpp"
#include "utils/concord_eth_hash.hpp"
#include "utils/concord_eth_sign.hpp"
#include "utils/rlp.hpp"

using namespace boost::program_options;

// Protobuf interface
using namespace com::vmware::concord;

using concord::blockchain::KVBStorage;
using concord::common::BlockNotFoundException;
using concord::common::EthBlock;
using concord::common::EthLog;
using concord::common::EthTransaction;
using concord::common::EVMException;
using concord::common::HexPrintBytes;
using concord::common::TransactionNotFoundException;
using concord::common::zero_address;
using concord::common::zero_hash;
using concord::ethereum::EVM;
using concord::time::TimeContract;
using concord::utils::EthSign;
using concord::utils::RLPBuilder;
using concord::utils::to_evm_uint256be;
using concord::common::operator<<;

using Blockchain::IBlocksAppender;
using Blockchain::ILocalKeyValueStorageReadOnly;

namespace concord {
namespace consensus {

KVBCommandsHandler::KVBCommandsHandler(
    EVM &athevm, EthSign &verifier,
    const concord::config::ConcordConfiguration &config,
    concord::config::ConcordConfiguration &nodeConfig,
    Blockchain::ILocalKeyValueStorageReadOnly *roStorage,
    Blockchain::IBlocksAppender *appender)
    : logger(log4cplus::Logger::getInstance("com.vmware.concord")),
      athevm_(athevm),
      verifier_(verifier),
      config_(config),
      nodeConfiguration(nodeConfig),
      m_ptrRoStorage(roStorage),
      m_ptrBlockAppender(appender) {
  assert(m_ptrBlockAppender);
  assert(m_ptrRoStorage);
}

KVBCommandsHandler::~KVBCommandsHandler() {
  // no other deinitialization necessary
}

int KVBCommandsHandler::execute(uint16_t clientId, uint64_t sequenceNum,
                                bool readOnly, uint32_t requestSize,
                                const char *request, uint32_t maxReplySize,
                                char *outReply, uint32_t &outActualReplySize) {
  bool res;
  if (readOnly) {
    res = executeReadOnlyCommand(requestSize, request, *m_ptrRoStorage,
                                 maxReplySize, outReply, outActualReplySize);
  } else {
    res = executeCommand(requestSize, request, sequenceNum, *m_ptrRoStorage,
                         *m_ptrBlockAppender, maxReplySize, outReply,
                         outActualReplySize);
  }

  return res ? 0 : 1;
}

/**
 * Callback from SBFT/KVB. Process the request (mostly by talking to
 * EVM). Returns false if the command is illegal or invalid; true otherwise.
 */
bool KVBCommandsHandler::executeReadOnlyCommand(
    uint32_t requestSize, const char *request,
    const ILocalKeyValueStorageReadOnly &roStorage, const size_t maxReplySize,
    char *outReply, uint32_t &outReplySize) const {
  KVBStorage kvbStorage(roStorage);

  ConcordRequest command;
  bool result;
  ConcordResponse athresp;
  if (command.ParseFromArray(request, requestSize)) {
    if (command.has_transaction_request()) {
      result = handle_transaction_request(command, kvbStorage, athresp);
    } else if (command.has_transaction_list_request()) {
      result = handle_transaction_list_request(command, kvbStorage, athresp);
    } else if (command.has_logs_request()) {
      result = handle_logs_request(command, kvbStorage, athresp);
    } else if (command.has_block_list_request()) {
      result = handle_block_list_request(command, kvbStorage, athresp);
    } else if (command.has_block_request()) {
      result = handle_block_request(command, kvbStorage, athresp);
    } else if (command.eth_request_size() > 0) {
      result = handle_eth_request_read_only(command, kvbStorage, athresp);
    } else {
      std::string pbtext;
      google::protobuf::TextFormat::PrintToString(command, &pbtext);
      LOG4CPLUS_ERROR(logger, "Unknown read-only command: " << pbtext);
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("Internal concord Error");
      result = false;
    }
  } else {
    LOG4CPLUS_ERROR(logger, "Unable to parse read-only command: "
                                << (HexPrintBytes{request, requestSize}));
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("Internal concord Error");
    result = false;
  }

  if (athresp.SerializeToArray(outReply, maxReplySize)) {
    outReplySize = athresp.ByteSize();
  } else {
    LOG4CPLUS_ERROR(logger, "Reply is too large");
    outReplySize = 0;
  }

  return result;
}

/**
 * Callback from SBFT/KVB. Process the request (mostly by talking to
 * EVM). Returns false if the command is illegal or invalid; true otherwise.
 */
bool KVBCommandsHandler::executeCommand(
    uint32_t requestSize, const char *request, const uint64_t sequenceNum,
    const ILocalKeyValueStorageReadOnly &roStorage,
    IBlocksAppender &blockAppender, const size_t maxReplySize, char *outReply,
    uint32_t &outReplySize) const {
  KVBStorage kvbStorage(roStorage, &blockAppender, sequenceNum);

  ConcordRequest command;
  bool result;
  ConcordResponse athresp;
  if (command.ParseFromArray(request, requestSize)) {
    if (command.eth_request_size() > 0) {
      result = handle_eth_request(command, kvbStorage, athresp);
    } else {
      // SBFT may decide to try one of our read-only commands in read-write
      // mode, for example if it has failed several times. So, go check the
      // read-only list if nothing matched here.
      LOG4CPLUS_INFO(logger, "Unknown read-write command. Trying read-only.");
      return executeReadOnlyCommand(requestSize, request, roStorage,
                                    maxReplySize, outReply, outReplySize);
    }
  } else {
    LOG4CPLUS_ERROR(logger, "Unable to parse command: "
                                << (HexPrintBytes{request, requestSize}));
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("Internal concord Error");
    result = false;
  }

  if (athresp.SerializeToArray(outReply, maxReplySize)) {
    outReplySize = athresp.ByteSize();
  } else {
    LOG4CPLUS_ERROR(logger, "Reply is too large");
    outReplySize = 0;
  }

  return result;
}

/*
 * Handle an ETH RPC request. Returns false if the command was invalid; true
 * otherwise.
 */
bool KVBCommandsHandler::handle_eth_request(ConcordRequest &athreq,
                                            KVBStorage &kvbStorage,
                                            ConcordResponse &athresp) const {
  switch (athreq.eth_request(0).method()) {
    case EthRequest_EthMethod_SEND_TX:
      return handle_eth_sendTransaction(athreq, kvbStorage, athresp);
      break;
    default:
      // SBFT may decide to try one of our read-only commands in read-write
      // mode, for example if it has failed several times. So, go check the
      // read-only list if othing matched here.

      // Be a little extra cautious, and create a read-only KVBStorage object,
      // to prvent accidental modifications.
      KVBStorage roKvbStorage(kvbStorage.getReadOnlyStorage());
      return handle_eth_request_read_only(athreq, roKvbStorage, athresp);
  }
}

/**
 * Handle an eth_sendTransaction request.
 */
bool KVBCommandsHandler::handle_eth_sendTransaction(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);

  uint64_t timestamp = 0;
  if (concord::time::IsTimeServiceEnabled(config_)) {
    TimeContract tc(kvbStorage);
    std::pair<std::string, uint64_t> cmd_time =
        concord::time::GetTimeFromCommand(athreq);
    // divide by 1000, because time service is in milliseconds, but ethereum is
    // in seconds
    timestamp = tc.Update(cmd_time.first, cmd_time.second) / 1000;
  } else {
    timestamp = request.timestamp();
  }

  evm_uint256be txhash{{0}};
  evm_result &&result = run_evm(request, kvbStorage, timestamp, txhash);

  if (result.status_code == EVM_REVERT && result.output_data != nullptr) {
    ErrorResponse *response = athresp.add_error_response();
    std::string error_msg(result.output_data,
                          result.output_data + result.output_size);
    response->set_description(error_msg);
  } else if (txhash != zero_hash) {
    EthResponse *response = athresp.add_eth_response();
    response->set_id(request.id());
    response->set_data(txhash.bytes, sizeof(evm_uint256be));
  } else {
    std::ostringstream description;
    description << "An error occurred running the transaction (status="
                << result.status_code << ")";
    ErrorResponse *response = athresp.add_error_response();
    response->set_description(description.str());
  }

  if (result.release) {
    result.release(&result);
  }

  // We return "true" even if the transaction encountered an error, because the
  // error response is the correct result for the evaluation. That is, we
  // expect that all replicas will return that error. If in the future, there
  // is a failure mode that we don't expect on all nodes (for example, the disk
  // is full), then it will be appropriate to return false.
  return true;
}

/**
 * Fetch a transaction from storage.
 */
bool KVBCommandsHandler::handle_transaction_request(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  try {
    const TransactionRequest request = athreq.transaction_request();
    evm_uint256be hash{{0}};
    std::copy(request.hash().begin(), request.hash().end(), hash.bytes);
    EthTransaction tx = kvbStorage.get_transaction(hash);

    TransactionResponse *response = athresp.mutable_transaction_response();
    build_transaction_response(hash, tx, response);
  } catch (TransactionNotFoundException) {
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("transaction not found");
  } catch (EVMException) {
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("error retrieving transaction");
  }

  // even requests for non-existent transactions are legal/valid
  return true;
}

/**
 * Fetch a transaction list from storage.
 */
bool KVBCommandsHandler::handle_transaction_list_request(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  try {
    const TransactionListRequest request = athreq.transaction_list_request();
    uint16_t remaining =
        std::min(nodeConfiguration.getValue<int>("transaction_list_max_count"),
                 static_cast<int>(request.count()));
    TransactionListResponse *response =
        athresp.mutable_transaction_list_response();
    std::vector<evm_uint256be>::iterator it;
    EthBlock curr_block;

    if (request.has_latest()) {
      evm_uint256be latest_tr{{0}};
      std::copy(request.latest().begin(), request.latest().end(),
                latest_tr.bytes);
      EthTransaction tr = kvbStorage.get_transaction(latest_tr);
      curr_block = kvbStorage.get_block(tr.block_number);
      it = std::find(curr_block.transactions.begin(),
                     curr_block.transactions.end(), tr.hash());
    } else {
      curr_block = kvbStorage.get_block(kvbStorage.current_block_number());
      it = curr_block.transactions.begin();
    }

    while (remaining >= 0) {
      while (it != curr_block.transactions.end() && remaining > 0) {
        TransactionResponse *tr = response->add_transaction();
        EthTransaction tx = kvbStorage.get_transaction(*it);
        build_transaction_response(*it, tx, tr);
        it++;
        remaining--;
      }

      if ((remaining == 0 && it != curr_block.transactions.end()) ||
          curr_block.number == 0) {
        break;
      } else {
        curr_block = kvbStorage.get_block(curr_block.number - 1);
        it = curr_block.transactions.begin();
      }
    }

    if (it != curr_block.transactions.end()) {
      evm_uint256be next = *it;
      response->set_next(next.bytes, sizeof(evm_uint256be));
    }

  } catch (TransactionNotFoundException) {
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("latest transaction not found");
  } catch (EVMException) {
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("error retrieving transactions");
  }

  // even requests for non-existent transactions are legal/valid
  return true;
}

/**
 * Populate a TransactionResponse protobuf with data from an EthTransaction
 * struct.
 */
void KVBCommandsHandler::build_transaction_response(
    evm_uint256be &hash, EthTransaction &tx,
    TransactionResponse *response) const {
  response->set_hash(hash.bytes, sizeof(hash.bytes));
  response->set_from(tx.from.bytes, sizeof(evm_address));
  if (tx.to != zero_address) {
    response->set_to(tx.to.bytes, sizeof(evm_address));
  }
  if (tx.contract_address != zero_address) {
    response->set_contract_address(tx.contract_address.bytes,
                                   sizeof(evm_address));
  }
  if (tx.input.size()) {
    response->set_input(std::string(tx.input.begin(), tx.input.end()));
  }

  response->set_status(tx.status);
  response->set_nonce(tx.nonce);
  response->set_value(tx.value.bytes, sizeof(evm_uint256be));
  response->set_block_hash(tx.block_hash.bytes, sizeof(evm_uint256be));
  response->set_block_number(tx.block_number);
  response->set_gas_limit(tx.gas_limit);
  response->set_gas_price(tx.gas_price);
  response->set_gas_used(tx.gas_used);
  response->set_sig_v(tx.sig_v);
  response->set_sig_r(tx.sig_r.bytes, sizeof(evm_uint256be));
  response->set_sig_s(tx.sig_s.bytes, sizeof(evm_uint256be));

  for (EthLog &log : tx.logs) {
    LogResponse *outlog = response->add_log();
    outlog->set_contract_address(log.address.bytes, sizeof(evm_address));
    for (evm_uint256be topic : log.topics) {
      outlog->add_topic(topic.bytes, sizeof(evm_uint256be));
    }
    if (log.data.size() > 0) {
      outlog->set_data(std::string(log.data.begin(), log.data.end()));
    }
  }
}

/**
 * Get logs from the blockchain
 */
bool KVBCommandsHandler::handle_logs_request(ConcordRequest &athreq,
                                             KVBStorage &kvbStorage,
                                             ConcordResponse &athresp) const {
  const LogsRequest request = athreq.logs_request();
  LogsResponse *response = athresp.mutable_logs_response();
  EthBlock block;

  assert(request.has_from_block() && request.has_to_block());

  try {
    // Let's figure out the block (range) first and then apply other filters
    if (request.has_block_hash()) {
      evm_uint256be block_hash;
      std::copy(request.block_hash().begin(), request.block_hash().end(),
                block_hash.bytes);
      block = kvbStorage.get_block(block_hash);
      collect_logs_from_block(block, kvbStorage, request, response);

    } else {
      uint64_t current_block_number = kvbStorage.current_block_number();
      uint64_t block_start;
      uint64_t block_end;

      if (request.from_block() < 0) {
        // "latest" and "pending"
        block_start = current_block_number;
      } else {
        block_start = static_cast<uint64_t>(request.from_block());
      }

      if (request.to_block() < 0) {
        block_end = current_block_number;
      } else {
        block_end = static_cast<uint64_t>(request.to_block());
      }

      if (block_start > current_block_number ||
          block_end > current_block_number) {
        ErrorResponse *resp = athresp.add_error_response();
        resp->set_description("block doesn't exist yet");
        return true;
      }

      assert(block_end >= block_start);

      for (uint64_t i = block_start; i <= block_end; ++i) {
        block = kvbStorage.get_block(i);
        collect_logs_from_block(block, kvbStorage, request, response);
      }
    }
  } catch (BlockNotFoundException) {
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("block not found");
    return true;
  }

  return true;
}

/**
 * Get logs from a single block
 */
void KVBCommandsHandler::collect_logs_from_block(const EthBlock &block,
                                                 KVBStorage &kvbStorage,
                                                 const LogsRequest &request,
                                                 LogsResponse *response) const {
  EthTransaction tx;
  int64_t tx_log_idx{-1};

  for (auto &tx_hash : block.transactions) {
    tx = kvbStorage.get_transaction(tx_hash);
    for (auto &tx_log : tx.logs) {
      tx_log_idx++;

      // Filter address
      if (request.has_contract_address() &&
          request.contract_address().size() > 0 &&
          memcmp(request.contract_address().data(), tx_log.address.bytes,
                 sizeof(evm_address)) != 0) {
        continue;
      }

      // Filter topics
      // Note: The order matters, element 0 is the event signature and following
      // elements are event parameters
      size_t topic_size = request.topic_size();
      if (topic_size > 0) {
        if (topic_size > tx_log.topics.size()) {
          continue;
        }
        bool match = true;
        for (int i = 0; i < request.topic_size(); ++i) {
          if (memcmp(request.topic(i).data(), tx_log.topics[i].bytes,
                     sizeof(evm_uint256be)) != 0) {
            match = false;
            break;
          }
        }
        if (!match) {
          continue;
        }
      }

      LogResponse *log = response->add_log();

      log->set_contract_address(tx_log.address.bytes, sizeof(evm_address));
      for (evm_uint256be topic : tx_log.topics) {
        log->add_topic(topic.bytes, sizeof(evm_uint256be));
      }
      if (tx_log.data.size() > 0) {
        log->set_data(std::string(tx_log.data.begin(), tx_log.data.end()));
      }
      log->set_block_hash(block.hash.bytes, sizeof(evm_uint256be));
      log->set_block_number(block.number);
      log->set_transaction_hash(tx.hash().bytes, sizeof(evm_uint256be));

      // So far we only have one transaction per block
      log->set_transaction_index(0);
      log->set_log_index(tx_log_idx);
      log->set_transaction_log_index(tx_log_idx);
    }
  }
}

/**
 * Get the list of blocks, starting at latest, and going back count-1 steps in
 * the chain.
 */
bool KVBCommandsHandler::handle_block_list_request(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  const BlockListRequest request = athreq.block_list_request();

  uint64_t latest = std::numeric_limits<uint64_t>::max();
  if (request.has_latest()) {
    latest = request.latest();
  }
  if (latest > kvbStorage.current_block_number()) {
    latest = kvbStorage.current_block_number();
  }

  uint64_t count = 10;
  if (request.has_count()) {
    count = request.count();
  }
  if (count > latest + 1) {
    count = latest + 1;
  }

  LOG4CPLUS_DEBUG(logger, "Getting block list from " << latest << " to "
                                                     << (latest - count));

  BlockListResponse *response = athresp.mutable_block_list_response();
  for (uint64_t i = 0; i < count; i++) {
    EthBlock b = kvbStorage.get_block(latest - i);
    BlockBrief *bb = response->add_block();
    bb->set_number(b.number);
    bb->set_hash(b.hash.bytes, sizeof(evm_uint256be));
  }

  // all list requests are valid
  return true;
}

/**
 * Fetch a block from the database.
 */
bool KVBCommandsHandler::handle_block_request(ConcordRequest &athreq,
                                              KVBStorage &kvbStorage,
                                              ConcordResponse &athresp) const {
  const BlockRequest request = athreq.block_request();

  // According to ethRPC requests the block number string can be either a hex
  // number or it can be one of "latest", "earliest", "pending". Since concord
  // only accepts uint64_t for block number helen will replace "latest" with -1
  // "earliest" with 0 (genesis block) and "pending" with -1 (since in concord
  // blocks are generated instantaneously we can say that "latest" =
  // "pending". Here we will have to first convert -1 to current block number
  // in that case.
  // TODO: Once SBFT is implemented blocks will not be generated instantaneously
  // this will have to be changed at that time.
  try {
    EthBlock block;
    if (request.has_number()) {
      if (request.number() >= 0) {
        // A request for a specific block.
        uint64_t requested_block_number = (uint64_t)request.number();
        if (requested_block_number <= kvbStorage.current_block_number()) {
          block = kvbStorage.get_block(requested_block_number);
        } else {
          // We haven't created a block with this number yet.
          throw BlockNotFoundException();
        }
      } else {
        // Anything less than zero is a special request
        // (latest/pending). Treat them all as "latest" right now.
        block = kvbStorage.get_block(kvbStorage.current_block_number());
      }
    } else if (request.has_hash()) {
      evm_uint256be blkhash;
      std::copy(request.hash().begin(), request.hash().end(), blkhash.bytes);
      block = kvbStorage.get_block(blkhash);
    }

    BlockResponse *response = athresp.mutable_block_response();
    response->set_number(block.number);
    response->set_hash(block.hash.bytes, sizeof(evm_uint256be));
    response->set_parent_hash(block.parent_hash.bytes, sizeof(evm_uint256be));
    response->set_timestamp(block.timestamp);
    response->set_gas_limit(block.gas_limit);
    response->set_gas_used(block.gas_used);

    // TODO: We're not mining, so nonce is mostly irrelevant. Maybe there will
    // be something relevant from KVBlockchain to put in here?
    // Note the proof of work nonce for Ethereum is 8 bytes.
    response->set_nonce(zero_hash.bytes, sizeof(uint64_t));

    // TODO: This is supposed to be "the size of this block in bytes". This is
    // a sum of transaction inputs, storage updates, log events, and maybe
    // other things. It needs to be counted when the block is
    // recorded. Does KVBlockchain have this facility built in?
    response->set_size(1);

    for (auto t : block.transactions) {
      try {
        EthTransaction tx = kvbStorage.get_transaction(t);
        TransactionResponse *txresp = response->add_transaction();
        build_transaction_response(t, tx, txresp);
      } catch (...) {
        LOG4CPLUS_ERROR(logger, "Error fetching block transaction "
                                    << t << " from block " << block.number);

        // we can still fill out some of the info, though, which may help an
        // operator debug
        TransactionResponse *txresp = response->add_transaction();
        txresp->set_hash(t.bytes, sizeof(evm_uint256be));
      }
    }
  } catch (BlockNotFoundException) {
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("block not found");
  }

  // even requests for non-existent blocks are legal/valid
  return true;
}

/*
 * Handle an ETH RPC request. Returns false if the command was invalid; true
 * otherwise.
 */
bool KVBCommandsHandler::handle_eth_request_read_only(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  switch (athreq.eth_request(0).method()) {
    case EthRequest_EthMethod_CALL_CONTRACT:
      return handle_eth_callContract(athreq, kvbStorage, athresp);
      break;
    case EthRequest_EthMethod_BLOCK_NUMBER:
      return handle_eth_blockNumber(athreq, kvbStorage, athresp);
      break;
    case EthRequest_EthMethod_GET_CODE:
      return handle_eth_getCode(athreq, kvbStorage, athresp);
      break;
    case EthRequest_EthMethod_GET_STORAGE_AT:
      return handle_eth_getStorageAt(athreq, kvbStorage, athresp);
      break;
    case EthRequest_EthMethod_GET_TX_COUNT:
      return handle_eth_getTransactionCount(athreq, kvbStorage, athresp);
      break;
    case EthRequest_EthMethod_GET_BALANCE:
      return handle_eth_getBalance(athreq, kvbStorage, athresp);
      break;
    default:
      ErrorResponse *e = athresp.add_error_response();
      e->mutable_description()->assign("ETH Method Not Implemented");
      return false;
  }
}

/**
 * Handle the 'contract.method.call()' functionality of ethereum. This is
 * used when the method being called does not make any changes to the state
 * of the system. Hence, in this case, we also do not record any transaction
 * Instead the return value of the contract function call will be returned
 * as the 'data' of EthResponse.
 */
bool KVBCommandsHandler::handle_eth_callContract(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);

  // Time service readings are only added to write-commands (call is
  // read-only). TODO: look up time from most recent block, or block specified
  // by call RPC.
  uint64_t timestamp = 0;

  evm_uint256be txhash{{0}};
  evm_result &&result = run_evm(request, kvbStorage, timestamp, txhash);
  // Here we don't care about the txhash. Transaction was never
  // recorded, instead we focus on the result object and the
  // output_data field in it.
  if (result.status_code == EVM_SUCCESS) {
    EthResponse *response = athresp.add_eth_response();
    response->set_id(request.id());
    if (result.output_data != NULL && result.output_size > 0) {
      response->set_data(result.output_data, result.output_size);
    }
  } else {
    ErrorResponse *err = athresp.add_error_response();
    err->mutable_description()->assign("Error while calling contract");
  }

  if (result.release) {
    result.release(&result);
  }

  // the request was valid, even if it failed
  return true;
}

/**
 * Get the latest written block number.
 */
bool KVBCommandsHandler::handle_eth_blockNumber(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);
  EthResponse *response = athresp.add_eth_response();
  evm_uint256be current_block{{0}};
  to_evm_uint256be(kvbStorage.current_block_number(), &current_block);
  response->set_data(current_block.bytes, sizeof(evm_uint256be));
  response->set_id(request.id());

  return true;
}

/**
 * Get the code stored for a contract.
 */
bool KVBCommandsHandler::handle_eth_getCode(ConcordRequest &athreq,
                                            KVBStorage &kvbStorage,
                                            ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);
  evm_address account{{0}};
  std::copy(request.addr_to().begin(), request.addr_to().end(), account.bytes);

  // handle the block number parameter
  uint64_t block_number = parse_block_parameter(request, kvbStorage);

  std::vector<uint8_t> code;
  evm_uint256be hash{{0}};
  if (kvbStorage.get_code(account, code, hash, block_number)) {
    EthResponse *response = athresp.add_eth_response();
    response->set_data(std::string(code.begin(), code.end()));
    response->set_id(request.id());
  } else {
    ErrorResponse *error = athresp.add_error_response();
    error->set_description("No code found at given address");
  }

  return true;
}

/**
 * Get the data stored for the given contract at the given location
 */
bool KVBCommandsHandler::handle_eth_getStorageAt(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);

  evm_address account{{0}};
  std::copy(request.addr_to().begin(), request.addr_to().end(), account.bytes);
  evm_uint256be key{{0}};
  std::copy(request.data().begin(), request.data().end(), key.bytes);
  // TODO(BWF): now that we're using KVB for storage, we can support the block
  // argument

  uint64_t block_number = parse_block_parameter(request, kvbStorage);

  evm_uint256be data = kvbStorage.get_storage(account, key, block_number);
  EthResponse *response = athresp.add_eth_response();
  response->set_id(request.id());
  response->set_data(data.bytes, sizeof(data));

  return true;
}

/**
 * Get the nonce for the given account
 */
bool KVBCommandsHandler::handle_eth_getTransactionCount(
    ConcordRequest &athreq, KVBStorage &kvbStorage,
    ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);

  evm_address account{{0}};
  std::copy(request.addr_to().begin(), request.addr_to().end(), account.bytes);

  uint64_t block_number = parse_block_parameter(request, kvbStorage);

  uint64_t nonce = kvbStorage.get_nonce(account, block_number);
  evm_uint256be bignonce;
  memset(bignonce.bytes, 0, sizeof(bignonce));
#ifdef BOOST_LITTLE_ENDIAN
  std::reverse_copy(reinterpret_cast<uint8_t *>(&nonce),
                    reinterpret_cast<uint8_t *>(&nonce) + sizeof(nonce),
                    bignonce.bytes + (sizeof(bignonce) - sizeof(nonce)));
#else
  std::copy(reinterpret_cast<uint8_t *>(&nonce),
            reinterpret_cast<uint8_t *>(&nonce) + sizeof(nonce),
            bignonce.bytes + (sizeof(bignonce) - sizeof(nonce)));
#endif

  EthResponse *response = athresp.add_eth_response();
  response->set_id(request.id());
  response->set_data(bignonce.bytes, sizeof(bignonce));

  return true;
}

/**
 * Get the balance for the given account
 */
bool KVBCommandsHandler::handle_eth_getBalance(ConcordRequest &athreq,
                                               KVBStorage &kvbStorage,
                                               ConcordResponse &athresp) const {
  const EthRequest request = athreq.eth_request(0);

  evm_address account;
  std::copy(request.addr_to().begin(), request.addr_to().end(), account.bytes);

  uint64_t block_number = parse_block_parameter(request, kvbStorage);
  evm_uint256be balance = kvbStorage.get_balance(account, block_number);

  EthResponse *response = athresp.add_eth_response();
  response->set_id(request.id());
  response->set_data(balance.bytes, sizeof(balance));

  return true;
}

/**
 * Extract "from" address from request+signature.
 */
void KVBCommandsHandler::recover_from(const EthRequest &request,
                                      evm_address *sender) const {
  static const std::vector<uint8_t> empty;

  if (request.has_sig_v() && request.has_sig_r() &&
      request.sig_r().size() == sizeof(evm_uint256be) && request.has_sig_s() &&
      request.sig_s().size() == sizeof(evm_uint256be)) {
    // First we have to reconstruct the original message
    RLPBuilder rlpb;
    rlpb.start_list();

    int8_t actualV = 0;
    uint64_t chainID = request.sig_v();
    if (chainID > 28) {
      // EIP155
      rlpb.add(empty);  // Signature S
      rlpb.add(empty);  // Signature R

      if (chainID % 2) {
        actualV = 0;
        chainID = (chainID - 35) / 2;
      } else {
        actualV = 1;
        chainID = (chainID - 36) / 2;
      }
      rlpb.add(chainID);  // Signature V
    } else if (chainID >= 27) {
      actualV = chainID - 27;
      chainID = 0;
    }

    if (request.has_data()) {
      rlpb.add(request.data());
    } else {
      rlpb.add(empty);
    }

    if (request.has_value()) {
      // trying not to decode the value, just to recode it, but if it's small
      // enough, we have to have special handling
      const std::string &value = request.value();
      if (value.size() == 1 && value[0] >= 0x00 && value[0] <= 0x7f) {
        // small value is represented by itself
        rlpb.add(value[0]);
      } else {
        rlpb.add(value);
      }
    } else {
      rlpb.add(empty);
    }

    if (request.has_addr_to()) {
      rlpb.add(request.addr_to());
    } else {
      rlpb.add(empty);
    }

    if (request.has_gas() && request.gas() > 0) {
      rlpb.add(request.gas());
    } else {
      rlpb.add(empty);
    }

    if (request.has_gas_price() && request.gas_price() > 0) {
      rlpb.add(request.gas_price());
    } else {
      rlpb.add(empty);
    }

    if (request.has_nonce() && request.nonce() > 0) {
      rlpb.add(request.nonce());
    } else {
      rlpb.add(empty);
    }

    std::vector<uint8_t> rlp = rlpb.build();
    evm_uint256be rlp_hash = concord::utils::eth_hash::keccak_hash(rlp);

    // Then we can check it against the signature.

    evm_uint256be sigR{{0}};
    std::copy(request.sig_r().begin(), request.sig_r().end(), sigR.bytes);
    evm_uint256be sigS{{0}};
    std::copy(request.sig_s().begin(), request.sig_s().end(), sigS.bytes);

    *sender = verifier_.ecrecover(rlp_hash, actualV, sigR, sigS);
  }
}

/**
 * parse the block number parameter
 * @param request ethrequest
 * @param kvbStorage
 * @return block number
 */
uint64_t KVBCommandsHandler::parse_block_parameter(
    const EthRequest &request, KVBStorage &kvbStorage) const {
  uint64_t block_number = std::numeric_limits<uint64_t>::max();
  if (request.has_block_number()) {
    block_number = request.block_number();
  }
  if (block_number > kvbStorage.current_block_number()) {
    block_number = kvbStorage.current_block_number();
  }
  return block_number;
}

/**
 * Pass a transaction or call to the EVM for execution.
 */
evm_result KVBCommandsHandler::run_evm(const EthRequest &request,
                                       KVBStorage &kvbStorage,
                                       uint64_t timestamp,
                                       evm_uint256be &txhash /* OUT */) const {
  evm_message message;
  evm_result result;

  memset(&message, 0, sizeof(message));
  memset(&result, 0, sizeof(result));

  if (request.has_addr_from()) {
    if (request.addr_from().length() != sizeof(message.sender)) {
      result.status_code = EVM_REJECTED;
      txhash = zero_hash;
      return result;
    }
    memcpy(message.sender.bytes, request.addr_from().c_str(), 20);

    if (request.has_sig_v() && request.has_sig_r() && request.has_sig_s()) {
      evm_address sig_from;
      recover_from(request, &sig_from);

      if (sig_from == zero_address) {
        LOG4CPLUS_DEBUG(logger, "Signature was invalid");
        result.status_code = EVM_REJECTED;
        txhash = zero_hash;
        return result;
      }

      if (message.sender != sig_from) {
        LOG4CPLUS_DEBUG(logger, "Message sender does not match signature");
        result.status_code = EVM_REJECTED;
        txhash = zero_hash;
        return result;
      }
    }
  } else if (request.method() != EthRequest_EthMethod_CALL_CONTRACT) {
    recover_from(request, &message.sender);
    if (message.sender == zero_address) {
      LOG4CPLUS_DEBUG(logger, "Signature was invalid");
      result.status_code = EVM_REJECTED;
      txhash = zero_hash;
      return result;
    }
  }

  if (request.has_data()) {
    message.input_data =
        reinterpret_cast<const uint8_t *>(request.data().c_str());
    message.input_size = request.data().length();
  }

  if (request.has_value()) {
    size_t req_offset, val_offset;
    if (request.value().size() > sizeof(evm_uint256be)) {
      // TODO: this should probably throw an error instead
      req_offset = request.value().size() - sizeof(evm_uint256be);
      val_offset = 0;
    } else {
      req_offset = 0;
      val_offset = sizeof(evm_uint256be) - request.value().length();
    }
    std::copy(request.value().begin() + req_offset, request.value().end(),
              message.value.bytes + val_offset);
  }

  if (request.has_gas()) {
    message.gas = request.gas();
  } else if (request.has_gas_limit()) {
    // Internal parameter
    message.gas = request.gas_limit();
  } else {
    // This was the former static value used for the gas limit.
    message.gas = 1000000;
  }

  // If this is not a transaction, nonce doesn't matter. If it is, get it from
  // either the request or storage.
  uint64_t nonce = 0;
  if (!kvbStorage.is_read_only()) {
    if (request.has_nonce()) {
      nonce = request.nonce();
    } else {
      nonce = kvbStorage.get_nonce(message.sender);
    }
  }

  // If this is not a transaction, set flags to static so that execution is
  // prohibited from modifying state.
  if (kvbStorage.is_read_only()) {
    message.flags |= EVM_STATIC;
  }

  std::vector<EthLog> logs;

  if (request.has_addr_to()) {
    message.kind = EVM_CALL;

    if (request.addr_to().length() != sizeof(message.destination)) {
      result.status_code = EVM_REJECTED;
      txhash = zero_hash;
      return result;
    }
    memcpy(message.destination.bytes, request.addr_to().c_str(),
           sizeof(message.destination));

    result = athevm_.run(message, timestamp, kvbStorage, logs, message.sender,
                         message.destination);
  } else {
    message.kind = EVM_CREATE;

    assert(!kvbStorage.is_read_only());

    evm_address contract_address =
        athevm_.contract_destination(message.sender, nonce);

    result = athevm_.create(contract_address, message, timestamp, kvbStorage,
                            logs, message.sender);
  }

  LOG4CPLUS_INFO(logger, "Execution result -"
                             << " status_code: " << result.status_code
                             << " gas_left: " << result.gas_left
                             << " output_size: " << result.output_size);

  if (result.status_code != EVM_SUCCESS) {
    // If the transaction failed, don't record any of its side effects.
    // TODO: except gas deduction?

    // This constructor doesn't do anything, so it's safe to create it without
    // checking the feature flag. We need the instance to live from the time of
    // read to the time of write, so that we can save the time service state
    // across this storage reset.
    TimeContract tc(kvbStorage);

    if (concord::time::IsTimeServiceEnabled(config_)) {
      // cache latest time contract state
      tc.GetTime();
    }

    kvbStorage.reset();

    if (concord::time::IsTimeServiceEnabled(config_)) {
      // Reapply the time update
      tc.StoreLatestSamples();
    }
  }

  if (!kvbStorage.is_read_only()) {
    // If this is a transaction, and not just a call, record it.
    txhash = record_transaction(message, request, nonce, result, timestamp,
                                logs, kvbStorage);
  }

  return result;
}

/**
 * Increment the sender's nonce, Add the transaction and write a block with
 * it. Message call depth must be zero.
 */
evm_uint256be KVBCommandsHandler::record_transaction(
    const evm_message &message, const EthRequest &request, const uint64_t nonce,
    const evm_result &result, const uint64_t timestamp,
    const std::vector<EthLog> &logs, KVBStorage &kvbStorage) const {
  // "to" is empty if this was a create
  evm_address to =
      message.kind == EVM_CALL ? message.destination : zero_address;
  evm_address create_address =
      message.kind == EVM_CREATE ? result.create_address : zero_address;

  uint64_t gas_price = 0;
  if (request.has_gas_price()) {
    gas_price = request.gas_price();
  }

  evm_uint256be sig_r{{0}};
  evm_uint256be sig_s{{0}};
  uint64_t sig_v;
  if (request.has_sig_r() && request.has_sig_s() && request.has_sig_v()) {
    std::copy(request.sig_r().begin(), request.sig_r().end(), sig_r.bytes);
    std::copy(request.sig_s().begin(), request.sig_s().end(), sig_s.bytes);
    sig_v = request.sig_v();
  } else {
    sig_r = zero_hash;
    sig_s = zero_hash;
    sig_v = 0;
  }

  uint64_t gas_limit = static_cast<uint64_t>(message.gas);

  assert(result.gas_left >= 0);
  uint64_t gas_left = static_cast<uint64_t>(result.gas_left);
  uint64_t gas_used = gas_limit - gas_left;

  EthTransaction tx = {
      nonce,
      zero_hash,       // block_hash: will be set during write_block
      0,               // block_number: will be set during write_block
      message.sender,  // from
      to,
      create_address,
      std::vector<uint8_t>(message.input_data,
                           message.input_data + message.input_size),
      result.status_code,
      message.value,  // value
      gas_price,
      gas_limit,
      gas_used,
      logs,
      sig_r,
      sig_s,
      sig_v};
  kvbStorage.add_transaction(tx);
  kvbStorage.set_nonce(message.sender, nonce + 1);

  evm_uint256be txhash = tx.hash();
  LOG4CPLUS_DEBUG(logger, "Recording transaction " << txhash);

  assert(message.depth == 0);
  kvbStorage.write_block(timestamp, message.gas);

  return txhash;
}

}  // namespace consensus
}  // namespace concord
