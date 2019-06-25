// Copyright 2018-2019 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#ifndef ETHEREUM_KVB_COMMANDS_HANDLER_HPP_
#define ETHEREUM_KVB_COMMANDS_HANDLER_HPP_

#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>

#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "ethereum/concord_evm.hpp"
#include "storage/blockchain_interfaces.h"
#include "utils/concord_eth_sign.hpp"

namespace concord {
namespace ethereum {

class EthKvbCommandsHandler : public concord::storage::ICommandsHandler {
 private:
  log4cplus::Logger logger;
  concord::ethereum::EVM &concevm_;
  concord::utils::EthSign &verifier_;
  const concord::config::ConcordConfiguration &config_;
  concord::config::ConcordConfiguration &nodeConfiguration;

  concord::storage::ILocalKeyValueStorageReadOnly *m_ptrRoStorage = nullptr;
  concord::storage::IBlocksAppender *m_ptrBlockAppender = nullptr;

 public:
  EthKvbCommandsHandler(
      concord::ethereum::EVM &concevm, concord::utils::EthSign &verifier,
      const concord::config::ConcordConfiguration &config,
      concord::config::ConcordConfiguration &nodeConfig,
      concord::storage::ILocalKeyValueStorageReadOnly *roStorage,
      concord::storage::IBlocksAppender *appender);
  ~EthKvbCommandsHandler();

  int execute(uint16_t clientId, uint64_t sequenceNum, bool readOnly,
              uint32_t requestSize, const char *request, uint32_t maxReplySize,
              char *outReply, uint32_t &outActualReplySize) override;

 private:
  bool executeCommand(
      uint32_t requestSize, const char *request, uint64_t sequenceNum,
      const concord::storage::ILocalKeyValueStorageReadOnly &roStorage,
      concord::storage::IBlocksAppender &blockAppender,
      const size_t maxReplySize, char *outReply, uint32_t &outReplySize) const;

  bool executeReadOnlyCommand(
      uint32_t requestSize, const char *request,
      const concord::storage::ILocalKeyValueStorageReadOnly &roStorage,
      const size_t maxReplySize, char *outReply, uint32_t &outReplySize) const;

  // Handlers
  bool handle_transaction_request(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_transaction_list_request(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_logs_request(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_block_list_request(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_block_request(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_time_request(com::vmware::concord::ConcordRequest &req,
                           EthKvbStorage &kvbStorage,
                           com::vmware::concord::ConcordResponse &resp) const;
  bool handle_eth_request(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_sendTransaction(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_request_read_only(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_callContract(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_blockNumber(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_getCode(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_getStorageAt(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_getTransactionCount(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;
  bool handle_eth_getBalance(
      com::vmware::concord::ConcordRequest &concreq, EthKvbStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &concresp) const;

  // Utilites
  void build_transaction_response(
      evm_uint256be &hash, concord::common::EthTransaction &tx,
      com::vmware::concord::TransactionResponse *response) const;

  void recover_from(const com::vmware::concord::EthRequest &request,
                    evm_address *sender) const;

  uint64_t parse_block_parameter(
      const com::vmware::concord::EthRequest &request,
      EthKvbStorage &kvbStorage) const;

  evm_result run_evm(const com::vmware::concord::EthRequest &request,
                     EthKvbStorage &kvbStorage, uint64_t timestamp,
                     evm_uint256be &txhash /* OUT */) const;

  evm_uint256be record_transaction(
      const evm_message &message,
      const com::vmware::concord::EthRequest &request, const uint64_t nonce,
      const evm_result &result, const uint64_t timestamp,
      const std::vector<::concord::common::EthLog> &logs,
      EthKvbStorage &kvbStorage) const;

  void collect_logs_from_block(
      const concord::common::EthBlock &block, EthKvbStorage &kvbStorage,
      const com::vmware::concord::LogsRequest &request,
      com::vmware::concord::LogsResponse *response) const;
};

}  // namespace ethereum
}  // namespace concord

#endif  // ETHEREUM_KVB_COMMANDS_HANDLER_HPP_
