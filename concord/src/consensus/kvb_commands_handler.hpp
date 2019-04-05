// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#ifndef CONSENSUS_KVB_COMMANDS_HANDLER_HPP
#define CONSENSUS_KVB_COMMANDS_HANDLER_HPP

#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>

#include "concord.pb.h"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "ethereum/concord_evm.hpp"
#include "utils/concord_eth_sign.hpp"

namespace concord {
namespace consensus {

class KVBCommandsHandler : public Blockchain::ICommandsHandler {
 private:
  log4cplus::Logger logger;
  concord::ethereum::EVM &athevm_;
  concord::utils::EthSign &verifier_;
  boost::program_options::variables_map &config;

  Blockchain::ILocalKeyValueStorageReadOnly *m_ptrRoStorage = nullptr;
  Blockchain::IBlocksAppender *m_ptrBlockAppender = nullptr;

 public:
  KVBCommandsHandler(concord::ethereum::EVM &athevm,
                     concord::utils::EthSign &verifier,
                     boost::program_options::variables_map &config_map,
                     Blockchain::ILocalKeyValueStorageReadOnly *roStorage,
                     Blockchain::IBlocksAppender *appendder);
  ~KVBCommandsHandler();

  int execute(uint16_t clientId, uint64_t sequenceNum, bool readOnly,
              uint32_t requestSize, const char *request, uint32_t maxReplySize,
              char *outReply, uint32_t &outActualReplySize) override;

 private:
  bool executeCommand(
      uint32_t requestSize, const char *request, const uint64_t sequenceNum,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender, const size_t maxReplySize,
      char *outReply, uint32_t &outReplySize) const;

  bool executeReadOnlyCommand(
      uint32_t requestSize, const char *request,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      const size_t maxReplySize, char *outReply, uint32_t &outReplySize) const;

  // Handlers
  bool handle_transaction_request(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_transaction_list_request(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_logs_request(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_block_list_request(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_block_request(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_request(com::vmware::concord::ConcordRequest &athreq,
                          concord::blockchain::KVBStorage &kvbStorage,
                          com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_sendTransaction(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_request_read_only(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_callContract(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_blockNumber(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_getCode(com::vmware::concord::ConcordRequest &athreq,
                          concord::blockchain::KVBStorage &kvbStorage,
                          com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_getStorageAt(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_getTransactionCount(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
  bool handle_eth_getBalance(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::KVBStorage &kvbStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  // Utilites
  void build_transaction_response(
      evm_uint256be &hash, concord::common::EthTransaction &tx,
      com::vmware::concord::TransactionResponse *response) const;

  void recover_from(const com::vmware::concord::EthRequest &request,
                    evm_address *sender) const;

  uint64_t parse_block_parameter(
      const com::vmware::concord::EthRequest &request,
      concord::blockchain::KVBStorage &kvbStorage) const;

  evm_result run_evm(const com::vmware::concord::EthRequest &request,
                     concord::blockchain::KVBStorage &kvbStorage,
                     evm_uint256be &txhash /* OUT */) const;

  evm_uint256be record_transaction(
      const evm_message &message,
      const com::vmware::concord::EthRequest &request, const uint64_t nonce,
      const evm_result &result, const uint64_t timestamp,
      const std::vector<::concord::common::EthLog> &logs,
      concord::blockchain::KVBStorage &kvbStorage) const;

  void collect_logs_from_block(
      const concord::common::EthBlock &block,
      concord::blockchain::KVBStorage &kvbStorage,
      const com::vmware::concord::LogsRequest &request,
      com::vmware::concord::LogsResponse *response) const;
};

}  // namespace consensus
}  // namespace concord

#endif  // CONSENSUS_KVB_COMMANDS_HANDLER_HPP
