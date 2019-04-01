// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#ifndef CONCORD_KVB_HPP
#define CONCORD_KVB_HPP

#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>

#include "concord.pb.h"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "ethereum/concord_evm.hpp"
#include "utils/concord_eth_sign.hpp"

namespace com {
namespace vmware {
namespace concord {

class KVBCommandsHandler : public Blockchain::ICommandsHandler {
 private:
  log4cplus::Logger logger;
  EVM &athevm_;
  EthSign &verifier_;
  boost::program_options::variables_map &config;

  Blockchain::ILocalKeyValueStorageReadOnly *m_ptrRoStorage = nullptr;
  Blockchain::IBlocksAppender *m_ptrBlockAppender = nullptr;

 public:
  KVBCommandsHandler(EVM &athevm, EthSign &verifier,
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
  bool handle_transaction_request(ConcordRequest &athreq,
                                  KVBStorage &kvbStorage,
                                  ConcordResponse &athresp) const;
  bool handle_transaction_list_request(ConcordRequest &athreq,
                                       KVBStorage &kvbStorage,
                                       ConcordResponse &athresp) const;
  bool handle_logs_request(ConcordRequest &athreq, KVBStorage &kvbStorage,
                           ConcordResponse &athresp) const;
  bool handle_block_list_request(ConcordRequest &athreq, KVBStorage &kvbStorage,
                                 ConcordResponse &athresp) const;
  bool handle_block_request(ConcordRequest &athreq, KVBStorage &kvbStorage,
                            ConcordResponse &athresp) const;
  bool handle_eth_request(ConcordRequest &athreq, KVBStorage &kvbStorage,
                          ConcordResponse &athresp) const;
  bool handle_eth_sendTransaction(ConcordRequest &athreq,
                                  KVBStorage &kvbStorage,
                                  ConcordResponse &athresp) const;
  bool handle_eth_request_read_only(ConcordRequest &athreq,
                                    KVBStorage &kvbStorage,
                                    ConcordResponse &athresp) const;
  bool handle_eth_callContract(ConcordRequest &athreq, KVBStorage &kvbStorage,
                               ConcordResponse &athresp) const;
  bool handle_eth_blockNumber(ConcordRequest &athreq, KVBStorage &kvbStorage,
                              ConcordResponse &athresp) const;
  bool handle_eth_getCode(ConcordRequest &athreq, KVBStorage &kvbStorage,
                          ConcordResponse &athresp) const;
  bool handle_eth_getStorageAt(ConcordRequest &athreq, KVBStorage &kvbStorage,
                               ConcordResponse &athresp) const;
  bool handle_eth_getTransactionCount(ConcordRequest &athreq,
                                      KVBStorage &kvbStorage,
                                      ConcordResponse &athresp) const;
  bool handle_eth_getBalance(ConcordRequest &athreq, KVBStorage &kvbStorage,
                             ConcordResponse &athresp) const;

  // Utilites
  void build_transaction_response(evm_uint256be &hash, EthTransaction &tx,
                                  TransactionResponse *response) const;

  void recover_from(const EthRequest &request, evm_address *sender) const;

  uint64_t parse_block_parameter(const EthRequest &request,
                                 KVBStorage &kvbStorage) const;

  evm_result run_evm(const EthRequest &request, KVBStorage &kvbStorage,
                     evm_uint256be &txhash /* OUT */) const;

  evm_uint256be record_transaction(
      const evm_message &message, const EthRequest &request,
      const uint64_t nonce, const evm_result &result, const uint64_t timestamp,
      const std::vector<EthLog> &logs, KVBStorage &kvbStorage) const;

  void collect_logs_from_block(const EthBlock &block, KVBStorage &kvbStorage,
                               const LogsRequest &request,
                               LogsResponse *response) const;
};

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif  // CONCORD_KVB_HPP
