// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>

#include "kvb/BlockchainInterfaces.h"
#include "kvb/slice.h"
#include "athena_evm.hpp"
#include "athena.pb.h"

namespace com {
namespace vmware {
namespace athena {

class KVBCommandsHandler : public Blockchain::ICommandsHandler {
private:
   log4cplus::Logger logger;
   EVM &athevm_;
   EthSign &verifier_;
   boost::program_options::variables_map &config;

   Blockchain::ILocalKeyValueStorageReadOnly *m_ptrRoStorage = nullptr;
   Blockchain::IBlocksAppender *m_ptrBlockAppender = nullptr;

public:
   KVBCommandsHandler(EVM &athevm,
                      EthSign &verifier,
                      boost::program_options::variables_map &config_map,
                      Blockchain::ILocalKeyValueStorageReadOnly *roStorage,
                      Blockchain::IBlocksAppender *appendder);
   ~KVBCommandsHandler();

   int execute(uint16_t clientId,
               bool readOnly,
               uint32_t requestSize,
               const char* request,
               uint32_t maxReplySize,
               char* outReply,
               uint32_t &outActualReplySize) override;

private:
   bool executeCommand(
      const Blockchain::Slice command,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender,
      const size_t maxReplySize,
      char *outReply,
      uint32_t &outReplySize) const;

   bool executeReadOnlyCommand(
      const Blockchain::Slice command,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      const size_t maxReplySize,
      char *outReply,
      uint32_t &outReplySize) const;

   // Handlers
   bool handle_transaction_request(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_transaction_list_request(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_block_list_request(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_block_request(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_request(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_sendTransaction(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_personal_newAccount(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_request_read_only(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_callContract(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_blockNumber(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_getCode(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_getStorageAt(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_getTransactionCount(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;
   bool handle_eth_getBalance(
      AthenaRequest &athreq,
      KVBStorage &kvbStorage,
      AthenaResponse &athresp) const;

   // Utilites
   void build_transaction_response(evm_uint256be &hash,
                                   EthTransaction &tx,
                                   TransactionResponse* response) const;

   void recover_from(const EthRequest &request, evm_address *sender) const;

   uint64_t parse_block_parameter(
      const EthRequest &request,
      KVBStorage &kvbStorage) const;

   evm_result run_evm(
      const EthRequest &request,
      KVBStorage &kvbStorage,
      evm_uint256be &txhash /* OUT */) const;

   evm_uint256be record_transaction(
      const evm_message &message,
      const EthRequest &request,
      const uint64_t nonce,
      const evm_result &result,
      const uint64_t timestamp,
      KVBStorage &kvbStorage) const;
};

}
}
}
