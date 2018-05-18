// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#include <log4cplus/loggingmacros.h>

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

public:
   KVBCommandsHandler(EVM &athevm);
   ~KVBCommandsHandler();

   // ICommandsHandler
   virtual bool executeCommand(
      const Blockchain::Slice command,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender,
      const size_t maxReplySize,
      char *outReply,
      size_t &outReplySize) const override;

   virtual bool executeReadOnlyCommand(
      const Blockchain::Slice command,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      const size_t maxReplySize,
      char *outReply,
      size_t &outReplySize) const override;

   // Handlers
   void handle_transaction_request(
      AthenaRequest &athreq,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      AthenaResponse &athresp) const;
   void handle_eth_request(
      AthenaRequest &athreq,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender,
      AthenaResponse &athresp) const;
   void handle_eth_sendTransaction(
      AthenaRequest &athreq,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender,
      AthenaResponse &athresp) const;

   evm_result run_evm(
      const EthRequest &request,
      bool isTransaction,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender,
      evm_uint256be &txhash /* OUT */) const;
};

}
}
}
