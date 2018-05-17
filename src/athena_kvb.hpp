// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#include <log4cplus/loggingmacros.h>

#include "kvb/BlockchainInterfaces.h"
#include "kvb/slice.h"
#include "athena_evm.hpp"

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
};

}
}
}
