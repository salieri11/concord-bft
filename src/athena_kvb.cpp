// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#include "kvb/BlockchainInterfaces.h"
#include "kvb/slice.h"
#include "athena_evm.hpp"
#include "athena_kvb.hpp"

using Blockchain::Slice;
using Blockchain::ILocalKeyValueStorageReadOnly;
using Blockchain::IBlocksAppender;

com::vmware::athena::KVBCommandsHandler::KVBCommandsHandler(EVM &athevm) :
   athevm_(athevm),
   logger(log4cplus::Logger::getInstance("com.vmware.athena"))
{
   // no other initialization necessary
}

com::vmware::athena::KVBCommandsHandler::~KVBCommandsHandler()
{
   // no other deinitialization necessary
}


bool com::vmware::athena::KVBCommandsHandler::executeCommand(
      const Slice command,
      const ILocalKeyValueStorageReadOnly &roStorage,
      IBlocksAppender &blockAppender,
      const size_t maxReplySize,
      char *outReply,
      size_t &outReplySize) const
{
   LOG4CPLUS_INFO(logger, "TODO: executeCommand!");
   return true;
}

bool com::vmware::athena::KVBCommandsHandler::executeReadOnlyCommand(
   const Slice command,
   const ILocalKeyValueStorageReadOnly &roStorage,
   const size_t maxReplySize,
   char *outReply,
   size_t &outReplySize) const
{
   LOG4CPLUS_INFO(logger, "TODO: executeReadOnlyCommand!");
   return true;
}
