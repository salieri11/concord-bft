// Copyright 2018-2019 VMware, all rights reserved

// functions:
// put_state
// get_state
// write_block(reuse the evm write block)

#ifndef CONCORD_HLF_HANDLER_HPP
#define CONCORD_HLF_HANDLER_HPP

#include <log4cplus/loggingmacros.h>
#include <cstring>
#include "common/concord_types.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "hlf/concord_hlf_chaincode_invoker.hpp"
#include "hlf/concord_hlf_kvb_storage.hpp"

namespace concord {
namespace hlf {
class HlfHandler {
 public:
  HlfHandler(ChaincodeInvoker *);
  ~HlfHandler();

  // APIs for set/revoke KVBHlfStorage
  Blockchain::Status setKVBHlfStoragePointer(
      concord::blockchain::hlf::KVBHlfStorage *);
  Blockchain::Status revokeKVBHlfStoragePointer();

  // APIs for kv service
  // return 0 if success
  string getConcordKvService();
  Blockchain::Status putState(string, string);
  string getState(string);
  Blockchain::Status writeBlock();

  // APIs for chaincode invoker
  Blockchain::Status installChaincode(string, string, string);
  Blockchain::Status instantiateChaincode(string, string, string);
  Blockchain::Status upgradeChaincode(string, string, string);
  Blockchain::Status invokeChaincode(string, string);
  string queryChaincode(string, string);

 private:
  concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage_ = nullptr;
  ChaincodeInvoker *chaincodeInvoker_ = nullptr;
  log4cplus::Logger logger_;
};
}  // namespace hlf
}  // namespace concord
#endif
