// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_HLF_HANDLER_H_
#define CONCORD_HLF_HANDLER_H_

#include <log4cplus/loggingmacros.h>
#include "common/concord_types.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "hlf/concord_hlf_chaincode_invoker.hpp"
#include "hlf/concord_hlf_kvb_storage.hpp"

namespace concord {
namespace hlf {
class HlfHandler {
 public:
  HlfHandler(ChaincodeInvoker*);
  ~HlfHandler();

  // APIs for set/revoke KvbStorageForHlf
  Blockchain::Status SetKvbHlfStoragePointer(
      concord::blockchain::hlf::KvbStorageForHlf*);
  Blockchain::Status RevokeKvbHlfStoragePointer();

  // APIs for kv service
  // return 0 if success
  std::string GetConcordKvService();
  Blockchain::Status PutState(std::string, std::string);
  std::string GetState(std::string);
  Blockchain::Status WriteBlock();

  // APIs for chaincode invoker
  Blockchain::Status InstallChaincode(std::string, std::string, std::string);
  Blockchain::Status InstantiateChaincode(std::string, std::string,
                                          std::string);
  Blockchain::Status UpgradeChaincode(std::string, std::string, std::string);
  Blockchain::Status InvokeChaincode(std::string, std::string);
  std::string QueryChaincode(std::string, std::string);

 private:
  concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage_ = nullptr;
  ChaincodeInvoker* chaincode_invoker_ = nullptr;
  log4cplus::Logger logger_;
};
}  // namespace hlf
}  // namespace concord
#endif
