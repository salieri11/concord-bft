// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_HLF_HANDLER_H_
#define CONCORD_HLF_HANDLER_H_

#include <log4cplus/loggingmacros.h>
#include "common/concord_types.hpp"
#include "consensus/status.hpp"
#include "hlf/chaincode_invoker.hpp"
#include "hlf/kvb_storage.hpp"

namespace concord {
namespace hlf {
class HlfHandler {
 public:
  HlfHandler(ChaincodeInvoker*);
  ~HlfHandler();

  // Constructor for copy
  HlfHandler(HlfHandler& hlf_handler) {
    logger_ = log4cplus::Logger::getInstance("com.vmware.concord.hlf.handler");
    chaincode_invoker_ = hlf_handler.chaincode_invoker_;
  }

  // APIs for set/revoke HlfKvbStorage
  concord::consensus::Status SetKvbHlfStoragePointer(
      concord::hlf::HlfKvbStorage* kvb_hlf_storage);

  concord::consensus::Status RevokeKvbHlfStoragePointer();

  // APIs for kv service
  std::string GetHlfKvService();

  concord::consensus::Status PutState(std::string key, std::string value);

  std::string GetState(const std::string& key);

  concord::consensus::Status WriteBlock();

  concord::consensus::Status AddTransaction(
      const com::vmware::concord::HlfRequest& hlf_request);

  // APIs for chaincode invoker
  concord::consensus::Status InstallChaincode(std::string name,
                                              std::string path,
                                              std::string version);
  concord::consensus::Status InstantiateChaincode(std::string name,
                                                  std::string path,
                                                  std::string version);
  concord::consensus::Status UpgradeChaincode(std::string name,
                                              std::string path,
                                              std::string version);
  concord::consensus::Status InvokeChaincode(std::string name,
                                             std::string input);
  std::string QueryChaincode(std::string name, std::string input);

 private:
  concord::hlf::HlfKvbStorage* kvb_hlf_storage_ = nullptr;
  ChaincodeInvoker* chaincode_invoker_ = nullptr;
  log4cplus::Logger logger_;
};
}  // namespace hlf
}  // namespace concord
#endif
