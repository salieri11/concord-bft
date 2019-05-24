// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_HLF_CHAINCODE_INVOKER_H_
#define CONCORD_HLF_CHAINCODE_INVOKER_H_

#include <log4cplus/loggingmacros.h>
#include <iostream>
#include "config/configuration_manager.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "stdio.h"
#include "stdlib.h"

namespace concord {
namespace hlf {
class ChaincodeInvoker {
 public:
  ChaincodeInvoker(concord::config::ConcordConfiguration& config);
  ChaincodeInvoker(std::string path);
  ~ChaincodeInvoker();

  // functions to config command tool
  Blockchain::Status SetHlfPeerTool(std::string tool_path);
  Blockchain::Status SetHlfKvServiceAddress(std::string address);

  std::string GetHlfPeerTool() const;
  std::string GetHlfKvServiceAddress() const;

  // general functions
  std::string SubProcess(std::string);
  std::string ConstructCmdPrefix();

  // functions to call hlf peer:
  std::string SendQuery(std::string, std::string);
  Blockchain::Status SendInvoke(std::string, std::string);
  Blockchain::Status SendInstall(std::string, std::string, std::string);
  Blockchain::Status SendInstantiate(std::string, std::string, std::string);
  Blockchain::Status SendUpgrade(std::string, std::string, std::string);

 private:
  std::string hlf_peer_tool_;
  std::string hlf_peer_tool_config_;
  std::string hlf_peer_address_;
  std::string hlf_orderer_address_;
  std::string hlf_concord_kv_service_address_;
  std::string hlf_local_msp_id_;
  std::string hlf_local_msp_dir_;

  log4cplus::Logger logger_;
};
}  // namespace hlf
}  // namespace concord

#endif  // CONCORD_CONSENSUS_HLF_CHAINCODE_INVOKER_H_
