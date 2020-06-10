// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_HLF_CHAINCODE_INVOKER_H_
#define CONCORD_HLF_CHAINCODE_INVOKER_H_

#include <iostream>
#include "Logger.hpp"
#include "config/configuration_manager.hpp"
#include "status.hpp"
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
  concordUtils::Status SetHlfPeerTool(std::string tool_path);
  concordUtils::Status SetHlfKvServiceAddress(std::string address);

  std::string GetHlfPeerTool() const;
  std::string GetHlfKvServiceAddress() const;

  // general functions
  std::string SubProcess(std::string);
  std::string ConstructCmdPrefix();

  // functions to call hlf peer:
  std::string SendQuery(std::string, std::string);
  concordUtils::Status SendInvoke(std::string, std::string);
  concordUtils::Status SendInstall(std::string, std::string, std::string);
  concordUtils::Status SendInstantiate(std::string, std::string, std::string);
  concordUtils::Status SendUpgrade(std::string, std::string, std::string);

 private:
  std::string hlf_peer_tool_;
  std::string hlf_peer_tool_config_;
  std::string hlf_peer_address_;
  std::string hlf_orderer_address_;
  std::string hlf_kv_service_address_;
  std::string hlf_local_msp_id_;
  std::string hlf_local_msp_dir_;

  logging::Logger logger_;
};
}  // namespace hlf
}  // namespace concord

#endif  // CONCORD_CONSENSUS_HLF_CHAINCODE_INVOKER_H_
