// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_CONSENSUS_HLF_CHAINCODE_INVOKER_H_
#define CONCORD_CONSENSUS_HLF_CHAINCODE_INVOKER_H_

#include <log4cplus/loggingmacros.h>
#include <iostream>
#include "config/configuration_manager.hpp"
#include "stdio.h"
#include "stdlib.h"

namespace concord {
namespace hlf {
class ChaincodeInvoker {
 public:
  ChaincodeInvoker(concord::config::ConcordConfiguration&);
  ChaincodeInvoker(std::string);
  ~ChaincodeInvoker();

  // functions to config command tool
  int SetHlfPeerTool(std::string);
  int SetHlfConcordKvServiceAddress(std::string);

  std::string GetHlfPeerTool() const;
  std::string GetHlfConcordKvServiceAddress() const;

  // general functions
  std::string SubProcess(std::string);
  std::string ConstructCmdPrefix();

  // functions to call hlf peer:
  int SendInvoke(std::string, std::string);
  std::string SendQuery(std::string, std::string);
  int SendInstall(std::string, std::string, std::string);
  int SendInstantiate(std::string, std::string, std::string);
  int SendUpgrade(std::string, std::string, std::string);

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
