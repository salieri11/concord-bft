// Copyright 2018-2019 VMware, all rights reserved

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
  int setHlfPeerTool(std::string);
  int setHlfConcordKvServiceAddress(std::string);

  std::string getHlfPeerTool() const;
  std::string getHlfConcordKvServiceAddress() const;

  // general functions
  std::string systemCall(std::string);
  std::string constructCmdPrefix();

  // functions to call hlf peer:
  // TODO replace int with Blockchain::status
  int sendInvoke(std::string, std::string);
  std::string sendQuery(std::string, std::string);
  int sendInstall(std::string, std::string, std::string);
  int sendInstantiate(std::string, std::string, std::string);
  int sendUpgrade(std::string, std::string, std::string);

 private:
  // std::string peerConfig
  // set config according to the enviroment variable
  std::string hlfPeerTool;
  std::string hlfPeerToolConfig;
  std::string hlfPeerAddress;
  std::string hlfOrdererAddress;
  std::string hlfConcordKvServiceAddress;
  std::string hlfLocalMspId;
  std::string hlfLocalMspDir;

  // TODO(lukec)
  // channel
  log4cplus::Logger logger;
};
}  // namespace hlf
}  // namespace concord
