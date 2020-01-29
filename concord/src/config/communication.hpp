// Copyright 2019 VMware, all rights reserved
//
// BFT Communication Configuration

#ifndef CONCORD_CONFIG_COMMUNICATION_H_
#define CONCORD_CONFIG_COMMUNICATION_H_

#include "communication/CommDefs.hpp"

#include <stdint.h>

#include <string>

namespace concord {
namespace config {

struct CommConfig {
  // common fields
  std::string listenIp;
  uint16_t listenPort;
  uint32_t bufferLength;
  std::unordered_map<NodeNum, NodeInfo> nodes;
  UPDATE_CONNECTIVITY_FN statusCallback;
  uint32_t selfId;

  // tcp
  uint32_t maxServerId;

  // tls (tcp fields should be set as well
  std::string certificatesRootPath;
  std::string cipherSuite;

  // possible values: "udp" and "tcp", lowercase
  std::string commType;
};

}  // namespace config
}  // namespace concord

#endif  // CONCORD_CONFIG_COMMUNICATION_H_
