// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#include "client_pool_config.hpp"

namespace concord::config_pool {
using bft::communication::CommFactory;
using bft::communication::ICommunication;
using bft::communication::PlainUdpConfig;
using bft::communication::TlsTcpConfig;
using config::CommConfig;
using config::ConcordConfiguration;
using config::ConfigurationResourceNotFoundException;
using config::kConcordExternalClientConfigurationStateLabel;
using config::YAMLConfigurationInput;

ClientPoolConfig::ClientPoolConfig() {
  logger_ = logging::getLogger("com.vmware.external_client_pool");
}
void ClientPoolConfig::ParseConfig(std::istream& config_stream,
                                   config::ConcordConfiguration& config) {
  config::specifyExternalClientConfiguration(config);
  config.setConfigurationStateLabel(FILE_NAME);
  YAMLConfigurationInput yaml{config_stream};
  yaml.parseInput();
  // First, load all template parameters.
  yaml.loadConfiguration(
      config, config.begin(ConcordConfiguration::kIterateAllTemplateParameters),
      config.end(ConcordConfiguration::kIterateAllTemplateParameters));

  // Instantiate the replica scope before node scope.
  config.subscope(NODE_VAR).instantiateScope(REPLICA_VAR);
  config.instantiateScope(NODE_VAR);
  config.subscope(PARTICIPANT_NODES)
      .subscope(PARTICIPANT_NODE)
      .subscope(EXTERNAL_CLIENTS)
      .instantiateScope(CLIENT);
  config.subscope(PARTICIPANT_NODES)
      .subscope(PARTICIPANT_NODE)
      .instantiateScope(EXTERNAL_CLIENTS);
  config.subscope(PARTICIPANT_NODES).instantiateScope(PARTICIPANT_NODE);
  config.instantiateScope(PARTICIPANT_NODES);

  // Next, load all features.
  yaml.loadConfiguration(config,
                         config.begin(ConcordConfiguration::kIterateAll),
                         config.end(ConcordConfiguration::kIterateAll));

  if (config.validateAll() != ConcordConfiguration::ParameterStatus::VALID) {
    LOG_ERROR(logger_,
              "Configuration file=" << config.getValue<std::string>(FILE_NAME)
                                    << " validation failed");
    throw ConfigurationResourceNotFoundException{
        "Node configuration complete validation failed."};
  }
}

std::unique_ptr<ICommunication> ClientPoolConfig::ToCommunication(
    const CommConfig& comm_config) {
  LOG_DEBUG(logger_, "client_id=" << comm_config.selfId
                                  << " initiating communication module of type="
                                  << comm_config.commType);
  if (comm_config.commType == "tls") {
    const auto tls_config =
        TlsTcpConfig{comm_config.listenIp,
                     comm_config.listenPort,
                     comm_config.bufferLength,
                     comm_config.nodes,
                     static_cast<std::int32_t>(comm_config.maxServerId),
                     comm_config.selfId,
                     comm_config.certificatesRootPath,
                     comm_config.cipherSuite,
                     comm_config.statusCallback};
    return std::unique_ptr<ICommunication>{CommFactory::create(tls_config)};
  } else if (comm_config.commType == "udp") {
    const auto udp_config =
        PlainUdpConfig{comm_config.listenIp,     comm_config.listenPort,
                       comm_config.bufferLength, comm_config.nodes,
                       comm_config.selfId,       comm_config.statusCallback};
    return std::unique_ptr<ICommunication>{CommFactory::create(udp_config)};
  }
  LOG_ERROR(logger_, comm_config.commType
                         << " is not a supported communication module type");
  throw std::invalid_argument{"Unknown communication module type=" +
                              comm_config.commType};
}

}  // namespace concord::config_pool
