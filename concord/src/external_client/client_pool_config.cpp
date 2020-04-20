// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License"). You may not use this product except in
// compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright notices and license terms. Your use of
// these subcomponents is subject to the terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#include "client_pool_config.hpp"
namespace concord {

namespace config_pool {
using bftEngine::CommFactory;
using bftEngine::ICommunication;
using bftEngine::PlainUdpConfig;
using bftEngine::TlsTcpConfig;
using config::CommConfig;
using config::ConcordConfiguration;
using config::ConfigurationException;
using config::ConfigurationPath;
using config::ConfigurationResourceNotFoundException;
using config::YAMLConfigurationInput;

config::ConcordConfiguration::ParameterStatus ValidateNumClients(
    const std::string& value, const config::ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failure_message, void* state) {
  if (const auto res = config::validateUInt(
          value, config, path, failure_message,
          const_cast<void*>(
              reinterpret_cast<const void*>(&config::kPositiveUInt16Limits)));
      res != ConcordConfiguration::ParameterStatus::VALID) {
    throw ConfigurationException{" Pool clients configuration failed"};
  }
  if (std::stoull(value) > MAX_EXTERNAL_CLIENTS)
    throw ConfigurationException{" Pool clients configuration failed"};
  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration::ParameterStatus ValidateNumReplicas(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failure_message, void* state) {
  if (const auto res = config::validateUInt(
          value, config, path, failure_message,
          const_cast<void*>(
              reinterpret_cast<const void*>(&config::kPositiveUInt16Limits)));
      res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }

  if (!config.hasValue<uint16_t>(F_VAL) || !config.hasValue<uint16_t>(C_VAL)) {
    if (failure_message) {
      *failure_message =
          "Cannot validate num_replicas: values for f_val and c_val are "
          "required to determine expected value of num_replicas.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }

  const auto expected = 3 * config.getValue<uint16_t>(F_VAL) +
                        2 * config.getValue<uint16_t>(C_VAL) + 1;

  if (std::stoll(value) != expected) {
    if (failure_message) {
      *failure_message =
          "Invalid value for num_replicas: " + value +
          "; num_replicas must be equal to 3 * f_val + 2 * c_val + 1.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration::ParameterStatus sizeExternalClients(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  assert(output);

  if (!(config.hasValue<uint16_t>("num_of_external_clients"))) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (!((config.validate("num_of_external_clients") ==
         ConcordConfiguration::ParameterStatus::VALID))) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }

  uint16_t num_clients_proxies =
      config.getValue<uint16_t>("num_of_external_clients");
  size_t numExternal = (size_t)num_clients_proxies;
  if (numExternal > (size_t)UINT16_MAX) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  *output = numExternal;
  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration::ParameterStatus ValidateTimeOutMilli(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failure_message, void* state) {
  if (const auto res = validateUInt(
          value, config, path, failure_message,
          const_cast<void*>(
              reinterpret_cast<const void*>(&config::kPositiveUInt16Limits)));
      res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }

  if (!config.hasValue<int>("client_initial_retry_timeout_milli") ||
      !config.hasValue<int>("client_max_retry_timeout_milli")) {
    if (failure_message) {
      *failure_message =
          "Cannot validate timeouts milli- some field not initialized";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }

  auto initial =
      config.getValue<uint16_t>("client_initial_retry_timeout_milli");
  auto max = config.getValue<uint16_t>("client_max_retry_timeout_milli");
  auto min = std::stoull(value);
  if (min < 1 || min > UINT_LEAST16_MAX)
    return ConcordConfiguration::ParameterStatus::INVALID;
  if (max < 1 || max > UINT_LEAST16_MAX)
    return ConcordConfiguration::ParameterStatus::INVALID;
  if (initial < min || initial > max) {
    if (failure_message) {
      *failure_message =
          "Invalid value , value has to be between min to max retry timeout "
          "milli";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

void SpecifyClientConfiguration(ConcordConfiguration& config) {
  config.declareParameter(NUM_EXTERNAL_CLIENTS,
                          "Total number of BFT clients in this deployment.");
  config.addValidator(NUM_EXTERNAL_CLIENTS, ValidateNumClients, nullptr);
  config.declareScope(EXTERNAL_CLIENTS, "External client pool params replicas",
                      sizeExternalClients, nullptr);
  auto& external_clients = config.subscope(EXTERNAL_CLIENTS);
  external_clients.declareScope(CLIENT, "One external client params",
                                config::sizeReplicas, nullptr);
  auto& client = external_clients.subscope(CLIENT);
  client.declareParameter(
      CLIENT_ID,
      "Unique ID number for this Concord-BFT replica. Concord-BFT considers "
      "replicas, clients and client proxies to be principals, each of which "
      "must have a unique ID.");

  client.declareParameter(CLIENT_PORT,
                          "Port number this replica can be reached at.");
  client.addValidator(
      CLIENT_PORT, config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
}

void SpecifyGeneralConfiguration(ConcordConfiguration& config) {
  // Validation of f_val is based on c_val and num_replicas .
  config.declareParameter(F_VAL, "F parameter to the SBFT algorithm.");

  // Validation of c_val is based on f_val and num_replicas .
  config.declareParameter(C_VAL, "C parameter to the SBFT algorithm.");

  // Validation is based on f_val and c_val .
  config.declareParameter(
      NUM_REPLICAS, "Total number of Concord replicas in this deployment.");
  config.addValidator(NUM_REPLICAS, ValidateNumReplicas, nullptr);

  //  Validation is done at construction of the client object.
  config.declareParameter(COMM_PROTOCOL, "Default communication module");

  config.declareParameter(CERT_FOLDER, "TLS certificates root folder path.");

  config.declareParameter(CIPHER_SUITE, "TLS cipher suite list to use.");

  config.declareParameter(
      COMM_BUFF_LEN,
      "Size of buffers to be used for messages exchanged with and within "
      "Concord-BFT.",
      "64000");
  config.addValidator(COMM_BUFF_LEN, config::validateUInt,
                      const_cast<void*>(reinterpret_cast<const void*>(
                          &config::kConcordBFTCommunicationBufferSizeLimits)));
}

void SpecifyReplicaConfiguration(ConcordConfiguration& config) {
  config.declareScope(NODE_VAR,
                      "Concord nodes that form the distributed system that "
                      "maintains a blockchain in Concord.",
                      config::sizeNodes, nullptr);

  auto& node = config.subscope(NODE_VAR);

  node.declareScope(
      REPLICA_VAR,
      "SBFT replicas, which serve as the core replicas for Byzantine fault "
      "tolerant consensus in a Concord deployment.",
      config::sizeReplicas, nullptr);
  auto& replica = node.subscope(REPLICA_VAR);

  replica.declareParameter(
      CLIENT_ID,
      "Unique ID number for this Concord-BFT replica. Concord-BFT considers "
      "replicas, clients and client proxies to be principals, each of which "
      "must have a unique ID.");

  replica.declareParameter(
      REPLICA_HOST, "IP address or host name this replica can be reached at.");

  replica.declareParameter(REPLICA_PORT,
                           "Port number this replica can be reached at.");
  replica.addValidator(
      REPLICA_PORT, config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
}

void SpecifySimpleClientParams(ConcordConfiguration& config) {
  config.declareParameter(MIN_RETRY_TIMEOUT, "Min retry timeout configuration",
                          "50");
  config.addValidator(
      MIN_RETRY_TIMEOUT, ValidateTimeOutMilli,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter(MAX_RETRY_TIMEOUT, "Max retry timeout configuration",
                          "1000");
  config.declareParameter(INITIAL_RETRY_TIMEOUT,
                          "The initial retry timeout configuration", "150");
  config.declareParameter(FIRST_THRESH,
                          "The first thresh configuration for client sends "
                          "requests to all replicas",
                          "4");
  config.addValidator(
      FIRST_THRESH, config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter(PERIODIC_THRESH,
                          "The period thresh configuration for client sends "
                          "requests to all replicas",
                          "2");
  config.addValidator(
      PERIODIC_THRESH, config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter(
      RESET_THRESH, "The client periodic reset thresh configuration", "30");
  config.addValidator(
      RESET_THRESH, config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
}

void ParseConfig(std::istream& config_stream,
                 config::ConcordConfiguration& config) {
  SpecifyGeneralConfiguration(config);
  SpecifyReplicaConfiguration(config);
  SpecifySimpleClientParams(config);
  SpecifyClientConfiguration(config);
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
  config.subscope(EXTERNAL_CLIENTS).instantiateScope(CLIENT);
  config.instantiateScope(EXTERNAL_CLIENTS);

  // Next, load all features.
  yaml.loadConfiguration(config,
                         config.begin(ConcordConfiguration::kIterateAll),
                         config.end(ConcordConfiguration::kIterateAll));

  if (config.validateAll() != ConcordConfiguration::ParameterStatus::VALID) {
    throw ConfigurationResourceNotFoundException{
        "Node configuration complete validation failed."};
  }
}

std::unique_ptr<ICommunication> ToCommunication(const CommConfig& comm_config) {
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

  throw std::invalid_argument{"Unknown communication module type: " +
                              comm_config.commType};
}

}  // namespace config_pool
}  // namespace concord