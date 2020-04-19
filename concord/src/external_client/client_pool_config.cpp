//
// Created by lgal on 4/19/20.
//
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
  auto max_num_of_clients =
      config.getValue<uint16_t>("max_num_of_external_clients");
  if (std::stoull(value) > max_num_of_clients)
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

  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val")) {
    if (failure_message) {
      *failure_message =
          "Cannot validate num_replicas: values for f_val and c_val are "
          "required to determine expected value of num_replicas.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }

  const auto expected = 3 * config.getValue<uint16_t>("f_val") +
                        2 * config.getValue<uint16_t>("c_val") + 1;

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

void SpecifyClientConfiguration(ConcordConfiguration& config) {
  config.declareParameter("num_of_external_clients",
                          "Total number of BFT clients in this deployment.");
  config.addValidator("num_of_external_clients", ValidateNumClients, nullptr);
  config.declareParameter("min_num_of_external_clients",
                          "minimum number of external clients", "1");
  config.addValidator(
      "min_num_of_external_clients", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("max_num_of_external_clients",
                          "maximum number of external clients", "4096");
  config.addValidator(
      "max_num_of_external_clients", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareScope("external_clients",
                      "External client pool params replicas",
                      config::sizeExternalClients, nullptr);
  auto& external_clients = config.subscope("external_clients");
  external_clients.declareScope("client", "One external client params",
                                config::sizeReplicas, nullptr);
  auto& client = external_clients.subscope("client");
  client.declareParameter(
      "principal_id",
      "Unique ID number for this Concord-BFT replica. Concord-BFT considers "
      "replicas, clients and client proxies to be principals, each of which "
      "must have a unique ID.");

  client.declareParameter(
      "client_host", "IP address or host name this replica can be reached at.");

  client.declareParameter("client_port",
                          "Port number this replica can be reached at.");
  client.addValidator(
      "client_port", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
}

void SpecifyGeneralConfiguration(ConcordConfiguration& config) {
  // Validation of f_val is based on c_val and num_replicas .
  config.declareParameter("f_val", "F parameter to the SBFT algorithm.");

  // Validation of c_val is based on f_val and num_replicas .
  config.declareParameter("c_val", "C parameter to the SBFT algorithm.");

  // Validation is based on f_val and c_val .
  config.declareParameter(
      "num_replicas", "Total number of Concord replicas in this deployment.");
  config.addValidator("num_replicas", ValidateNumReplicas, nullptr);

  //  Validation is done at construction of the client object.
  config.declareParameter("comm_to_use", "Default communication module");

  config.declareParameter("tls_certificates_folder_path",
                          "TLS certificates root folder path.");

  config.declareParameter("tls_cipher_suite_list",
                          "TLS cipher suite list to use.");

  config.declareParameter(
      "concord-bft_communication_buffer_length",
      "Size of buffers to be used for messages exchanged with and within "
      "Concord-BFT.",
      "64000");
  config.addValidator("concord-bft_communication_buffer_length",
                      config::validateUInt,
                      const_cast<void*>(reinterpret_cast<const void*>(
                          &config::kConcordBFTCommunicationBufferSizeLimits)));
}

void SpecifyReplicaConfiguration(ConcordConfiguration& config) {
  config.declareScope("node",
                      "Concord nodes that form the distributed system that "
                      "maintains a blockchain in Concord.",
                      config::sizeNodes, nullptr);

  auto& node = config.subscope("node");

  node.declareScope(
      "replica",
      "SBFT replicas, which serve as the core replicas for Byzantine fault "
      "tolerant consensus in a Concord deployment.",
      config::sizeReplicas, nullptr);
  auto& replica = node.subscope("replica");

  replica.declareParameter(
      "principal_id",
      "Unique ID number for this Concord-BFT replica. Concord-BFT considers "
      "replicas, clients and client proxies to be principals, each of which "
      "must have a unique ID.");

  replica.declareParameter(
      "replica_host",
      "IP address or host name this replica can be reached at.");

  replica.declareParameter("replica_port",
                           "Port number this replica can be reached at.");
  replica.addValidator(
      "replica_port", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
}

void SpecifySimpleClientParams(ConcordConfiguration& config) {
  config.declareParameter("client_min_retry_timeout_milli",
                          "Min retry timeout configuration", "50");
  config.addValidator(
      "client_min_retry_timeout_milli", config::ValidateTimeOutMilli,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("client_max_retry_timeout_milli",
                          "Max retry timeout configuration", "1000");
  config.declareParameter("client_initial_retry_timeout_milli",
                          "The initial retry timeout configuration", "150");
  config.declareParameter("client_sends_request_to_all_replicas_first_thresh",
                          "The first thresh configuration for client sends "
                          "requests to all replicas",
                          "4");
  config.addValidator(
      "client_sends_request_to_all_replicas_first_thresh", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("client_sends_request_to_all_replicas_period_thresh",
                          "The period thresh configuration for client sends "
                          "requests to all replicas",
                          "2");
  config.addValidator(
      "client_sends_request_to_all_replicas_period_thresh",
      config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("client_periodic_reset_thresh",
                          "The client periodic reset thresh configuration",
                          "30");
  config.addValidator(
      "client_periodic_reset_thresh", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
}

void ParseConfig(std::istream& config_stream,
                 config::ConcordConfiguration& config) {
  SpecifyGeneralConfiguration(config);
  SpecifyReplicaConfiguration(config);
  SpecifySimpleClientParams(config);
  SpecifyClientConfiguration(config);
  config.setConfigurationStateLabel("concord_external_client");
  YAMLConfigurationInput yaml{config_stream};

  yaml.parseInput();

  // First, load all template parameters.
  yaml.loadConfiguration(
      config, config.begin(ConcordConfiguration::kIterateAllTemplateParameters),
      config.end(ConcordConfiguration::kIterateAllTemplateParameters));

  // Instantiate the replica scope before node scope.
  config.subscope("node").instantiateScope("replica");
  config.instantiateScope("node");
  config.subscope("external_clients").instantiateScope("client");
  config.instantiateScope("external_clients");

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