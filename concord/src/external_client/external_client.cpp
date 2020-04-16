// Copyright 2020 VMware, all rights reserved

#include "external_client.hpp"
#include <fstream>
#include <string>
#include <thread>
#include <utility>
#include "CommDefs.hpp"
#include "CommFactory.hpp"
#include "ICommunication.hpp"
#include "KVBCInterfaces.h"
#include "config/communication.hpp"
#include "config/configuration_manager.hpp"

namespace concord {
namespace external_client {

using bftEngine::ClientMsgFlag;
using bftEngine::CommFactory;
using bftEngine::ICommunication;
using bftEngine::PlainUdpConfig;
using bftEngine::TlsTcpConfig;
using concord::kvbc::ClientConfig;
using concord::kvbc::createClient;
using concord::kvbc::IClient;
using concordUtils::Status;
using config::CommConfig;
using config::ConcordConfiguration;
using config::ConfigurationException;
using config::ConfigurationPath;
using config::ConfigurationResourceNotFoundException;
using config::YAMLConfigurationInput;

namespace {

ConcordConfiguration::ParameterStatus ValidateNumClients(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failure_message, void* state) {
  if (const auto res = config::validateUInt(
          value, config, path, failure_message,
          const_cast<void*>(
              reinterpret_cast<const void*>(&config::kPositiveUInt16Limits)));
      res != ConcordConfiguration::ParameterStatus::VALID) {
    throw ConfigurationException{" Pool clients configuration failed"};
  }
  if (std::stoull(value) > 4096)
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
  config.declareParameter("num_clients_proxies",
                          "Total number of BFT clients in this deployment.");
  config.addValidator("num_clients_proxies", ValidateNumClients, nullptr);
  // Validation of f_val is based on c_val and num_replicas .
  config.declareParameter(
      "f_val",
      "F parameter to the SBFT algorithm, that is, the number of "
      "Byzantine-faulty replicas that can be tolerated in the system before "
      "safety guarantees are lost.");

  // Validation of c_val is based on f_val and num_replicas .
  config.declareParameter(
      "c_val",
      "C parameter to the SBFT algorithm, that is, the number of slow, "
      "crashed, or otherwise non-responsive replicas that can be tolerated "
      "before having to fall back on a slow path for consensus.");

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
      "Concord-BFT. Note that the capacity of these buffers may limit things "
      "like the maximum sizes of transactions or replies to requests that can "
      "be handled.",
      "64000");
  config.addValidator("concord-bft_communication_buffer_length",
                      config::validateUInt,
                      const_cast<void*>(reinterpret_cast<const void*>(
                          &config::kConcordBFTCommunicationBufferSizeLimits)));

  config.declareScope("node",
                      "Concord nodes that form the distributed system that "
                      "maintains a blockchain in Concord.",
                      config::sizeNodes, nullptr);

  config.declareParameter("client_min_retry_timeout_milli",
                          "Min retry timeout configuration");
  config.addValidator(
      "client_min_retry_timeout_milli", config::ValidateTimeOutMilli,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("client_max_retry_timeout_milli",
                          "Max retry timeout configuration");
  config.declareParameter("client_initial_retry_timeout_milli",
                          "The initial retry timeout configuration");
  config.declareParameter("client_sends_request_to_all_replicas_first_thresh",
                          "The first thresh configuration for client sends "
                          "requests to all replicas");
  config.addValidator(
      "client_sends_request_to_all_replicas_first_thresh", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("client_sends_request_to_all_replicas_period_thresh",
                          "The period thresh configuration for client sends "
                          "requests to all replicas");
  config.addValidator(
      "client_sends_request_to_all_replicas_period_thresh",
      config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));
  config.declareParameter("client_periodic_reset_thresh",
                          "The client periodic reset thresh configuration");
  config.addValidator(
      "client_periodic_reset_thresh", config::validateUInt,
      const_cast<void*>(reinterpret_cast<const void*>(&config::kUInt16Limits)));

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
  config.declareScope("client_proxies", "External client pool params replicas",
                      config::sizeExternalClients, nullptr);
  auto& client_proxies = config.subscope("client_proxies");
  client_proxies.declareScope("client", "One external client params",
                              config::sizeReplicas, nullptr);
  auto& client = client_proxies.subscope("client");
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

void ParseConfig(std::istream& config_stream,
                 config::ConcordConfiguration& config) {
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
  config.subscope("client_proxies").instantiateScope("client");
  config.instantiateScope("client_proxies");

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

}  // namespace

ConcordClient::ConcordClient(ConcordConfiguration config, int client_id) {
  CreateClient(config, client_id);
}

ConcordClient::ConcordClient(std::istream& config_stream) {
  Initialize(config_stream);
}

ConcordClient::ConcordClient(std::string_view config_file_path) {
  std::ifstream config_file;
  config_file.exceptions(std::ifstream::failbit | std::fstream::badbit);
  config_file.open(config_file_path.data());

  Initialize(config_file);
}

ConcordClient::~ConcordClient() noexcept {
  try {
    comm_->Stop();
  } catch (...) {
  }

  try {
    client_->stop();
  } catch (...) {
  }
}

void ConcordClient::SendRequestASync(const void* request,
                                     std::uint32_t request_size,
                                     ClientMsgFlag flags,
                                     std::chrono::milliseconds timeout_ms,
                                     std::uint32_t reply_size, void* out_reply,
                                     std::uint32_t* out_actual_reply_size,
                                     const std::string& correlation_id) {
  const auto status = client_->invokeCommandSynch(
      static_cast<const char*>(request), request_size, flags, timeout_ms,
      reply_size, static_cast<char*>(out_reply), out_actual_reply_size,
      correlation_id);

  if (!status.isOK()) {
    throw ClientRequestException{
        status, "ConcordClient failed to send a synchronous request"};
  }
}

void ConcordClient::CreateClient(const ConcordConfiguration& config,
                                 int client_id) {
  const auto num_replicas = config.getValue<std::uint16_t>("num_replicas");

  ClientConfig client_config;
  const auto& client_proxies_conf =
      config.subscope("client_proxies", client_id);
  const auto client_conf = client_proxies_conf.subscope("client", 0);

  client_config.fVal = config.getValue<decltype(client_config.fVal)>("f_val");
  client_config.cVal = config.getValue<decltype(client_config.cVal)>("c_val");
  client_config.clientId =
      client_conf.getValue<decltype(client_config.clientId)>("principal_id");

  CommConfig comm_config;
  comm_config.commType =
      config.getValue<decltype(comm_config.commType)>("comm_to_use");
  comm_config.listenIp =
      client_conf.getValue<decltype(comm_config.listenIp)>("client_host");
  comm_config.listenPort =
      client_conf.getValue<decltype(comm_config.listenPort)>("client_port");
  comm_config.bufferLength =
      config.getValue<decltype(comm_config.bufferLength)>(
          "concord-bft_communication_buffer_length");
  comm_config.selfId =
      client_conf.getValue<decltype(comm_config.selfId)>("principal_id");

  if (comm_config.commType == "tcp" || comm_config.commType == "tls") {
    comm_config.maxServerId = num_replicas - 1;
  }

  if (comm_config.commType == "tls") {
    comm_config.certificatesRootPath =
        config.getValue<decltype(comm_config.certificatesRootPath)>(
            "tls_certificates_folder_path");
    comm_config.cipherSuite =
        config.getValue<decltype(comm_config.cipherSuite)>(
            "tls_cipher_suite_list");
  }

  for (auto i = 0u; i < num_replicas; ++i) {
    const auto& node_conf = config.subscope("node", i);
    const auto replica_conf = node_conf.subscope("replica", 0);

    const auto replica_id =
        replica_conf.getValue<decltype(comm_config.nodes)::key_type>(
            "principal_id");

    NodeInfo node_info;
    node_info.host =
        replica_conf.getValue<decltype(node_info.host)>("replica_host");
    node_info.port =
        replica_conf.getValue<decltype(node_info.port)>("replica_port");
    node_info.isReplica = true;
    comm_config.nodes[replica_id] = node_info;
  }

  comm_config.statusCallback = nullptr;

  // Ensure exception safety by creating local pointers and only moving to
  // object members if construction and startup haven't thrown.
  auto comm = ToCommunication(comm_config);
  /*bftEngine::SimpleClientParams p;
  p.clientInitialRetryTimeoutMilli =
      config.getValue<std::uint16_t>("client_initial_retry_timeout_milli");
  p.clientMinRetryTimeoutMilli =
      config.getValue<std::uint16_t>("client_min_retry_timeout_milli");
  p.clientMaxRetryTimeoutMilli =
      config.getValue<std::uint16_t>("client_max_retry_timeout_milli");
  p.clientSendsRequestToAllReplicasFirstThresh = config.getValue<std::uint16_t>(
      "client_sends_request_to_all_replicas_first_thresh");
  p.clientSendsRequestToAllReplicasPeriodThresh =
      config.getValue<std::uint16_t>(
          "client_sends_request_to_all_replicas_period_thresh");
  p.clientPeriodicResetThresh =
      config.getValue<std::uint16_t>("client_periodic_reset_thresh");
  auto client = std::unique_ptr<IClient>{
      concord::kvbc::createClient(client_config, comm.get(), p)};*/
  auto client = std::unique_ptr<IClient>{
      concord::kvbc::createClient(client_config, comm.get())};

  client->start();

  comm_ = std::move(comm);
  client_ = std::move(client);
}

void ConcordClient::CreateClient(const ConcordConfiguration& config) {
  const auto num_replicas = config.getValue<std::uint16_t>("num_replicas");

  ClientConfig client_config;
  client_config.fVal = config.getValue<decltype(client_config.fVal)>("f_val");
  client_config.cVal = config.getValue<decltype(client_config.cVal)>("c_val");
  client_config.clientId =
      config.getValue<decltype(client_config.clientId)>("self_principal_id");

  CommConfig comm_config;
  comm_config.commType =
      config.getValue<decltype(comm_config.commType)>("comm_to_use");
  comm_config.listenIp =
      config.getValue<decltype(comm_config.listenIp)>("bind_ip");
  comm_config.listenPort =
      config.getValue<decltype(comm_config.listenPort)>("bind_port");
  comm_config.bufferLength =
      config.getValue<decltype(comm_config.bufferLength)>(
          "concord-bft_communication_buffer_length");
  comm_config.selfId =
      config.getValue<decltype(comm_config.selfId)>("self_principal_id");

  if (comm_config.commType == "tcp" || comm_config.commType == "tls") {
    comm_config.maxServerId = num_replicas - 1;
  }

  if (comm_config.commType == "tls") {
    comm_config.certificatesRootPath =
        config.getValue<decltype(comm_config.certificatesRootPath)>(
            "tls_certificates_folder_path");
    comm_config.cipherSuite =
        config.getValue<decltype(comm_config.cipherSuite)>(
            "tls_cipher_suite_list");
  }

  for (auto i = 0u; i < num_replicas; ++i) {
    const auto& node_conf = config.subscope("node", i);
    const auto replica_conf = node_conf.subscope("replica", 0);

    const auto replica_id =
        replica_conf.getValue<decltype(comm_config.nodes)::key_type>(
            "principal_id");

    NodeInfo node_info;
    node_info.host =
        replica_conf.getValue<decltype(node_info.host)>("replica_host");
    node_info.port =
        replica_conf.getValue<decltype(node_info.port)>("replica_port");
    node_info.isReplica = true;
    comm_config.nodes[replica_id] = node_info;
  }

  comm_config.statusCallback = nullptr;

  // Ensure exception safety by creating local pointers and only moving to
  // object members if construction and startup haven't thrown.
  auto comm = ToCommunication(comm_config);
  auto client =
      std::unique_ptr<IClient>{createClient(client_config, comm.get())};
  client->start();

  comm_ = std::move(comm);
  client_ = std::move(client);
}

void ConcordClient::Initialize(std::istream& config_stream) {
  ConcordConfiguration config;
  ParseConfig(config_stream, config);
  CreateClient(config);
}

}  // namespace external_client
}  // namespace concord
