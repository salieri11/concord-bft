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

#include "config.h"
#include "Logger.hpp"
#include "kvstream.h"

using concord::config::CommConfig;
using concord::config::ConcordConfiguration;

namespace concord::op {

auto logger = logging::getLogger("concord.operator");

// Load the communication config from participant.config
config::CommConfig loadCommConfig(const ConcordConfiguration& conc_config) {
  CommConfig c;

  auto num_replicas = conc_config.getValue<uint32_t>("num_replicas");

  // Top level keys
  c.commType = conc_config.getValue<std::string>("comm_to_use");
  c.bufferLength =
      conc_config.getValue<uint32_t>("concord-bft_communication_buffer_length");
  c.maxServerId = num_replicas - 1;

  if (c.commType == "tls") {
    c.certificatesRootPath =
        conc_config.getValue<std::string>("tls_certificates_folder_path");
    c.cipherSuite = conc_config.getValue<std::string>("tls_cipher_suite_list");
  }

  // Keys under the participant_nodes/participant_node
  auto index = 0;
  const auto& node = conc_config.subscope("participant_nodes", index)
                         .subscope("participant_node", index);
  c.listenIp = node.getValue<std::string>("participant_node_host");
  c.listenPort = node.getValue<uint16_t>("operator_port");
  c.selfId = node.getValue<uint32_t>("operator_id");

  c.statusCallback = nullptr;

  // Replicas
  for (auto i = 0u; i < num_replicas; i++) {
    const auto& replica_conf =
        conc_config.subscope("node", i).subscope("replica", 0);
    const auto replica_id =
        replica_conf.getValue<decltype(c.nodes)::key_type>("principal_id");
    bft::communication::NodeInfo node_info;
    node_info.host =
        replica_conf.getValue<decltype(node_info.host)>("replica_host");
    node_info.port =
        replica_conf.getValue<decltype(node_info.port)>("replica_port");
    node_info.isReplica = true;
    c.nodes[replica_id] = node_info;
  }

  LOG_INFO(logger,
           "Loaded CommConfig: " << KVLOG(
               c.commType, c.bufferLength, c.maxServerId, c.listenIp,
               c.listenPort, c.selfId, c.certificatesRootPath, c.cipherSuite));

  return c;
}

bft::client::ClientConfig loadClientConfig(
    const ConcordConfiguration& conc_config,
    const config::CommConfig& comm_config) {
  bft::client::ClientConfig c;
  c.all_replicas = {};
  for (const auto& r : comm_config.nodes) {
    c.all_replicas.insert(
        bft::client::ReplicaId{static_cast<uint16_t>(r.first)});
  }
  c.c_val = conc_config.getValue<uint16_t>("c_val");
  c.f_val = conc_config.getValue<uint16_t>("f_val");
  c.id = bft::client::ClientId{static_cast<uint16_t>(comm_config.selfId)};
  c.retry_timeout_config.initial_retry_timeout = std::chrono::milliseconds(
      conc_config.getValue<uint32_t>("client_initial_retry_timeout_milli"));
  c.retry_timeout_config.max_retry_timeout = std::chrono::milliseconds(
      conc_config.getValue<uint32_t>("client_max_retry_timeout_milli"));
  c.retry_timeout_config.min_retry_timeout = std::chrono::milliseconds(
      conc_config.getValue<uint32_t>("client_min_retry_timeout_milli"));
  c.retry_timeout_config.number_of_standard_deviations_to_tolerate =
      conc_config.getValue<uint32_t>(
          "client_number_of_standard_deviations_to_tolerate");
  c.retry_timeout_config.samples_per_evaluation =
      conc_config.getValue<uint16_t>("client_samples_per_evaluation");
  c.retry_timeout_config.samples_until_reset =
      conc_config.getValue<int16_t>("client_samples_until_reset");

  auto r = c.retry_timeout_config;

  LOG_INFO(logger,
           "Loaded Client Config: " << KVLOG(
               c.c_val, c.f_val, c.id.val, r.initial_retry_timeout.count(),
               r.max_decreasing_factor, r.max_decreasing_factor,
               r.max_retry_timeout.count(), r.min_retry_timeout.count(),
               r.number_of_standard_deviations_to_tolerate,
               r.samples_per_evaluation, r.samples_until_reset));
  return c;
}

Config Config::parse(const char* path) {
  LOG_INFO(logger, "Parsing Config file: " << KVLOG(path));
  std::ifstream file(path);
  ConcordConfiguration conc_config;
  config::specifyExternalClientConfiguration(conc_config);
  conc_config.setConfigurationStateLabel(path);
  config::YAMLConfigurationInput yaml{file};
  yaml.parseInput();

  // Concord configuration is defined by recursive scopes. Each subscope that
  // can have multiple members is a "template" and must be instantiated before
  // the parameters in it can be accessed.
  yaml.loadConfiguration(
      conc_config,
      conc_config.begin(ConcordConfiguration::kIterateAllTemplateParameters),
      conc_config.end(ConcordConfiguration::kIterateAllTemplateParameters));
  conc_config.subscope("node").instantiateScope("replica");
  conc_config.instantiateScope("node");
  conc_config.subscope("participant_nodes")
      .instantiateScope("participant_node");
  conc_config.instantiateScope("participant_nodes");

  // Once all the necessary subscopes are instantiated we can load all
  // parameters.
  yaml.loadConfiguration(conc_config,
                         conc_config.begin(ConcordConfiguration::kIterateAll),
                         conc_config.end(ConcordConfiguration::kIterateAll));

  if (conc_config.validateAll() !=
      ConcordConfiguration::ParameterStatus::VALID) {
    LOG_ERROR(logger, "Configuration file validation failed: " << KVLOG(path));
    throw config::ConfigurationResourceNotFoundException{
        "Node configuration complete validation failed."};
  }

  auto config = Config{};
  config.comm_config = loadCommConfig(conc_config);
  config.client_config = loadClientConfig(conc_config, config.comm_config);
  return config;
}

}  // namespace concord::op
