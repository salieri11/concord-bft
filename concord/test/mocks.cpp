// Copyright 2020 VMware, all rights reserved

#include <daml/daml_kvb_commands_handler.hpp>

using namespace concord::kvbc;
using namespace concord::daml;
using namespace concordUtils;

namespace da_kvbc = com::digitalasset::kvbc;

using std::make_shared;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::DamlRequest;
using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;

using concord::thin_replica::SubBufferList;

using CryptoPP::AutoSeededRandomPool;

const auto LATEST_BLOCK_ID = BlockId{150};
const auto NUM_BLOCKS_TO_KEEP = 30;
const auto REPLICA_PRINCIPAL_ID_START = 0;
const auto CLIENT_PRINCIPAL_ID_START = 20000;

class NodeScopeSizer : public ConcordConfiguration::ScopeSizer {
 private:
  const size_t& numReplicas;

 public:
  NodeScopeSizer(const size_t& numReplicas) : numReplicas(numReplicas) {}
  virtual ~NodeScopeSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    output = numReplicas;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ReplicaScopeSizer : public ConcordConfiguration::ScopeSizer {
 public:
  virtual ~ReplicaScopeSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    output = 1;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ClientProxyScopeSizer : public ConcordConfiguration::ScopeSizer {
 private:
  const size_t& clientProxiesPerReplica;

 public:
  ClientProxyScopeSizer(const size_t& clientProxiesPerReplica)
      : clientProxiesPerReplica(clientProxiesPerReplica) {}
  virtual ~ClientProxyScopeSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    output = clientProxiesPerReplica;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

ConcordConfiguration TestConfiguration(
    std::size_t replica_count, std::size_t proxies_per_replica,
    std::uint64_t num_blocks_to_keep = 0,
    std::uint32_t duration_to_keep_minutes = 0, bool pruning_enabled = true,
    bool time_service_enabled = true,
    bool summaries_or_histograms_enabled = true) {
  ConcordConfiguration config;

  config.declareScope("node", "Node scope",
                      make_shared<NodeScopeSizer>(replica_count));
  config.declareParameter("pruning_enabled",
                          "A flag indicating if pruning is enabled");
  config.declareParameter("pruning_num_blocks_to_keep",
                          "Number of blocks to keep when pruning");
  config.declareParameter("pruning_duration_to_keep_minutes",
                          "Pruning duration to keep in minutes");

  config.declareParameter("FEATURE_time_service", "Enable time service");
  if (time_service_enabled) {
    config.loadValue("FEATURE_time_service", "true");
    config.declareParameter("time_verification",
                            "Time verification scheme to use.");
    config.loadValue("time_verification", "none");
  } else {
    config.loadValue("FEATURE_time_service", "false");
  }

  auto& nodeTemplate = config.subscope("node");
  nodeTemplate.declareScope("replica", "Replica scope",
                            make_shared<ReplicaScopeSizer>());
  nodeTemplate.declareScope(
      "client_proxy", "Client proxy scope",
      make_shared<ClientProxyScopeSizer>(proxies_per_replica));
  nodeTemplate.declareParameter("time_source_id", "Time Source ID");
  nodeTemplate.declareParameter("enable_histograms_or_summaries", "");

  auto& replicaTemplate = nodeTemplate.subscope("replica");
  replicaTemplate.declareParameter("private_key", "Private RSA key");
  replicaTemplate.declareParameter("public_key", "Public RSA key");
  replicaTemplate.declareParameter("principal_id", "Replica ID");

  auto& clientProxyTemplate = nodeTemplate.subscope("client_proxy");
  clientProxyTemplate.declareParameter("principal_id", "Client proxy ID");

  if (pruning_enabled) {
    config.loadValue("pruning_enabled", "true");
  }

  if (num_blocks_to_keep) {
    config.loadValue("pruning_num_blocks_to_keep",
                     std::to_string(num_blocks_to_keep));
  }

  if (duration_to_keep_minutes) {
    config.loadValue("pruning_duration_to_keep_minutes",
                     std::to_string(duration_to_keep_minutes));
  }

  config.instantiateScope("node");

  AutoSeededRandomPool random_pool;
  auto client_principal_id = CLIENT_PRINCIPAL_ID_START;
  for (auto i = 0; i < replica_count; ++i) {
    auto& node_scope = config.subscope("node", i);

    node_scope.loadValue("time_source_id", "time_source_" + std::to_string(i));
    node_scope.loadValue("enable_histograms_or_summaries",
                         summaries_or_histograms_enabled ? "true" : "false");

    node_scope.instantiateScope("replica");
    auto& replica_scope = node_scope.subscope("replica", 0);
    const auto rsaKeys = concord::config::generateRSAKeyPair(random_pool);
    replica_scope.loadValue("private_key", rsaKeys.first);
    replica_scope.loadValue("public_key", rsaKeys.second);
    replica_scope.loadValue("principal_id",
                            std::to_string(REPLICA_PRINCIPAL_ID_START + i));

    node_scope.instantiateScope("client_proxy");
    for (auto j = 0; j < proxies_per_replica; ++j) {
      auto& client_proxy_scope = node_scope.subscope("client_proxy", j);
      client_proxy_scope.loadValue("principal_id",
                                   std::to_string(client_principal_id++));
    }
  }

  return config;
}

const ConcordConfiguration& GetNodeConfig(const ConcordConfiguration& config,
                                          int index) {
  return config.subscope("node", index);
}

ConcordConfiguration EmptyConfiguration() { return ConcordConfiguration{}; }
