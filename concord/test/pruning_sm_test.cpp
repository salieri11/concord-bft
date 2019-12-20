// Copyright 2018-2019 VMware, all rights reserved

#include "blockchain/db_interfaces.h"
#include "config/configuration_manager.hpp"
#include "gtest/gtest.h"
#include "pruning/kvb_pruning_sm.hpp"
#include "pruning/pruning_exception.hpp"
#include "pruning/rsa_pruning_signer.hpp"
#include "pruning/rsa_pruning_verifier.hpp"

#include <cryptopp/osrng.h>
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>

#include <opentracing/tracer.h>

#include <cassert>
#include <cstdint>
#include <exception>
#include <memory>
#include <set>
#include <string>
#include <vector>

using namespace concord::config;
using namespace concordUtils;
using namespace concord::pruning;
using namespace concord::storage::blockchain;
using namespace com::vmware::concord;

using CryptoPP::AutoSeededRandomPool;

namespace {

const auto LATEST_BLOCK_ID = BlockId{150};
const auto NUM_BLOCKS_TO_KEEP = 30;
const auto REPLICA_PRINCIPAL_ID_START = 0;
const auto CLIENT_PRINCIPAL_ID_START = 20000;

class TestStorage : public ILocalKeyValueStorageReadOnly {
 public:
  Status get(const Key&, Value&) const override {
    EXPECT_TRUE(false) << "get(key) should not be called by this test";
    return Status::IllegalOperation("get() not supported in test mode");
  }

  Status get(BlockId, const Sliver&, Sliver&, BlockId&) const override {
    EXPECT_TRUE(false) << "get(version, key) should not be called by this test";
    return Status::IllegalOperation("get() not supported in test mode");
  }

  BlockId getLastBlock() const override { return LATEST_BLOCK_ID; }

  Status getBlockData(BlockId, SetOfKeyValuePairs&) const override {
    EXPECT_TRUE(false) << "getBlockData() should not be called by this test";
    return Status::IllegalOperation(
        "getBlockData() not supported in test mode");
  }

  Status mayHaveConflictBetween(const Sliver&, BlockId, BlockId,
                                bool&) const override {
    EXPECT_TRUE(false)
        << "mayHaveConflictBetween() should not be called by this test";
    return Status::IllegalOperation(
        "mayHaveConflictBetween() not supported in test mode");
  }

  ILocalKeyValueStorageReadOnlyIterator* getSnapIterator() const override {
    EXPECT_TRUE(false) << "getSnapIterator() should not be called by this test";
    return nullptr;
  }

  Status freeSnapIterator(
      ILocalKeyValueStorageReadOnlyIterator*) const override {
    EXPECT_TRUE(false)
        << "freeSnapIterator() should not be called by this test";
    return Status::IllegalOperation(
        "freeSnapIterator() not supported in test mode");
  }

  void monitor() const override {
    EXPECT_TRUE(false) << "monitor() should not be called by this test";
  }
};

ConcordConfiguration::ParameterStatus NodeScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  *output = *reinterpret_cast<const int*>(state);
  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration::ParameterStatus ReplicaScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  *output = 1;
  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration::ParameterStatus ClientProxyScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  *output = *reinterpret_cast<const int*>(state);
  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration TestConfiguration(std::size_t replica_count,
                                       std::size_t proxies_per_replica,
                                       std::uint64_t num_blocks_to_keep = 0) {
  ConcordConfiguration config;

  config.declareScope("node", "Node scope", NodeScopeSizer, &replica_count);
  config.declareParameter("pruning_num_blocks_to_keep",
                          "Number of blocks to keep when pruning");
  auto& nodeTemplate = config.subscope("node");
  nodeTemplate.declareScope("replica", "Replica scope", ReplicaScopeSizer,
                            nullptr);
  nodeTemplate.declareScope("client_proxy", "Client proxy scope",
                            ClientProxyScopeSizer, &proxies_per_replica);

  auto& replicaTemplate = nodeTemplate.subscope("replica");
  replicaTemplate.declareParameter("private_key", "Private RSA key");
  replicaTemplate.declareParameter("public_key", "Public RSA key");
  replicaTemplate.declareParameter("principal_id", "Replica ID");

  auto& clientProxyTemplate = nodeTemplate.subscope("client_proxy");
  clientProxyTemplate.declareParameter("principal_id", "Client proxy ID");

  if (num_blocks_to_keep) {
    config.loadValue("pruning_num_blocks_to_keep",
                     std::to_string(num_blocks_to_keep));
  }

  config.instantiateScope("node");

  AutoSeededRandomPool random_pool;
  auto client_principal_id = CLIENT_PRINCIPAL_ID_START;
  for (auto i = 0; i < replica_count; ++i) {
    auto& node_scope = config.subscope("node", i);

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

ConcordConfiguration EmptyConfiguration() { return ConcordConfiguration{}; }

const ConcordConfiguration& GetNodeConfig(const ConcordConfiguration& config,
                                          int index) {
  return config.subscope("node", index);
}

using ReplicaIDs = std::set<std::uint64_t>;

ReplicaIDs GetReplicaIDs(const ConcordConfiguration& config) {
  ReplicaIDs ret;

  for (auto node_id = 0u; node_id < config.scopeSize("node"); ++node_id) {
    const auto& node_config = config.subscope("node", node_id);
    const auto& replica_config = node_config.subscope("replica", 0);

    const auto ins_res =
        ret.insert(replica_config.getValue<std::uint64_t>("principal_id"));
    assert(ins_res.second);
  }

  return ret;
}

const ConcordConfiguration& NodeConfig(const ConcordConfiguration& config,
                                       std::size_t idx) {
  return config.subscope("node", idx);
}

auto test_span = opentracing::Tracer::Global() -> StartSpan("pruning_sm_test");

TEST(pruning_sm_test, sign_verify_parse_configuration) {
  {
    EXPECT_ANY_THROW({
      const auto signer = RSAPruningSigner{EmptyConfiguration()};
    }) << "RSAPruningSigner cannot be constructed with no replicas in the "
          "configuraiton";
  }

  {
    EXPECT_THROW(
        { const auto verifier = RSAPruningVerifier{EmptyConfiguration()}; },
        PruningConfigurationException)
        << "RSAPruningVerifier cannot be constructed with no replicas in the "
           "configuraiton";
  }

  {
    const auto replicas = 0;
    const auto client_proxies = 4;
    const auto config = TestConfiguration(replicas, client_proxies);
    EXPECT_THROW({ const auto verifier = RSAPruningVerifier{config}; },
                 PruningConfigurationException)
        << "RSAPruningVerifier cannot be constructed with no replicas in the "
           "configuraiton";
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 4;
    auto config = TestConfiguration(replicas, client_proxies);
    config.subscope("node", 0)
        .subscope("replica", 0)
        .loadValue("principal_id",
                   std::to_string(REPLICA_PRINCIPAL_ID_START + 1));
    EXPECT_THROW({ const auto verifier = RSAPruningVerifier{config}; },
                 PruningConfigurationException)
        << "RSAPruningVerifier cannot be constructed with duplicate replica "
           "principal_ids";
  }
}

TEST(pruning_sm_test, sm_parse_configuration) {
  TestStorage s;

  {
    const auto replicas = 4;
    const auto client_proxies = 4;
    const auto c = TestConfiguration(replicas, client_proxies);
    const auto& n = GetNodeConfig(c, 1);
    EXPECT_NO_THROW({ KVBPruningSM sm(s, c, n); });
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 8;
    const auto c = TestConfiguration(replicas, client_proxies);
    const auto& n = GetNodeConfig(c, 1);
    EXPECT_NO_THROW({ KVBPruningSM sm(s, c, n); });
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 2;
    const auto c = TestConfiguration(replicas, client_proxies);
    const auto& n = GetNodeConfig(c, 1);
    EXPECT_NO_THROW({ KVBPruningSM sm(s, c, n); });
  }

  {
    const auto replicas = 7;
    const auto client_proxies = 3;
    const auto c = TestConfiguration(replicas, client_proxies);
    const auto& n = GetNodeConfig(c, 1);
    EXPECT_NO_THROW({ KVBPruningSM sm(s, c, n); });
  }

  {
    const auto replicas = 4;
    const auto client_proxies = 0;
    const auto c = TestConfiguration(replicas, client_proxies);
    const auto& n = GetNodeConfig(c, 1);
    EXPECT_NO_THROW({ KVBPruningSM sm(s, c, n); });
  }
}

TEST(pruning_sm_test, sign_verify_correct) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto sending_id = 1;
  const auto config = TestConfiguration(replica_count, client_proxy_count);
  const auto verifier = RSAPruningVerifier{config};
  std::vector<RSAPruningSigner> signers;
  for (auto i = 0; i < replica_count; ++i) {
    signers.push_back(RSAPruningSigner{NodeConfig(config, i)});
  }

  // Sign and verify a LatestPrunableBlock message.
  {
    LatestPrunableBlock block;
    block.set_replica(REPLICA_PRINCIPAL_ID_START + sending_id);
    block.set_block_id(LATEST_BLOCK_ID);
    signers[sending_id].Sign(block);

    EXPECT_TRUE(verifier.Verify(block));
  }

  // Sign and verify a PruneRequest message.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START +
                       client_proxy_count * sending_id);
    for (auto i = 0; i < replica_count; ++i) {
      auto block = request.add_latest_prunable_block();
      block->set_replica(REPLICA_PRINCIPAL_ID_START + i);
      block->set_block_id(LATEST_BLOCK_ID);
      signers[i].Sign(*block);
    }
    signers[sending_id].Sign(request);

    EXPECT_TRUE(verifier.Verify(request));
  }
}

TEST(pruning_sm_test, sign_malformed_messages) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto sending_id = 1;
  const auto config = TestConfiguration(replica_count, client_proxy_count);
  const auto verifier = RSAPruningVerifier{config};
  std::vector<RSAPruningSigner> signers;
  for (auto i = 0; i < replica_count; ++i) {
    signers.push_back(RSAPruningSigner{NodeConfig(config, i)});
  }

  // Sign an empty LatestPrunableBlock.
  {
    LatestPrunableBlock block;
    EXPECT_THROW(signers[sending_id].Sign(block), PruningRuntimeException)
        << "RSAPruningSigner fails to sign an empty LatestPrunableBlock "
           "message";
  }

  // Omit the block ID in LatestPrunableBlock and sign.
  {
    LatestPrunableBlock block;
    block.set_replica(REPLICA_PRINCIPAL_ID_START + sending_id);
    EXPECT_THROW(signers[sending_id].Sign(block), PruningRuntimeException)
        << "RSAPruningSigner fails to sign a malformed(missing block ID) "
           "LatestPrunableBlock message";
  }

  // Omit the replica ID in LatestPrunableBlock and sign.
  {
    LatestPrunableBlock block;
    block.set_block_id(LATEST_BLOCK_ID);
    EXPECT_THROW(signers[sending_id].Sign(block), PruningRuntimeException)
        << "RSAPruningSigner fails to sign a malformed(missing replica ID) "
           "LatestPrunableBlock message";
  }

  // Sign an empty PruneRequest.
  {
    PruneRequest request;
    EXPECT_THROW(signers[sending_id].Sign(request), PruningRuntimeException)
        << "RSAPruningSigner fails to sign an empty PruneRequest message";
  }

  // Omit the sender in PruneRequest and sign.
  {
    PruneRequest request;
    auto block = request.add_latest_prunable_block();
    block->set_replica(REPLICA_PRINCIPAL_ID_START + sending_id);
    block->set_block_id(LATEST_BLOCK_ID);
    signers[sending_id].Sign(*block);
    EXPECT_THROW(signers[sending_id].Sign(request), PruningRuntimeException)
        << "RSAPruningSigner fails to sign a malformed(missing sender ID) "
           "PruneRequest message";
  }

  // Omit latest blocks in PruneRequest and sign.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START + sending_id);
    EXPECT_THROW(signers[sending_id].Sign(request), PruningRuntimeException)
        << "RSAPruningSigner fails to sign a malformed(missing latest blocks) "
           "PruneRequest message";
  }
}

TEST(pruning_sm_test, verify_malformed_messages) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto sending_id = 1;
  const auto config = TestConfiguration(replica_count, client_proxy_count);
  const auto verifier = RSAPruningVerifier{config};
  std::vector<RSAPruningSigner> signers;
  for (auto i = 0; i < replica_count; ++i) {
    signers.push_back(RSAPruningSigner{NodeConfig(config, i)});
  }

  // Break verification of LatestPrunableBlock messages.
  {
    LatestPrunableBlock block;
    block.set_replica(REPLICA_PRINCIPAL_ID_START + sending_id);
    block.set_block_id(LATEST_BLOCK_ID);
    signers[sending_id].Sign(block);

    // Change the replica ID after signing.
    block.set_replica(REPLICA_PRINCIPAL_ID_START + sending_id + 1);
    EXPECT_FALSE(verifier.Verify(block));

    // Make sure it works with the correct replica ID.
    block.set_replica(REPLICA_PRINCIPAL_ID_START + sending_id);
    EXPECT_TRUE(verifier.Verify(block));

    // Change the block ID after signing.
    block.set_block_id(LATEST_BLOCK_ID + 1);
    EXPECT_FALSE(verifier.Verify(block));

    // Make sure it works with the correct block ID.
    block.set_block_id(LATEST_BLOCK_ID);
    EXPECT_TRUE(verifier.Verify(block));

    // Change a single byte from the signature and make sure it doesn't verify.
    block.mutable_signature()->at(0) += 1;
    EXPECT_FALSE(verifier.Verify(block));
  }

  // Change the sender in PruneRequest after signing and verify.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START +
                       client_proxy_count * sending_id);
    for (auto i = 0; i < replica_count; ++i) {
      auto block = request.add_latest_prunable_block();
      block->set_replica(REPLICA_PRINCIPAL_ID_START + i);
      block->set_block_id(LATEST_BLOCK_ID);
      signers[i].Sign(*block);
    }
    signers[sending_id].Sign(request);
    request.set_sender(request.sender() + 1);

    EXPECT_FALSE(verifier.Verify(request));
  }

  // Change the signature in PruneRequest and verify.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START +
                       client_proxy_count * sending_id);
    for (auto i = 0; i < replica_count; ++i) {
      auto block = request.add_latest_prunable_block();
      block->set_replica(REPLICA_PRINCIPAL_ID_START + i);
      block->set_block_id(LATEST_BLOCK_ID);
      signers[i].Sign(*block);
    }
    signers[sending_id].Sign(request);
    request.mutable_signature()->at(0) += 1;

    EXPECT_FALSE(verifier.Verify(request));
  }

  // Verify a PruneRequest with replica_count - 1 latest prunable blocks.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START +
                       client_proxy_count * sending_id);
    for (auto i = 0; i < replica_count - 1; ++i) {
      auto block = request.add_latest_prunable_block();
      block->set_replica(REPLICA_PRINCIPAL_ID_START + i);
      block->set_block_id(LATEST_BLOCK_ID);
      signers[i].Sign(*block);
    }
    signers[sending_id].Sign(request);

    EXPECT_FALSE(verifier.Verify(request));
  }

  // Change replica in a single latest prunable block message after signing it
  // and before signing the PruneRequest message and then and verify.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START +
                       client_proxy_count * sending_id);
    for (auto i = 0; i < replica_count; ++i) {
      auto block = request.add_latest_prunable_block();
      block->set_replica(REPLICA_PRINCIPAL_ID_START + i);
      block->set_block_id(LATEST_BLOCK_ID);
      signers[i].Sign(*block);
    }
    request.mutable_latest_prunable_block(0)->set_replica(
        REPLICA_PRINCIPAL_ID_START + replica_count + 8);
    signers[sending_id].Sign(request);

    EXPECT_FALSE(verifier.Verify(request));
  }

  // Change replica in a single latest prunable block message after signing it
  // and after signing the PruneRequest message and then and verify.
  {
    PruneRequest request;
    request.set_sender(CLIENT_PRINCIPAL_ID_START +
                       client_proxy_count * sending_id);
    for (auto i = 0; i < replica_count; ++i) {
      auto block = request.add_latest_prunable_block();
      block->set_replica(REPLICA_PRINCIPAL_ID_START + i);
      block->set_block_id(LATEST_BLOCK_ID);
      signers[i].Sign(*block);
    }
    signers[sending_id].Sign(request);
    request.mutable_latest_prunable_block(0)->set_replica(
        REPLICA_PRINCIPAL_ID_START + replica_count + 8);

    EXPECT_FALSE(verifier.Verify(request));
  }
}

TEST(pruning_sm_test, sm_handle_latest_prunable_request) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto config =
      TestConfiguration(replica_count, client_proxy_count, NUM_BLOCKS_TO_KEEP);
  TestStorage storage;
  const auto verifier = RSAPruningVerifier{config};

  const auto sm =
      KVBPruningSM{storage, config, GetNodeConfig(config, replica_idx)};

  ConcordRequest req;
  ConcordResponse resp;
  req.mutable_latest_prunable_block_request();
  sm.Handle(req, resp, true, *test_span);

  ASSERT_TRUE(resp.has_latest_prunable_block_response());
  const auto& latest_prunable_resp = resp.latest_prunable_block_response();
  ASSERT_EQ(latest_prunable_resp.block_size(), 1);
  const auto& block = latest_prunable_resp.block(0);
  ASSERT_EQ(block.replica(), replica_idx);
  ASSERT_EQ(block.block_id(), LATEST_BLOCK_ID - NUM_BLOCKS_TO_KEEP);
  ASSERT_TRUE(verifier.Verify(block));
}

TEST(pruning_sm_test, sm_latest_prunable_request_big_num_blocks_to_keep) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto config = TestConfiguration(replica_count, client_proxy_count,
                                        LATEST_BLOCK_ID + 42);
  TestStorage storage;
  const auto verifier = RSAPruningVerifier{config};

  const auto sm =
      KVBPruningSM{storage, config, GetNodeConfig(config, replica_idx)};

  ConcordRequest req;
  ConcordResponse resp;
  req.mutable_latest_prunable_block_request();

  sm.Handle(req, resp, true, *test_span);

  ASSERT_TRUE(resp.has_latest_prunable_block_response());
  const auto& latest_prunable_resp = resp.latest_prunable_block_response();
  ASSERT_EQ(latest_prunable_resp.block_size(), 1);
  const auto& block = latest_prunable_resp.block(0);
  ASSERT_EQ(block.replica(), replica_idx);
  // Verify that the returned block ID is 0 when pruning_num_blocks_to_keep is
  // bigger than the latest block ID.
  ASSERT_EQ(block.block_id(), 0);
  ASSERT_TRUE(verifier.Verify(block));
}

ConcordRequest ConstructPruneRequest(const ConcordConfiguration& config,
                                     std::size_t client_idx) {
  ConcordRequest req;
  auto prune_req = req.mutable_prune_request();

  const auto sender = config.subscope("node", client_idx)
                          .subscope("client_proxy", 0)
                          .getValue<std::uint64_t>("principal_id");
  prune_req->set_sender(sender);

  for (auto i = 0u; i < config.scopeSize("node"); ++i) {
    const auto& node_config = config.subscope("node", i);
    const auto& replica_config = node_config.subscope("replica", 0);

    auto latest_block = prune_req->add_latest_prunable_block();
    latest_block->set_replica(
        replica_config.getValue<std::uint64_t>("principal_id"));
    // Send different block IDs.
    latest_block->set_block_id(LATEST_BLOCK_ID + i);

    const auto block_signer = RSAPruningSigner{GetNodeConfig(config, i)};
    block_signer.Sign(*latest_block);
  }

  const auto req_signer = RSAPruningSigner{GetNodeConfig(config, client_idx)};
  req_signer.Sign(*prune_req);

  return req;
}

TEST(pruning_sm_test, sm_handle_correct_prune_request) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto client_idx = 0;
  const auto config =
      TestConfiguration(replica_count, client_proxy_count, NUM_BLOCKS_TO_KEEP);
  TestStorage storage;

  const auto sm =
      KVBPruningSM{storage, config, GetNodeConfig(config, replica_idx)};

  const auto req = ConstructPruneRequest(config, client_idx);
  ConcordResponse resp;

  sm.Handle(req, resp, false, *test_span);

  ASSERT_TRUE(resp.has_prune_response());
  ASSERT_TRUE(resp.prune_response().has_ok());
  EXPECT_TRUE(resp.prune_response().ok());
  // TODO: Verify correct number of blocks have been pruned from storage.
}

TEST(pruning_sm_test, sm_handle_incorrect_prune_request) {
  const auto replica_count = 4;
  const auto client_proxy_count = replica_count;
  const auto replica_idx = 1;
  const auto client_idx = 0;
  const auto config =
      TestConfiguration(replica_count, client_proxy_count, NUM_BLOCKS_TO_KEEP);
  TestStorage storage;

  const auto sm =
      KVBPruningSM{storage, config, GetNodeConfig(config, replica_idx)};

  // Set an invalid sender.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    req.mutable_prune_request()->set_sender(req.prune_request().sender() + 1);
    ConcordResponse resp;

    sm.Handle(req, resp, false, *test_span);

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(resp.has_prune_response());
    ASSERT_EQ(resp.error_response_size(), 0);
  }

  // Add a valid N + 1 latest prunable block.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    const auto& block = req.prune_request().latest_prunable_block(3);
    auto added_block = req.mutable_prune_request()->add_latest_prunable_block();
    added_block->CopyFrom(block);
    ConcordResponse resp;

    sm.Handle(req, resp, false, *test_span);

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(resp.has_prune_response());
    ASSERT_EQ(resp.error_response_size(), 0);
  }

  // Send N - 1 latest prunable blocks.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    req.mutable_prune_request()->mutable_latest_prunable_block()->RemoveLast();
    ConcordResponse resp;

    sm.Handle(req, resp, false, *test_span);

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(resp.has_prune_response());
    ASSERT_EQ(resp.error_response_size(), 0);
  }

  // Send a latest prunable block with an invalid signature.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    auto block =
        req.mutable_prune_request()->mutable_latest_prunable_block()->Mutable(
            2);
    block->mutable_signature()->at(0) += 1;
    ConcordResponse resp;

    sm.Handle(req, resp, false, *test_span);

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(resp.has_prune_response());
    ASSERT_EQ(resp.error_response_size(), 0);
  }

  // Send a prune request with an invalid signature.
  {
    auto req = ConstructPruneRequest(config, client_idx);
    req.mutable_prune_request()->mutable_signature()->at(42) += 1;
    ConcordResponse resp;

    sm.Handle(req, resp, false, *test_span);

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(resp.has_prune_response());
    ASSERT_EQ(resp.error_response_size(), 0);
  }

  // Send a valid prune request in a read-only message.
  {
    const auto req = ConstructPruneRequest(config, client_idx);
    ConcordResponse resp;

    sm.Handle(req, resp, true, *test_span);

    // Expect that the state machine has ignored the message.
    ASSERT_FALSE(resp.has_prune_response());
    ASSERT_EQ(resp.error_response_size(), 0);
  }
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  auto& hierarchy = log4cplus::Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  log4cplus::BasicConfigurator config{hierarchy, false};
  config.configure();
  return RUN_ALL_TESTS();
}
