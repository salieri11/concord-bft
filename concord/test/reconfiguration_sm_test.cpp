// Copyright 2020 VMware, all rights reserved

#include "reconfiguration/reconfiguration_sm.hpp"
#include <log4cplus/configurator.h>
#include <opentracing/tracer.h>
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "gtest/gtest.h"
#include "mocks.hpp"
#include "status.hpp"

using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationResponse;
using com::vmware::concord::ReconfigurationSmRequest;
using com::vmware::concord::ReconfigurationSmRequest_PluginId_MOCK;
using concord::reconfiguration::IReconfigurationPlugin;
using concord::reconfiguration::PluginReply;
using concord::reconfiguration::ReconfigurationSM;
using concord::utils::openssl_crypto::AsymmetricPrivateKey;
using concord::utils::openssl_crypto::AsymmetricPublicKey;
using concord::utils::openssl_crypto::GenerateAsymmetricCryptoKeyPair;

namespace {

// Helper function to the pruning state machine unit tests to generate a key
// pair for the operator privileged to issue pruning commands, add the public
// key for that key pair to the provided ConcordConfiguration object, and return
// the private key from that key pair for use in the testing.
std::unique_ptr<AsymmetricPrivateKey> ConfigureOperatorKey(
    ConcordConfiguration& config) {
  config.declareParameter("reconfiguration_operator_public_key",
                          "Public key for the privileged operator authorized "
                          "to issue reconfiguration commands.");

  std::pair<std::unique_ptr<AsymmetricPrivateKey>,
            std::unique_ptr<AsymmetricPublicKey>>
      operator_key_pair = GenerateAsymmetricCryptoKeyPair("secp256r1");
  config.loadValue("reconfiguration_operator_public_key",
                   operator_key_pair.second->Serialize());
  return move(operator_key_pair.first);
}

ConcordConfiguration defaultTestConfig() {
  int replicas = 0;
  int proxies_per_replica = 0;
  int num_of_blocks_to_keep = 0;
  int duration_to_keep_minutes = 0;
  bool pruning_enabled = false;
  bool time_service_enabled = false;
  return TestConfiguration(replicas, proxies_per_replica, num_of_blocks_to_keep,
                           duration_to_keep_minutes, pruning_enabled,
                           time_service_enabled);
}

class MockPlugin : public IReconfigurationPlugin {
 public:
  MockPlugin(ReconfigurationSmRequest::PluginId pluginId)
      : IReconfigurationPlugin(pluginId) {}
  PluginReply Handle(const std::string& command, bool readOnly,
                     opentracing::Span& parent_span) override {
    return {true, command + "-world"};
  }
};

auto test_span =
    opentracing::Tracer::Global() -> StartSpan("reconfiguration_sm_test");

/*
 * Test successful creation of the reconfiguration state machine.
 */
TEST(reconfiguration_sm_test, create_rsm) {
  auto config = defaultTestConfig();
  auto priv_key = ConfigureOperatorKey(config);
  EXPECT_NO_THROW(ReconfigurationSM rsm(config));
}

/*
 * Test that the state machine return error if the plugin is not loaded
 */
TEST(reconfiguration_sm_test, reject_invalid_pluginId) {
  auto config = defaultTestConfig();
  auto priv_key = ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config);

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_MOCK);
  ConcordResponse res;
  rsm.Handle(reconfig_sm_req, res, true, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());
}

/*
 * Test various of invalid commands
 */
TEST(reconfiguration_sm_test, reject_invalid_command) {
  auto config = defaultTestConfig();
  auto priv_key = ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config);

  rsm.LoadPlugin(
      std::make_unique<MockPlugin>(ReconfigurationSmRequest_PluginId_MOCK));

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_MOCK);

  // Test that an empty reconfiguration request will be rejected
  ConcordResponse res;
  rsm.Handle(reconfig_sm_req, res, true, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command without signature will be rejected
  reconfig_sm_req.set_command("hello world");
  rsm.Handle(reconfig_sm_req, res, true, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command with invalid signature will be rejected
  reconfig_sm_req.set_signature("hello world");
  rsm.Handle(reconfig_sm_req, res, true, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that signature with a different key will be rejected
  auto config2 = defaultTestConfig();
  priv_key = ConfigureOperatorKey(config2);
  reconfig_sm_req.set_signature(priv_key->Sign("hello world"));
  rsm.Handle(reconfig_sm_req, res, true, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());
}

/*
 * Test a basic mock command
 */
TEST(reconfiguration_sm_test, run_basic_ro_mock_plugin) {
  auto config = defaultTestConfig();
  auto priv_key = ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config);
  rsm.LoadPlugin(
      std::make_unique<MockPlugin>(ReconfigurationSmRequest_PluginId_MOCK));

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_MOCK);
  reconfig_sm_req.set_command("hello");
  reconfig_sm_req.set_signature(priv_key->Sign("hello"));

  ConcordResponse res;
  rsm.Handle(reconfig_sm_req, res, true, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "hello-world");
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  auto& hierarchy = log4cplus::Logger::getDefaultHierarchy();
  log4cplus::BasicConfigurator config{hierarchy, false};
  config.configure();
  return RUN_ALL_TESTS();
}
