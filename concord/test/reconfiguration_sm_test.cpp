// Copyright 2020 VMware, all rights reserved

#include "reconfiguration/reconfiguration_sm.hpp"
#include <opentracing/tracer.h>
#include "Logger.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "gtest/gtest.h"
#include "mocks.hpp"
#include "reconfiguration/upgrade_plugin.hpp"
#include "status.hpp"
#include "utils/concord_prometheus_metrics.hpp"

using com::vmware::concord::ConcordReplicaSpecificInfoResponse;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationResponse;
using com::vmware::concord::ReconfigurationSmRequest;
using com::vmware::concord::ReconfigurationSmRequest_PluginId_MOCK;
using com::vmware::concord::ReconfigurationSmRequest_PluginId_UPGRADE;
using com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand;
using com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand_UpgradeType;
using concord::reconfiguration::IReconfigurationPlugin;
using concord::reconfiguration::ReconfigurationSM;
using concord::utils::openssl_crypto::AsymmetricPrivateKey;
using concord::utils::openssl_crypto::AsymmetricPublicKey;
using concord::utils::openssl_crypto::GenerateAsymmetricCryptoKeyPair;

namespace {

auto registry =
    std::make_shared<concord::utils::PrometheusRegistry>("127.0.0.1:9891");

// Helper function to the pruning state machine unit tests to generate a key
// pair for the operator privileged to issue pruning commands, add the public
// key for that key pair to the provided ConcordConfiguration object, and return
// the private key from that key pair for use in the testing.
void ConfigureOperatorKey(ConcordConfiguration& config) {
  config.declareParameter("signing_key_path",
                          "Public key for the privileged operator authorized "
                          "to issue reconfiguration commands.");
  config.loadValue("signing_key_path", "./resources/signing_keys");
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
  MockPlugin() { pluginId_ = ReconfigurationSmRequest_PluginId_MOCK; }
  void Handle(
      const std::string& command, uint64_t sequence_num, bool readOnly,
      opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      concord::reconfiguration::ConcordControlHandler& control_handlers)
      override {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata(command + "-world");
  }
};

auto test_span =
    opentracing::Tracer::Global() -> StartSpan("reconfiguration_sm_test");

/*
 * Test successful creation of the reconfiguration state machine.
 */
TEST(reconfiguration_sm_test, create_rsm) {
  auto config = defaultTestConfig();
  ConfigureOperatorKey(config);
  EXPECT_NO_THROW(ReconfigurationSM rsm(config, registry));
}

/*
 * Test that the state machine return error if the plugin is not loaded
 */
TEST(reconfiguration_sm_test, reject_invalid_pluginId) {
  auto config = defaultTestConfig();
  ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config, registry);

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_MOCK);
  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());
}

/*
 * Test various of invalid commands
 */
TEST(reconfiguration_sm_test, reject_invalid_command) {
  auto config = defaultTestConfig();
  ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config, registry);
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  rsm.LoadPlugin(std::make_unique<MockPlugin>());

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_MOCK);

  // Test that an empty reconfiguration request will be rejected
  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command without signature will be rejected
  reconfig_sm_req.set_command("hello world");
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command with invalid signature will be rejected
  reconfig_sm_req.set_signature("hello world");
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());
}

/*
 * Test a basic mock command
 */
TEST(reconfiguration_sm_test, run_basic_ro_mock_plugin) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config, registry);
  rsm.LoadPlugin(std::make_unique<MockPlugin>());

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_MOCK);
  reconfig_sm_req.set_command("hello");
  reconfig_sm_req.set_signature(priv_key->Sign("hello"));

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "hello-world");
}

TEST(reconfiguration_sm_test, test_get_version_upgrade_plugin_command) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config, registry);
  rsm.LoadPlugin(std::make_unique<concord::reconfiguration::UpgradePlugin>());
  ReconfigurationSmRequest_UpgradeCommand upgrade_command;
  upgrade_command.set_type(
      ReconfigurationSmRequest_UpgradeCommand_UpgradeType::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_GET_VERSION);
  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_UPGRADE);
  auto upgrade_as_string = upgrade_command.SerializeAsString();
  reconfig_sm_req.set_command(upgrade_as_string);
  reconfig_sm_req.set_signature(priv_key->Sign(upgrade_as_string));

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "Upgrading");
}

TEST(reconfiguration_sm_test, test_validate_version_upgrade_plugin_command) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config, registry);
  rsm.LoadPlugin(std::make_unique<concord::reconfiguration::UpgradePlugin>());
  ReconfigurationSmRequest_UpgradeCommand upgrade_command;
  upgrade_command.set_type(
      ReconfigurationSmRequest_UpgradeCommand_UpgradeType::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_VALIDATE_VERSION);
  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_UPGRADE);
  auto upgrade_as_string = upgrade_command.SerializeAsString();
  reconfig_sm_req.set_command(upgrade_as_string);
  reconfig_sm_req.set_signature(priv_key->Sign(upgrade_as_string));

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "Valid");
}

TEST(reconfiguration_sm_test, test_execute_upgrade_upgrade_plugin_command) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  ReconfigurationSM rsm(config, registry);
  rsm.LoadPlugin(std::make_unique<concord::reconfiguration::UpgradePlugin>());
  ReconfigurationSmRequest_UpgradeCommand upgrade_command;
  upgrade_command.set_type(
      ReconfigurationSmRequest_UpgradeCommand_UpgradeType::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_EXECUTE_UPGRADE);
  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.set_pluginid(ReconfigurationSmRequest_PluginId_UPGRADE);
  auto upgrade_as_string = upgrade_command.SerializeAsString();
  reconfig_sm_req.set_command(upgrade_as_string);
  reconfig_sm_req.set_signature(priv_key->Sign(upgrade_as_string));

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "Upgraded");
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
