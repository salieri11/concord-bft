// Copyright 2020 VMware, all rights reserved

#include "reconfiguration/reconfiguration_sm.hpp"
#include <concord.pb.h>
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
void sign_message(
    com::vmware::concord::ReconfigurationSmRequest& request,
    concord::utils::openssl_crypto::AsymmetricPrivateKey* priv_key) {
  auto sig = priv_key->Sign(request.SerializeAsString());
  request.set_signature(sig);
}

class MockPlugin : public IReconfigurationPlugin {
 public:
  virtual com::vmware::concord::ReconfigurationSmRequest::CommandCase
  GetPluginId() const override {
    return com::vmware::concord::ReconfigurationSmRequest::CommandCase::
        kTestCmd;
  }
  bool Handle(
      const com::vmware::concord::ReconfigurationSmRequest& command,
      uint64_t sequence_num, bool readOnly, opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      concord::reconfiguration::ConcordControlHandler& control_handlers)
      override {
    auto& cmd = command.test_cmd();
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata(cmd + "-world");
    return true;
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
  reconfig_sm_req.set_test_cmd("hello");
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
  reconfig_sm_req.set_test_cmd("hello");

  // Test that an empty reconfiguration request will be rejected
  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command without signature will be rejected
  reconfig_sm_req.set_test_cmd("hello");
  rsm.Handle(reconfig_sm_req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command with invalid signature will be rejected
  reconfig_sm_req.set_signature("xxx");
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
  reconfig_sm_req.set_test_cmd("hello");
  sign_message(reconfig_sm_req, priv_key.get());

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
  rsm.LoadPlugin(
      std::make_unique<concord::reconfiguration::DownLoadSwVersionPlugin>());

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.mutable_download_sw_version_cmd();
  sign_message(reconfig_sm_req, priv_key.get());

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
  rsm.LoadPlugin(
      std::make_unique<concord::reconfiguration::HasSwVersionPlugin>());

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.mutable_has_sw_version_cmd();
  sign_message(reconfig_sm_req, priv_key.get());

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
  rsm.LoadPlugin(
      std::make_unique<concord::reconfiguration::UpgradeSwVersionPlugin>());

  ReconfigurationSmRequest reconfig_sm_req;
  reconfig_sm_req.mutable_upgrade_sw_version_cmd();
  sign_message(reconfig_sm_req, priv_key.get());

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