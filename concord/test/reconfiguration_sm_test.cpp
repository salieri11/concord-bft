// Copyright 2020 VMware, all rights reserved

#include "reconfiguration/reconfiguration_sm.hpp"
#include <concord.pb.h>
#include <opentracing/tracer.h>
#include "Logger.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "gtest/gtest.h"
#include "mocks.hpp"
#include "reconfiguration/reconfiguration_sm_dispatcher.hpp"
#include "status.hpp"
#include "utils/concord_prometheus_metrics.hpp"

using com::vmware::concord::ConcordReplicaSpecificInfoResponse;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationResponse;
using concord::config::ConcordConfiguration;
using concord::reconfiguration::ReconfigurationSM;
using concord::reconfiguration::ReconfigurationSMDispatcher;
using concord::utils::openssl_crypto::AsymmetricPrivateKey;
using concord::utils::openssl_crypto::AsymmetricPublicKey;
using concord::utils::openssl_crypto::GenerateAsymmetricCryptoKeyPair;

using concord::messages::DownloadCommand;
using concord::messages::GetVersionCommand;
using concord::messages::ReconfigurationRequest;
using concord::messages::TestCommand;
using concord::messages::UpgradeCommand;

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
    ReconfigurationRequest& request,
    concord::utils::openssl_crypto::AsymmetricPrivateKey* priv_key) {
  std::vector<uint8_t> serialized_req;
  serialize(serialized_req, request);
  auto sig =
      priv_key->Sign(std::string(serialized_req.begin(), serialized_req.end()));
  request.signature = std::vector<uint8_t>(sig.begin(), sig.end());
}

auto test_span =
    opentracing::Tracer::Global() -> StartSpan("reconfiguration_sm_test");

/*
 * Test successful creation of the reconfiguration state machine.
 */
TEST(reconfiguration_sm_test, create_dispatcher_and_rsm) {
  auto config = defaultTestConfig();
  ConfigureOperatorKey(config);
  EXPECT_NO_THROW(ReconfigurationSMDispatcher d(
      std::make_unique<ReconfigurationSM>(), config, registry));
}

/*
 * Test various of invalid commands
 */
TEST(reconfiguration_sm_test, reject_invalid_command) {
  auto config = defaultTestConfig();
  ConfigureOperatorKey(config);
  auto rsm = std::make_unique<ReconfigurationSMDispatcher>(
      std::make_unique<ReconfigurationSM>(), config, registry);
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  ReconfigurationRequest req;

  // Test that an empty reconfiguration request will be rejected
  rsm->dispatch(req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command without signature will be rejected
  req.command = TestCommand{"hello"};
  rsm->dispatch(req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());

  // Test that a command with invalid signature will be rejected
  req.signature.push_back(10);
  rsm->dispatch(req, res, 0, true, rsi_response, *test_span);
  ASSERT_FALSE(res.mutable_reconfiguration_sm_response()->success());
}

TEST(reconfiguration_sm_test, test_download_command) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  auto rsm = std::make_unique<ReconfigurationSMDispatcher>(
      std::make_unique<ReconfigurationSM>(), config, registry);

  ReconfigurationRequest req;
  req.command = DownloadCommand{};
  sign_message(req, priv_key.get());

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm->dispatch(req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "Downloading");
}

TEST(reconfiguration_sm_test, test_get_version_command) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  auto rsm = std::make_unique<ReconfigurationSMDispatcher>(
      std::make_unique<ReconfigurationSM>(), config, registry);

  ReconfigurationRequest req;
  req.command = GetVersionCommand{};
  sign_message(req, priv_key.get());

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm->dispatch(req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "Version");
}

TEST(reconfiguration_sm_test, test_upgrade_command) {
  auto config = defaultTestConfig();
  auto priv_key = concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
      "./resources/signing_keys/operator_priv.pem", "secp256r1");
  ConfigureOperatorKey(config);
  auto rsm = std::make_unique<ReconfigurationSMDispatcher>(
      std::make_unique<ReconfigurationSM>(), config, registry);

  ReconfigurationRequest req;
  req.command = UpgradeCommand{};
  sign_message(req, priv_key.get());

  ConcordResponse res;
  ConcordReplicaSpecificInfoResponse rsi_response;
  rsm->dispatch(req, res, 0, true, rsi_response, *test_span);
  ASSERT_TRUE(res.mutable_reconfiguration_sm_response()->success());
  ASSERT_EQ(res.mutable_reconfiguration_sm_response()->additionaldata(),
            "Upgrading");
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
