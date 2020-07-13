#include "external_client/external_client.hpp"
#include "external_client/concord_client_pool.hpp"
#include "gtest/gtest.h"

#include "config/configuration_manager.hpp"

#include <array>
#include <chrono>
#include <cstdint>
#include <exception>

namespace {
using namespace concord::concord_client_pool;
using namespace concord::external_client;
using namespace concordUtils;
using namespace concord::config;
using buf_t = std::array<std::uint8_t, 512>;
constexpr auto flags = bftEngine::READ_ONLY_REQ;

class DamlMock {
 public:
  DamlMock(std::istream& config_stream)
      : client_tls_(ConcordClientPool(config_stream)), config_() {}

  SubmitResult SendRequest() {
    bft::client::Msg* request_ = new bft::client::Msg(msg_.begin(), msg_.end());
    return client_tls_.SendRequest(config_, std::move(*request_));
  }

  void BuildRequest() {
    config_.request.pre_execute = false;
    config_.request.correlation_id = std::to_string(cid_);
    cid_++;
  }

 private:
  ConcordClientPool client_tls_;
  std::string msg_ =
      "08041801226A1F8B0800000000000000E3B2B2B21052"
      "B134324C4B32494AD3B530B448D63549B2B0D4B54C4D"
      "33D34D314F314F4A36374D4C34B090124849CCCD89CF"
      "494D494F2D8A4F2CC834E4E256E20C482C2AA9F44BCC"
      "4DD5F2E322CA182184164C1391CC0300427431ED9900"
      "0000";
  bft::client::ReadConfig config_;
  uint32_t cid_ = 0;
};
const auto good_conf =
    "f_val: 1\n"
    "c_val: 0\n"
    "num_replicas: 4\n"
    "clients_per_participant_node: 1\n"
    "comm_to_use: tls\n"
    "prometheus_port: 9873\n"
    "tls_certificates_folder_path: resources/tls_certs\n"
    "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
    "concord-bft_communication_buffer_length: 64000\n"
    "client_initial_retry_timeout_milli: 151\n"
    "client_min_retry_timeout_milli: 50\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_number_of_standard_deviations_to_tolerate: 2\n"
    "client_samples_per_evaluation: 32\n"
    "client_samples_until_reset: 1000\n"
    "client_sends_request_to_all_replicas_first_thresh: 2\n"
    "client_sends_request_to_all_replicas_period_thresh: 2\n"
    "client_periodic_reset_thresh: 30\n"
    "node:\n"
    "  - replica:\n"
    "      - principal_id: 0\n"
    "        replica_host: concord1\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 1\n"
    "        replica_host: concord2\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 2\n"
    "        replica_host: concord3\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 3\n"
    "        replica_host: concord4\n"
    "        replica_port: 3501\n"
    "participant_nodes:\n"
    "  - participant_node:\n"
    "      - participant_node_host: 0.0.0.0\n"
    "        external_clients:\n"
    "          - client:\n"
    "              - client_port: 3507\n"
    "                principal_id: 9\n"
    "          - client:\n"
    "              - client_port: 3508\n"
    "                principal_id: 10\n"
    "          - client:\n"
    "              - client_port: 3509\n"
    "                principal_id: 11\n"
    "          - client:\n"
    "              - client_port: 3510\n"
    "                principal_id: 12\n"
    "          - client:\n"
    "              - client_port: 3511\n"
    "                principal_id: 13\n";
const auto clients_lt_0_conf =
    "f_val: 1\n"
    "c_val: 0\n"
    "num_replicas: 4\n"
    "clients_per_participant_node: -1\n"
    "comm_to_use: tls\n"
    "prometheus_port: 9873\n"
    "tls_certificates_folder_path: resources/tls_certs\n"
    "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
    "concord-bft_communication_buffer_length: 64000\n"
    "client_initial_retry_timeout_milli: 151\n"
    "client_min_retry_timeout_milli: 50\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_number_of_standard_deviations_to_tolerate: 2\n"
    "client_samples_per_evaluation: 32\n"
    "client_sends_request_to_all_replicas_first_thresh: 2\n"
    "client_sends_request_to_all_replicas_period_thresh: 2\n"
    "client_periodic_reset_thresh: 30\n"
    "node:\n"
    "  - replica:\n"
    "      - principal_id: 0\n"
    "        replica_host: concord1\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 1\n"
    "        replica_host: concord2\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 2\n"
    "        replica_host: concord3\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 3\n"
    "        replica_host: concord4\n"
    "        replica_port: 3501\n"
    "participant_nodes:\n"
    "  - participant_node:\n"
    "      - participant_node_host: 0.0.0.0\n"
    "        external_clients:\n"
    "          - client:\n"
    "              - client_port: 3507\n"
    "                principal_id: 9\n"
    "          - client:\n"
    "              - client_port: 3508\n"
    "                principal_id: 10\n"
    "          - client:\n"
    "              - client_port: 3509\n"
    "                principal_id: 11\n"
    "          - client:\n"
    "              - client_port: 3510\n"
    "                principal_id: 12\n"
    "          - client:\n"
    "              - client_port: 3511\n"
    "                principal_id: 13\n";

const auto clients_gt_4096_conf =
    "f_val: 1\n"
    "c_val: 0\n"
    "num_replicas: 4\n"
    "clients_per_participant_node: 4097\n"
    "comm_to_use: tls\n"
    "prometheus_port: 9873\n"
    "tls_certificates_folder_path: resources/tls_certs\n"
    "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
    "concord-bft_communication_buffer_length: 64000\n"
    "client_initial_retry_timeout_milli: 151\n"
    "client_min_retry_timeout_milli: 50\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_number_of_standard_deviations_to_tolerate: 2\n"
    "client_samples_per_evaluation: 32\n"
    "client_sends_request_to_all_replicas_first_thresh: 2\n"
    "client_sends_request_to_all_replicas_period_thresh: 2\n"
    "client_periodic_reset_thresh: 30\n"
    "node:\n"
    "  - replica:\n"
    "      - principal_id: 0\n"
    "        replica_host: concord1\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 1\n"
    "        replica_host: concord2\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 2\n"
    "        replica_host: concord3\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 3\n"
    "        replica_host: concord4\n"
    "        replica_port: 3501\n"
    "participant_nodes:\n"
    "  - participant_node:\n"
    "      - participant_node_host: 0.0.0.0\n"
    "        external_clients:\n"
    "          - client:\n"
    "              - client_port: 3507\n"
    "                principal_id: 9\n"
    "          - client:\n"
    "              - client_port: 3508\n"
    "                principal_id: 10\n"
    "          - client:\n"
    "              - client_port: 3509\n"
    "                principal_id: 11\n"
    "          - client:\n"
    "              - client_port: 3510\n"
    "                principal_id: 12\n"
    "          - client:\n"
    "              - client_port: 3511\n"
    "                principal_id: 13\n";
const auto undefined_port_conf =
    "f_val: 1\n"
    "c_val: 0\n"
    "num_replicas: 4\n"
    "clients_per_participant_node: 5\n"
    "comm_to_use: tls\n"
    "prometheus_port: 9873\n"
    "tls_certificates_folder_path: resources/tls_certs\n"
    "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
    "concord-bft_communication_buffer_length: 64000\n"
    "client_initial_retry_timeout_milli: 151\n"
    "client_min_retry_timeout_milli: 50\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_number_of_standard_deviations_to_tolerate: 2\n"
    "client_samples_per_evaluation: 32\n"
    "client_sends_request_to_all_replicas_first_thresh: 2\n"
    "client_sends_request_to_all_replicas_period_thresh: 2\n"
    "client_periodic_reset_thresh: 30\n"
    "node:\n"
    "  - replica:\n"
    "      - principal_id: 0\n"
    "        replica_host: concord1\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 1\n"
    "        replica_host: concord2\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 2\n"
    "        replica_host: concord3\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 3\n"
    "        replica_host: concord4\n"
    "        replica_port: 3501\n"
    "participant_nodes:\n"
    "  - participant_node:\n"
    "      - participant_node_host: 0.0.0.0\n"
    "        external_clients:\n"
    "          - client:\n"
    "              - client_port: \n"
    "                principal_id: 9\n"
    "          - client:\n"
    "              - client_port: 3508\n"
    "                principal_id: 10\n"
    "          - client:\n"
    "              - client_port: 3509\n"
    "                principal_id: 11\n"
    "          - client:\n"
    "              - client_port: 3510\n"
    "                principal_id: 12\n"
    "          - client:\n"
    "              - client_port: 3511\n"
    "                principal_id: 13\n";
const auto undefined_principal_id_conf =
    "f_val: 1\n"
    "c_val: 0\n"
    "num_replicas: 4\n"
    "clients_per_participant_node: 5\n"
    "comm_to_use: tls\n"
    "prometheus_port: 9873\n"
    "tls_certificates_folder_path: resources/tls_certs\n"
    "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
    "concord-bft_communication_buffer_length: 64000\n"
    "client_initial_retry_timeout_milli: 151\n"
    "client_min_retry_timeout_milli: 50\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_max_retry_timeout_milli: 1000\n"
    "client_number_of_standard_deviations_to_tolerate: 2\n"
    "client_samples_per_evaluation: 32\n"
    "client_sends_request_to_all_replicas_first_thresh: 2\n"
    "client_sends_request_to_all_replicas_period_thresh: 2\n"
    "client_periodic_reset_thresh: 30\n"
    "node:\n"
    "  - replica:\n"
    "      - principal_id: 0\n"
    "        replica_host: concord1\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 1\n"
    "        replica_host: concord2\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 2\n"
    "        replica_host: concord3\n"
    "        replica_port: 3501\n"
    "  - replica:\n"
    "      - principal_id: 3\n"
    "        replica_host: concord4\n"
    "        replica_port: 3501\n"
    "participant_nodes:\n"
    "  - participant_node:\n"
    "      - participant_node_host: 0.0.0.0\n"
    "        external_clients:\n"
    "          - client:\n"
    "              - client_port: 3507\n"
    "                principal_id: \n"
    "          - client:\n"
    "              - client_port: 3508\n"
    "                principal_id: 10\n"
    "          - client:\n"
    "              - client_port: 3509\n"
    "                principal_id: 11\n"
    "          - client:\n"
    "              - client_port: 3510\n"
    "                principal_id: 12\n"
    "          - client:\n"
    "              - client_port: 3511\n"
    "                principal_id: 13\n";

// Pre-integration tests
TEST(mock_integration_test, mock_daml_request_acknowledged_situation) {
  auto stream = std::stringstream{good_conf};
  std::unique_ptr<DamlMock> da_mock = std::make_unique<DamlMock>(stream);
  da_mock->BuildRequest();
  auto res = da_mock->SendRequest();
  ASSERT_EQ(res, SubmitResult::Acknowledged);
}

TEST(mock_integration_test, mock_daml_request_internal_error_situation) {
  auto stream = std::stringstream{clients_lt_0_conf};
  EXPECT_THROW(
      {
        std::unique_ptr<DamlMock> da_mock = std::make_unique<DamlMock>(stream);
      },
      InternalError);
}

TEST(mock_integration_test, mock_daml_request_overload_situation) {
  auto stream = std::stringstream{good_conf};
  std::unique_ptr<DamlMock> da_mock = std::make_unique<DamlMock>(stream);
  da_mock->BuildRequest();
  auto res = da_mock->SendRequest();
  da_mock->BuildRequest();
  res = da_mock->SendRequest();
  da_mock->BuildRequest();
  res = da_mock->SendRequest();
  da_mock->BuildRequest();
  res = da_mock->SendRequest();
  ASSERT_EQ(res, SubmitResult::Overloaded);
}

// Test file parsing.
TEST(external_client_test, parse_valid_config_file) {
  EXPECT_NO_THROW({
    ConcordClientPool client_tls("resources/external_client_tls.config");
  });
}

// configuration file tests
TEST(external_client_configuration_test, num_external_clients_le_to_0) {
  auto stream = std::stringstream{clients_lt_0_conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); }, InternalError);
}

TEST(external_client_configuration_test, num_external_clients_gt_4096) {
  auto stream = std::stringstream{clients_gt_4096_conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); }, InternalError);
}

TEST(external_client_configuration_test,
     num_external_clients_lt_defined_clients) {
  auto stream = std::stringstream{good_conf};
  EXPECT_NO_THROW({ ConcordClientPool client_tls(stream); });
}

TEST(external_client_configuration_test, undefined_client_port) {
  auto stream = std::stringstream{undefined_port_conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); }, InternalError);
}

TEST(external_client_configuration_test, undefined_client_principal_id) {
  auto stream = std::stringstream{undefined_principal_id_conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); }, InternalError);
}
}  // namespace

int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}