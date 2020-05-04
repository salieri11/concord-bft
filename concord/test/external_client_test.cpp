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

// Test file parsing.
TEST(external_client_test, parse_valid_config_file) {
  EXPECT_NO_THROW({
    ConcordClientPool client_tls("resources/external_client_tls.config");
  });
}

// configuration file tests
TEST(external_client_configuration_test, num_external_clients_le_to_0) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "num_replicas: 4\n"
      "num_of_participant_nodes: 2\n"
      "clients_per_participant_node: -1\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "client_initial_retry_timeout_milli: 151\n"
      "client_min_retry_timeout_milli: 50\n"
      "client_max_retry_timeout_milli: 1000\n"
      "client_sends_request_to_all_replicas_first_thresh: 4\n"
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
      "Participant_nodes:\n"
      "  - Participant_node:\n"
      "      - external_clients:\n"
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

  auto stream = std::stringstream{conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); },
               ConfigurationException);
}

TEST(external_client_configuration_test, num_external_clients_gt_4096) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "num_replicas: 4\n"
      "num_of_participant_nodes: 2\n"
      "clients_per_participant_node: 4097\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "client_initial_retry_timeout_milli: 151\n"
      "client_min_retry_timeout_milli: 50\n"
      "client_max_retry_timeout_milli: 1000\n"
      "client_sends_request_to_all_replicas_first_thresh: 4\n"
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
      "Participant_nodes:\n"
      "  - Participant_node:\n"
      "      - external_clients:\n"
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

  auto stream = std::stringstream{conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); },
               ConfigurationException);
}

TEST(external_client_configuration_test,
     num_external_clients_lt_defined_clients) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "num_replicas: 4\n"
      "num_of_participant_nodes: 2\n"
      "clients_per_participant_node: 2\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "client_initial_retry_timeout_milli: 151\n"
      "client_min_retry_timeout_milli: 50\n"
      "client_max_retry_timeout_milli: 1000\n"
      "client_sends_request_to_all_replicas_first_thresh: 4\n"
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
      "Participant_nodes:\n"
      "  - Participant_node:\n"
      "      - external_clients:\n"
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

  auto stream = std::stringstream{conf};
  EXPECT_NO_THROW({ ConcordClientPool client_tls(stream); });
}

TEST(external_client_configuration_test, undefined_client_port) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "num_replicas: 4\n"
      "num_of_participant_nodes: 2\n"
      "clients_per_participant_node: 5\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "client_initial_retry_timeout_milli: 151\n"
      "client_min_retry_timeout_milli: 50\n"
      "client_max_retry_timeout_milli: 1000\n"
      "client_sends_request_to_all_replicas_first_thresh: 4\n"
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
      "Participant_nodes:\n"
      "  - Participant_node:\n"
      "      - external_clients:\n"
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

  auto stream = std::stringstream{conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); },
               ConfigurationResourceNotFoundException);
}

TEST(external_client_configuration_test, undefined_client_principal_id) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "num_replicas: 4\n"
      "num_of_participant_nodes: 2\n"
      "clients_per_participant_node: 5\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "client_initial_retry_timeout_milli: 151\n"
      "client_min_retry_timeout_milli: 50\n"
      "client_max_retry_timeout_milli: 1000\n"
      "client_sends_request_to_all_replicas_first_thresh: 4\n"
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
      "Participant_nodes:\n"
      "  - Participant_node:\n"
      "      - external_clients:\n"
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

  auto stream = std::stringstream{conf};
  EXPECT_THROW({ ConcordClientPool client_tls(stream); },
               ConfigurationResourceNotFoundException);
}
}  // namespace

int main(int argc, char *argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}