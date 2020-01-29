#include "external_client/external_client.hpp"

#include "gtest/gtest.h"

#include "config/configuration_manager.hpp"

#include <array>
#include <chrono>
#include <cstdint>
#include <exception>
#include <sstream>

namespace {

using namespace concord::external_client;
using namespace concordUtils;
using namespace concord::config;
using buf_t = std::array<std::uint8_t, 512>;
constexpr auto flags = bftEngine::READ_ONLY_REQ;

void StatusCallback(PeerConnectivityStatus) {}

// Test file parsing.
TEST(external_client_test, parse_valid_config_files) {
  ASSERT_NO_THROW({
    ConcordClient client_tls("resources/external_client_tls.config",
                             StatusCallback);
    ConcordClient client_udp("resources/external_client_udp.config",
                             StatusCallback);
  });
}

// Test stream parsing.
TEST(external_client_test, parse_valid_stream) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_NO_THROW({ ConcordClient client(stream, StatusCallback); });
}

// Send an UDP request to non-existent hosts and verify a timeout has occurred.
TEST(external_client_test, send_udp_request) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
      "node:\n"
      "  - replica:\n"
      "      - principal_id: 0\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica1\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 1\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica2\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 2\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica3\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 3\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica4\n"
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  buf_t req_buf;
  buf_t resp_buf;
  std::uint32_t resp_size;
  ConcordClient client{stream, StatusCallback};

  // Expect a timeout.
  ASSERT_THROW(
      {
        try {
          client.SendRequestSync(req_buf.data(), req_buf.size(), flags,
                                 std::chrono::milliseconds{1}, resp_buf.size(),
                                 resp_buf.data(), &resp_size);
        } catch (const ClientRequestException& e) {
          ASSERT_TRUE(e.Status().isGeneralError());
          throw;
        }
      },
      ClientRequestException);
}

// Send a TLS request to non-existent hosts and verify a timeout has occurred.
TEST(external_client_test, send_tls_request) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3502\n"
      "node:\n"
      "  - replica:\n"
      "      - principal_id: 0\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica1\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 1\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica2\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 2\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica3\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 3\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica4\n"
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  buf_t req_buf;
  buf_t resp_buf;
  std::uint32_t resp_size;
  ConcordClient client{stream, StatusCallback};

  // Expect a timeout.
  ASSERT_THROW(
      {
        try {
          client.SendRequestSync(req_buf.data(), req_buf.size(), flags,
                                 std::chrono::milliseconds{1}, resp_buf.size(),
                                 resp_buf.data(), &resp_size);
        } catch (const ClientRequestException& e) {
          ASSERT_TRUE(e.Status().isGeneralError());
          throw;
        }
      },
      ClientRequestException);
}

// Verify that the constructor throws on invalid 'comm_to_use' .
TEST(external_client_test, invalid_comm_type) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: dummy\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_THROW({ ConcordClient client(stream, StatusCallback); },
               std::invalid_argument);
}

// Verify that the user cannot specify a buffer that is too small.
TEST(external_client_test, small_buffer_len) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 511\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_THROW({ ConcordClient client(stream, StatusCallback); },
               ConfigurationException);
}

// Verify that the constructor throws in case of an invalid f_val that makes
// 'num_replicas != 3 * f_val + 2 * c_val + 1' .
TEST(external_client_test, invalid_f_val) {
  const auto conf =
      "f_val: 2\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_THROW({ ConcordClient client(stream, StatusCallback); },
               ConfigurationException);
}

// Verify that the constructor throws in case of an invalid c_val that makes
// 'num_replicas != 3 * f_val + 2 * c_val + 1' .
TEST(external_client_test, invalid_c_val) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 3\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_THROW({ ConcordClient client(stream, StatusCallback); },
               ConfigurationException);
}

// Verify that the constructor throws in case of a missing replica in the
// configuration.
TEST(external_client_test, missing_replica) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_THROW({ ConcordClient client(stream, StatusCallback); },
               ConfigurationException);
}

TEST(external_client_test, missing_ip) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 19\n"
      "num_replicas: 4\n"
      "comm_to_use: udp\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_port: 3503\n"
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
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_THROW({ ConcordClient client(stream, StatusCallback); },
               ConfigurationException);
}

// Verify that the constructor throws when self_principal_id doesn't point
// to existing TLS certificates on the file system.
TEST(external_client_test, missing_tls_certs) {
  const auto conf =
      "f_val: 1\n"
      "c_val: 0\n"
      "self_principal_id: 42\n"
      "num_replicas: 4\n"
      "comm_to_use: tls\n"
      "tls_certificates_folder_path: resources/tls_certs\n"
      "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
      "concord-bft_communication_buffer_length: 64000\n"
      "bind_ip: 0.0.0.0\n"
      "bind_port: 3502\n"
      "node:\n"
      "  - replica:\n"
      "      - principal_id: 0\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica1\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 1\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica2\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 2\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica3\n"
      "        replica_port: 3501\n"
      "  - replica:\n"
      "      - principal_id: 3\n"
      "        replica_host: 0x05non_existent_external_test_concord_replica4\n"
      "        replica_port: 3501\n";
  auto stream = std::stringstream{conf};

  ASSERT_ANY_THROW({ ConcordClient client(stream, StatusCallback); });
}

}  // namespace

int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
