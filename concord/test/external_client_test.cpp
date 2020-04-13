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
TEST(external_client_test, parse_valid_config_file) {
  EXPECT_NO_THROW({
    ConcordClientPool client_tls("resources/external_client_tls.config",
                                 StatusCallback);
  });
}
}  // namespace

int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
