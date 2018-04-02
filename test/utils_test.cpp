#include "common/utils.hpp"
#include "gtest/gtest.h"

using namespace std;
namespace athena = com::vmware::athena;
using json = nlohmann::json;

namespace {
TEST(utils_test, parses_genesis_block) {
   string genesis_test_file = "resources/genesis.json";
   json pj = athena::parse_genesis_block(genesis_test_file);
   int chainID = pj["config"]["chainId"];
   ASSERT_EQ(chainID, 12349876);
}
}
