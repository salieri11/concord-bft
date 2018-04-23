#include "common/utils.hpp"
#include "gtest/gtest.h"
#include "common/json.hpp"
#include "common/rlp.hpp"

using namespace std;
namespace athena = com::vmware::athena;
using json = nlohmann::json;

namespace {
TEST(utils_test, parses_genesis_block) {
   // string genesis_test_file = "resources/genesis.json";
   // json pj = athena::parse_genesis_block(genesis_test_file);
   // int chainID = pj["config"]["chainId"];
   // ASSERT_EQ(chainID, 12349876);
}

// examples from https://github.com/ethereum/wiki/wiki/RLP
TEST(rlp_test, example_dog) {
   athena::RLPBuilder rlpb;
   std::vector<uint8_t> input{ 'd', 'o', 'g' };
   rlpb.add(input);
   std::vector<uint8_t> expect{ 0x83, 'd', 'o', 'g' };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_cat_dog) {
   athena::RLPBuilder rlpb;
   rlpb.start_list();
   std::vector<uint8_t> input1{ 'd', 'o', 'g' };
   rlpb.add(input1);
   std::vector<uint8_t> input2{ 'c', 'a', 't' };
   rlpb.add(input2);
   std::vector<uint8_t> expect{
      0xc8, 0x83, 'c', 'a', 't', 0x83, 'd', 'o', 'g' };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_empty_string) {
   athena::RLPBuilder rlpb;
   std::vector<uint8_t> input;
   rlpb.add(input);
   std::vector<uint8_t> expect{ 0x80 };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_empty_list) {
   athena::RLPBuilder rlpb;
   rlpb.start_list();
   std::vector<uint8_t> expect{ 0xc0 };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_integer_0) {
   athena::RLPBuilder rlpb;
   rlpb.add(0);
   std::vector<uint8_t> expect{ 0x00 };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_integer_15) {
   athena::RLPBuilder rlpb;
   rlpb.add(15);
   std::vector<uint8_t> expect{ 0x0f };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_integer_1024) {
   athena::RLPBuilder rlpb;
   rlpb.add(1024);
   std::vector<uint8_t> expect{ 0x82, 0x04, 0x00 };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_nexted_list) {
   athena::RLPBuilder rlpb;

   // remember that RLPBuilder expects additions in reverse order, so read the
   // test case backward to understand the code: [ [], [[]], [ [], [[]] ] ]
   rlpb.start_list(); {
      rlpb.start_list(); {
         rlpb.start_list(); {
            rlpb.start_list();
            rlpb.end_list();
         } rlpb.end_list();

         rlpb.start_list();
         rlpb.end_list();
      } rlpb.end_list();

      rlpb.start_list(); {
         rlpb.start_list();
         rlpb.end_list();
      } rlpb.end_list();

      rlpb.start_list();
      rlpb.end_list();
   } //implicit end
   std::vector<uint8_t> expect{
      0xc7, 0xc0, 0xc1, 0xc0, 0xc3, 0xc0, 0xc1, 0xc0 };
   EXPECT_EQ(expect, rlpb.build());
}

TEST(rlp_test, example_lipsum) {
   athena::RLPBuilder rlpb;
   std::string str("Lorem ipsum dolor sit amet, consectetur adipisicing elit");
   std::vector<uint8_t> input(str.begin(), str.end());
   rlpb.add(input);
   std::vector<uint8_t> expect{ 0xb8, 0x38 };
   std::copy(input.begin(), input.end(), std::back_inserter(expect));
   EXPECT_EQ(expect, rlpb.build());
}



}

int main(int argc, char **argv) {
   ::testing::InitGoogleTest(&argc, argv);
   return RUN_ALL_TESTS();
}
