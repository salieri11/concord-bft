#include "common/utils.hpp"
#include "gtest/gtest.h"
#include "common/json.hpp"
#include "common/athena_eth_sign.hpp"
#include "common/athena_eth_hash.hpp"

using namespace std;
namespace athena = com::vmware::athena;
using json = nlohmann::json;

namespace {
/**
 * These three sort of build on each other. In order to verify the Elliptic
 * Curve public key recovery, we have to generate the correct hash for the
 * transaction. In order to generate the correct hash, we have to generate the
 * correct RLP encoding. So, even though we could verify ecrecover and be done,
 * the additional tests will give us a clearer idea of which layer has a bug.
 *
 * This version is a very naive smoke test: verify that we agree that this
 * random transaction found on the public Ethereum chain is valid.
 *
 * https://etherscan.io/tx/0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68
 */

const string expectedRLP_s("0xf86b19847735940082520894f6c3fff0b77efe806fcc10176b8cbf71c6dfe3be880429d069189e00008025a0141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397a07fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38");
const string expectedHash_s("0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68");
const string expectedFrom_s("0x42c4f19a097955ff2a013ef8f014977f4e8516c3");
const string expectedBlock_s("0x062092761067b1381cb544b581be2afeb9fe75dc00ce502932b53cebc40d81b1");
const string expectedTo_s("0xf6c3fff0b77efe806fcc10176b8cbf71c6dfe3be");
const string expectedSigR_s("0x141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397");
const string expectedSigS_s("0x7fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38");
const int8_t expectedSigV = 27; //pretty sure this tx actually says 37 here

std::vector<uint8_t> example_expected_rlp() {
   return com::vmware::athena::dehex(expectedRLP_s);
}

evm_uint256be uint256_from_string(const string &str) {
   std::vector<uint8_t> vec = com::vmware::athena::dehex(str);
   evm_uint256be hash;
   std::copy(vec.begin(), vec.end(), hash.bytes);
   return hash;
}

evm_address addr_from_string(const string &str) {
   std::vector<uint8_t> vec = com::vmware::athena::dehex(str);
   evm_address addr;
   std::copy(vec.begin(), vec.end(), addr.bytes);
   return addr;
}

// evm_uint256be example_expected_hash() {
//    return uint256_from_string(expectedHash_s);
// }

// evm_address example_expected_from() {
//    return addr_from_string(expectedFrom_s);
// }

// const com::vmware::athena::EthTransaction tx{
//    25, // nonce
//       uint256_from_string(expectedBlock_s), // block hash
//       5864161, // block number
//       example_expected_from(), // from
//       addr_from_string(expectedTo_s), // to
//       zero_address, // contract_address
//       {}, // input
//       EVM_SUCCESS, // status
//          0x0429d069189e0000, // value
//          0x77359400, // gas_price
//          0x5208, // gas_limit
//          uint256_from_string(expectedSigR_s), // sig_r
//          uint256_from_string(expectedSigS_s), // sig_s
//          37 // sig_v
//       };

// TEST(sign_test, rlp_matches) {
//    std::vector<uint8_t> calc_rlp = tx.rlp();
//    EXPECT_EQ(example_expected_rlp(), calc_rlp);
// }

// TEST(sign_test, tx_hash_matches) {
//    evm_uint256be calc_hash = tx.hash();
//    EXPECT_EQ(example_expected_hash(), calc_hash);
// }

void expect_match(evm_address e, evm_address t, bool should_match) {
   for (int i = 0; i < sizeof(evm_address); i++) {
      if (should_match) {
         EXPECT_EQ(e.bytes[i], t.bytes[i]);
      } else {
         EXPECT_NE(e.bytes[i], t.bytes[i]);
      }
   }
}

TEST(sign_text, ecrecover_matches) {
   com::vmware::athena::EthSign verifier;
//   evm_address calc_addr = verifier.ecrecover(tx.hash(), tx.sig_v, tx.sig_r, tx.sig_s);
   evm_uint256be testHash = uint256_from_string(expectedHash_s);
   evm_uint256be testSigR = uint256_from_string(expectedSigR_s);
   evm_uint256be testSigS = uint256_from_string(expectedSigS_s);
   evm_address calc_addr = verifier.ecrecover(testHash, expectedSigV-27, testSigR, testSigS);
   evm_address expected_addr = addr_from_string(expectedFrom_s);
   expect_match(expected_addr, calc_addr, true);
}

TEST(sign_test, ecrecover_no_matches) {
//   com::vmware::athena::EthTransaction badtx = tx;
//   badtx.sig_r.bytes[0] = badtx.sig_r.bytes[0]+1;
   evm_uint256be testHash = uint256_from_string(expectedHash_s);
   evm_uint256be testSigR = uint256_from_string(expectedSigR_s);
   testSigR.bytes[0] = testSigR.bytes[0]+1;
   evm_uint256be testSigS = uint256_from_string(expectedSigS_s);
   com::vmware::athena::EthSign verifier;
   evm_address calc_addr = verifier.ecrecover(testHash, expectedSigV-27, testSigR, testSigS);
//   evm_address calc_addr = verifier.ecrecover(badtx.hash(), badtx.sig_v, badtx.sig_r, badtx.sig_s);
   evm_address expected_addr = addr_from_string(expectedFrom_s);
   expect_match(expected_addr, calc_addr, false);
}

}

int main(int argc, char **argv) {
   ::testing::InitGoogleTest(&argc, argv);
   return RUN_ALL_TESTS();
}
