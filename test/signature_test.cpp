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

// this is derived from expectedRLP_s: as per EIP-155, replace v,r,s with CHAIN_ID,0,0, and that changes the list-length bytes on the front
const string expectedNoSigRLP_s("0xeb19847735940082520894f6c3fff0b77efe806fcc10176b8cbf71c6dfe3be880429d069189e000080018080");
const string expectedRLP_s("0xf86b19847735940082520894f6c3fff0b77efe806fcc10176b8cbf71c6dfe3be880429d069189e00008025a0141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397a07fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38");
const string expectedHash_s("0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68");
const string expectedFrom_s("0x42c4f19a097955ff2a013ef8f014977f4e8516c3");
const string expectedBlock_s("0x062092761067b1381cb544b581be2afeb9fe75dc00ce502932b53cebc40d81b1");
const string expectedTo_s("0xf6c3fff0b77efe806fcc10176b8cbf71c6dfe3be");
const string expectedSigR_s("0x141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397");
const string expectedSigS_s("0x7fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38");
const int8_t expectedSigV = 37;

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

/**
 * See https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md. The V
 * signature value used to be [0,1]+27, but since block 2675000 is
 * [0,1]+(2*CHAIN_ID)+35. This example block is on CHAIN_ID=1 well after the
 * FORK_BLKNUM, so [0,1]+2*1+35 == [0,1]+37.
 */
int8_t correctSigV() {
   return expectedSigV - 37;
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

std::string addr_to_string(evm_address a) {
   static const char hexes[] = "0123456789abcdef";
   std::string out;
   for (int i = 0; i < sizeof(evm_address); i++) {
      out.append(hexes+(a.bytes[i] >> 4), 1)
         .append(hexes+(a.bytes[i] & 0x0f), 1);
   }
   return out;
}

std::string hash_to_string(evm_uint256be a) {
   static const char hexes[] = "0123456789abcdef";
   std::string out;
   for (int i = 0; i < sizeof(evm_uint256be); i++) {
      out.append(hexes+(a.bytes[i] >> 4), 1)
         .append(hexes+(a.bytes[i] & 0x0f), 1);
   }
   return out;
}

void expect_match(const evm_address e, const evm_address t, bool should_match) {
   bool matches = true;
   for (int i = 0; i < sizeof(evm_address); i++) {
      matches = matches & (e.bytes[i] == t.bytes[i]);
   }

   EXPECT_TRUE(matches == should_match) << "Expected: " << addr_to_string(e) << std::endl << "   Found: " << addr_to_string(t);
}

void expect_match(const evm_uint256be e, const evm_uint256be t, bool should_match) {
   bool matches = true;
   for (int i = 0; i < sizeof(evm_uint256be); i++) {
      matches = matches & (e.bytes[i] == t.bytes[i]);
   }

   EXPECT_TRUE(matches == should_match) << "Expected: " << hash_to_string(e) << std::endl << "   Found: " << hash_to_string(t);
}

evm_uint256be prefixHash(evm_uint256be rawHash) {
   // "32" is the length of the message being signed, in this case a hash
   static const std::string prefix("\x19""Ethereum Signed Message:\n32");
   std::vector<uint8_t> prefixedHash;
   std::copy(prefix.begin(), prefix.end(), std::back_inserter(prefixedHash));
   std::copy(rawHash.bytes, rawHash.bytes+sizeof(evm_uint256be),
             std::back_inserter(prefixedHash));
   return com::vmware::athena::EthHash::keccak_hash(prefixedHash);
}

evm_uint256be prefixHash_s(string rawHash) {
   // "32" is the length of the message being signed, in this case a hash
   assert(rawHash.size() == 64);
   static const std::string prefix("\x19""Ethereum Signed Message:\n64");
   std::vector<uint8_t> prefixedHash;
   std::copy(prefix.begin(), prefix.end(), std::back_inserter(prefixedHash));
   std::copy(rawHash.begin(), rawHash.end(),
             std::back_inserter(prefixedHash));
   return com::vmware::athena::EthHash::keccak_hash(prefixedHash);
}

TEST(sign_text, ecrecover_matches) {
   com::vmware::athena::EthSign verifier;
//   evm_address calc_addr = verifier.ecrecover(tx.hash(), tx.sig_v, tx.sig_r, tx.sig_s);
   evm_uint256be txHash = uint256_from_string(expectedHash_s);
   evm_uint256be testHash = prefixHash(txHash);
//   evm_uint256be crazyHash = prefixHash_s(expectedHash_s);
   evm_uint256be testSigR = uint256_from_string(expectedSigR_s);
   evm_uint256be testSigS = uint256_from_string(expectedSigS_s);
   evm_address calc_addr = verifier.ecrecover(testHash, correctSigV(), testSigR, testSigS);
   evm_address expected_addr = addr_from_string(expectedFrom_s);

   cout << "       txHash: " << hash_to_string(txHash) << std::endl
        << "     testHash: " << hash_to_string(testHash) << std::endl
        << "     testSigR: " << hash_to_string(testSigR) << std::endl
        << "     testSigS: " << hash_to_string(testSigS) << std::endl
        << "    calc_addr: " << addr_to_string(calc_addr) << std::endl
        << "expected_addr: " << addr_to_string(expected_addr) << std::endl;

   vector<uint8_t> rlp_no_sig = com::vmware::athena::dehex(expectedNoSigRLP_s);
   // evm_uint256be rlp_whole_hash = com::vmware::athena::EthHash::keccak_hash(rlp);
   // vector<uint8_t> rlp_no_sig;
   // std::copy(rlp.begin(), rlp.begin()+(rlp.size()-67), std::back_inserter(rlp_no_sig));
   evm_uint256be rlp_no_sig_hash = com::vmware::athena::EthHash::keccak_hash(rlp_no_sig);
   evm_address no_sig_calc_addr = verifier.ecrecover(rlp_no_sig_hash, correctSigV(), testSigR, testSigS);
   evm_uint256be prefix_rlp_no_sig_hash = prefixHash(rlp_no_sig_hash);
   evm_address prefix_no_sig_calc_addr = verifier.ecrecover(prefix_rlp_no_sig_hash, correctSigV(), testSigR, testSigS);

   cout << " nosig: " << hash_to_string(rlp_no_sig_hash) << std::endl
        << "  calc: " << addr_to_string(no_sig_calc_addr) << std::endl
        << "prefix: " << hash_to_string(prefix_rlp_no_sig_hash) << std::endl
        << " pcalc: " << addr_to_string(prefix_no_sig_calc_addr) << std::endl;

   expect_match(expected_addr, no_sig_calc_addr, true);
}

TEST(sign_test, ecrecover_no_matches) {
//   com::vmware::athena::EthTransaction badtx = tx;
//   badtx.sig_r.bytes[0] = badtx.sig_r.bytes[0]+1;
   evm_uint256be testHash = uint256_from_string(expectedHash_s);
   evm_uint256be testSigR = uint256_from_string(expectedSigR_s);
   testSigR.bytes[0] = testSigR.bytes[0]+1;
   evm_uint256be testSigS = uint256_from_string(expectedSigS_s);
   com::vmware::athena::EthSign verifier;
   evm_address calc_addr = verifier.ecrecover(testHash, correctSigV(), testSigR, testSigS);
//   evm_address calc_addr = verifier.ecrecover(badtx.hash(), badtx.sig_v, badtx.sig_r, badtx.sig_s);
   evm_address expected_addr = addr_from_string(expectedFrom_s);
   expect_match(expected_addr, calc_addr, false);
}

TEST(sign_test, ultra_basic) {
   // see SignAndRecover in cpp-ethereum/test/unittests/libdevcrypto/crypto.cpp
   const string secret_s("sec");
   std::vector<uint8_t> secret;
   std::copy(secret_s.begin(), secret_s.end(), std::back_inserter(secret));
   const evm_uint256be key = com::vmware::athena::EthHash::keccak_hash(secret);

   const string msg_s("msg");
   std::vector<uint8_t> msg;
   std::copy(msg_s.begin(), msg_s.end(), std::back_inserter(msg));
   const evm_uint256be hash = com::vmware::athena::EthHash::keccak_hash(msg);

   com::vmware::athena::EthSign verifier;
   std::vector<uint8_t> signature = verifier.sign(hash, key);

   const string sigR_s("b826808a8c41e00b7c5d71f211f005a84a7b97949d5e765831e1da4e34c9b829");
   const evm_uint256be sigR = uint256_from_string(sigR_s);
   const string sigS_s("5d2a622eee50f25af78241c1cb7cfff11bcf2a13fe65dee1e3b86fd79a4e3ed0");
   const evm_uint256be sigS = uint256_from_string(sigS_s);
   const uint8_t sigV = 0;

   uint8_t calc_v = signature[0];
   evm_uint256be calc_r;
   std::copy(signature.begin()+1, signature.begin()+33, calc_r.bytes);
   evm_uint256be calc_s;
   std::copy(signature.begin()+33, signature.end(), calc_s.bytes);

   std::cout << "Checking signature:" << std::endl;
   expect_match(sigR, calc_r, true);
   expect_match(sigS, calc_s, true);
   EXPECT_EQ(sigV, calc_v);
   std::cout << "Done checking signature" << std::endl;

   const string expectedPub_s("e40930c838d6cca526795596e368d16083f0672f4ab61788277abfa23c3740e1cc84453b0b24f49086feba0bd978bb4446bae8dff1e79fcc1e9cf482ec2d07c3");
   std::vector<uint8_t> expectedPub = com::vmware::athena::dehex(expectedPub_s);
   const evm_uint256be expectedHash = com::vmware::athena::EthHash::keccak_hash(expectedPub);
   evm_address expectedAddr;
   std::copy(expectedHash.bytes+(sizeof(evm_uint256be)-sizeof(evm_address)),
             expectedHash.bytes+sizeof(evm_uint256be),
             expectedAddr.bytes);

   evm_address calc_addr = verifier.ecrecover(hash, sigV, sigR, sigS);

   expect_match(expectedAddr, calc_addr, true);
}

TEST(sign_test, medium_basic) {
   // See https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f
   const string given_rlpHash_s("0x6a74f15f29c3227c5d1d2e27894da58d417a484ef53bc7aa57ee323b42ded656");
   const string given_v_s("0x1c");
   const string given_r_s("0x668ed6500efd75df7cb9c9b9d8152292a75453ec2d11030b0eec42f6a7ace602");
   const string given_s_s("0x3efcbbf4d53e0dfa4fde5c6d9a73221418652abc66dff7fddd78b81cc28b9fbf");
   const string given_from_s("0x53ae893e4b22d707943299a8d0c844df0e3d5557");

   evm_uint256be given_rlpHash = uint256_from_string(given_rlpHash_s);
   uint8_t given_v = 1; // pre-converted
   evm_uint256be given_r = uint256_from_string(given_r_s);
   evm_uint256be given_s = uint256_from_string(given_s_s);
   evm_address given_from = addr_from_string(given_from_s);

   com::vmware::athena::EthSign verifier;
   evm_address from = verifier.ecrecover(given_rlpHash, given_v, given_r, given_s);

   expect_match(given_from, from, true);

   const string given_message_s("0xe6808504a817c800830186a094687422eea2cb73b5d3e242ba5456b782919afc858203e882c0de");
   vector<uint8_t> given_message = com::vmware::athena::dehex(given_message_s);
   evm_uint256be message_hash = com::vmware::athena::EthHash::keccak_hash(given_message);
   std::cout << "Checking RLP Hash" << std::endl;
   expect_match(given_rlpHash, message_hash, true);

   // evm_uint256be prefix_message_hash = prefixHash(message_hash);
   // std::cout << "Checking Prefix RLP Hash" << std::endl;
   // expect_match(given_rlpHash, prefix_message_hash, true);
}

}

int main(int argc, char **argv) {
   ::testing::InitGoogleTest(&argc, argv);
   return RUN_ALL_TESTS();
}
