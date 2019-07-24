#define USE_ROCKSDB

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include "concord.pb.h"
#include "consensus/sliver.hpp"
#include "consensus/status.hpp"
#include "gtest/gtest.h"
#include "hlf/chaincode_invoker.hpp"
#include "hlf/kvb_storage.hpp"
#include "storage/blockchain_db_types.h"
#include "storage/comparators.h"
#include "storage/in_memory_db_client.h"

using namespace std;
using namespace log4cplus;

using com::vmware::concord::HlfRequest;
using concord::hlf::ChaincodeInvoker;
using concord::storage::IDBClient;
using concord::storage::InMemoryDBClient;
using concord::storage::RocksKeyComparator;

namespace {
const string kTestHlfPeerToolPath1 = "test/peer1";
const string kTestHlfPeerToolPath2 = "test/peer2";

ChaincodeInvoker chaincode_invoker(kTestHlfPeerToolPath1);
Logger* logger = nullptr;

// Define TestStorage
class TestStorage : public concord::storage::ILocalKeyValueStorageReadOnly,
                    public concord::storage::IBlocksAppender {
 private:
  InMemoryDBClient db_ = InMemoryDBClient(
      (IDBClient::KeyComparator)&RocksKeyComparator::InMemKeyComp);

 public:
  concord::consensus::Status get(
      concord::storage::Key key,
      concord::storage::Value& outValue) const override {
    concord::storage::BlockId outBlockId;
    return get(0, key, outValue, outBlockId);
  }

  concord::consensus::Status get(
      concord::storage::BlockId readVersion, concord::consensus::Sliver key,
      concord::consensus::Sliver& outValue,
      concord::storage::BlockId& outBlock) const override {
    outBlock = 0;
    return db_.get(key, outValue);
  }

  concord::storage::BlockId getLastBlock() const override { return 0; }

  concord::consensus::Status getBlockData(
      concord::storage::BlockId blockId,
      concord::storage::SetOfKeyValuePairs& outBlockData) const override {
    EXPECT_TRUE(false) << "Test should not cause getBlockData to be called";
    return concord::consensus::Status::IllegalOperation(
        "getBlockData not supported in test");
  }

  concord::consensus::Status mayHaveConflictBetween(
      concord::consensus::Sliver key, concord::storage::BlockId fromBlock,
      concord::storage::BlockId toBlock, bool& outRes) const override {
    EXPECT_TRUE(false)
        << "Test should not cause mayHaveConflictBetween to be called";
    return concord::consensus::Status::IllegalOperation(
        "mayHaveConflictBetween not supported in test");
  }

  concord::storage::ILocalKeyValueStorageReadOnlyIterator* getSnapIterator()
      const override {
    EXPECT_TRUE(false) << "Test should not cause getSnapIterator to be called";
    return nullptr;
  }

  concord::consensus::Status freeSnapIterator(
      concord::storage::ILocalKeyValueStorageReadOnlyIterator* iter)
      const override {
    EXPECT_TRUE(false) << "Test should not cause freeSnapIterator to be called";
    return concord::consensus::Status::IllegalOperation(
        "freeSnapIterator not supported in test");
  }

  void monitor() const override {
    EXPECT_TRUE(false) << "Test should not cause monitor to be called";
  }

  concord::consensus::Status addBlock(
      const concord::storage::SetOfKeyValuePairs& updates,
      concord::storage::BlockId& outBlockId) override {
    concord::consensus::Status status = db_.multiPut(updates);
    outBlockId = 0;
    return status;
  }
};

// Single unit test for chaincode invoker
TEST(hlf_test, chaincode_invoker_peer_command_tool) {
  // Verify constructor
  ASSERT_EQ(kTestHlfPeerToolPath1, chaincode_invoker.GetHlfPeerTool());

  // Call and verify set function
  chaincode_invoker.SetHlfPeerTool(kTestHlfPeerToolPath2);
  ASSERT_EQ(kTestHlfPeerToolPath2, chaincode_invoker.GetHlfPeerTool());
}

// Unit tests for KVB HLF Storage

TEST(hlf_test, hlf_kvb_storage_write_block) {
  TestStorage test_storage;
  concord::hlf::HlfKvbStorage kvb_hlf_storage(test_storage, &test_storage);
  EXPECT_TRUE(kvb_hlf_storage.WriteHlfBlock().isOK());
}

TEST(hlf_test, hlf_kvb_storage_get) {
  TestStorage test_storage;
  concord::hlf::HlfKvbStorage kvb_hlf_storage(test_storage, &test_storage);
  ASSERT_EQ("", kvb_hlf_storage.GetHlfState("mycc-key"));
}

TEST(hlf_test, hlf_kvb_storage_put) {
  TestStorage test_storage;
  concord::hlf::HlfKvbStorage kvb_hlf_storage(test_storage, &test_storage);
  EXPECT_TRUE(kvb_hlf_storage.SetHlfState("mycc-key", "abcefg").isOK());
}

TEST(hlf_test, hlf_kvb_storage_add_tx) {
  TestStorage test_storage;
  concord::hlf::HlfKvbStorage kvb_hlf_storage(test_storage, &test_storage);
  // mock up HLFRequest
  HlfRequest hlf_request;
  hlf_request.set_input("input");
  hlf_request.set_version("version");
  hlf_request.set_chaincode_name("chaincode_name");
  hlf_request.set_chain_id("chain_id");
  EXPECT_TRUE(kvb_hlf_storage.AddHlfTransaction(hlf_request).isOK());
}

};  // namespace

int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);

  logger = new Logger(Logger::getInstance("com.vmware.test"));
  initialize();
  Hierarchy& hierarchy = Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  BasicConfigurator config(hierarchy, false);
  config.configure();

  return RUN_ALL_TESTS();
}
