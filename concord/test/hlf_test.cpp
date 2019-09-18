// Copyright 2019 VMware, all rights reserved
//

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include "blockchain/db_adapter.h"
#include "blockchain/db_types.h"
#include "concord.pb.h"
#include "gtest/gtest.h"
#include "hlf/chaincode_invoker.hpp"
#include "hlf/kvb_storage.hpp"
#include "memorydb/client.h"
#include "memorydb/key_comparator.h"
#include "sliver.hpp"
#include "status.hpp"

using namespace std;
using namespace log4cplus;

using com::vmware::concord::HlfRequest;
using concord::hlf::ChaincodeInvoker;
using concord::storage::IDBClient;
using concord::storage::blockchain::IBlocksAppender;
using concord::storage::blockchain::ILocalKeyValueStorageReadOnly;
using concord::storage::blockchain::ILocalKeyValueStorageReadOnlyIterator;
using concord::storage::blockchain::KeyManipulator;
using concord::storage::memorydb::Client;
using concord::storage::memorydb::KeyComparator;
using concordUtils::BlockId;
using concordUtils::Key;
using concordUtils::SetOfKeyValuePairs;
using concordUtils::Sliver;
using concordUtils::Status;
using concordUtils::Value;

namespace {
const string kTestHlfPeerToolPath1 = "test/peer1";
const string kTestHlfPeerToolPath2 = "test/peer2";

ChaincodeInvoker chaincode_invoker(kTestHlfPeerToolPath1);
Logger* logger = nullptr;

// Define TestStorage
class TestStorage : public ILocalKeyValueStorageReadOnly,
                    public IBlocksAppender {
 private:
  KeyComparator comp = KeyComparator(new KeyManipulator());
  Client db_ = Client(comp);

 public:
  Status get(Key key, Value& outValue) const override {
    BlockId outBlockId;
    return get(0, key, outValue, outBlockId);
  }

  Status get(BlockId readVersion, Sliver key, Sliver& outValue,
             BlockId& outBlock) const override {
    outBlock = 0;
    return db_.get(key, outValue);
  }

  BlockId getLastBlock() const override { return 0; }

  Status getBlockData(BlockId blockId,
                      SetOfKeyValuePairs& outBlockData) const override {
    EXPECT_TRUE(false) << "Test should not cause getBlockData to be called";
    return Status::IllegalOperation("getBlockData not supported in test");
  }

  Status mayHaveConflictBetween(Sliver key, BlockId fromBlock, BlockId toBlock,
                                bool& outRes) const override {
    EXPECT_TRUE(false)
        << "Test should not cause mayHaveConflictBetween to be called";
    return Status::IllegalOperation(
        "mayHaveConflictBetween not supported in test");
  }

  ILocalKeyValueStorageReadOnlyIterator* getSnapIterator() const override {
    EXPECT_TRUE(false) << "Test should not cause getSnapIterator to be called";
    return nullptr;
  }

  Status freeSnapIterator(
      ILocalKeyValueStorageReadOnlyIterator* iter) const override {
    EXPECT_TRUE(false) << "Test should not cause freeSnapIterator to be called";
    return Status::IllegalOperation("freeSnapIterator not supported in test");
  }

  void monitor() const override {
    EXPECT_TRUE(false) << "Test should not cause monitor to be called";
  }

  Status addBlock(const SetOfKeyValuePairs& updates,
                  BlockId& outBlockId) override {
    Status status = db_.multiPut(updates);
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
