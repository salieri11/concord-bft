#define USE_ROCKSDB

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include "concord.pb.h"
#include "consensus/kvb/BlockchainDBTypes.hpp"
#include "consensus/kvb/Comparators.h"
#include "consensus/kvb/InMemoryDBClient.h"
#include "consensus/kvb/status.hpp"
#include "gtest/gtest.h"
#include "hlf/handler.hpp"

using namespace std;
using namespace log4cplus;

using com::vmware::concord::HlfRequest;
using concord::hlf::ChaincodeInvoker;
using concord::hlf::HlfHandler;

namespace {
string testHlfPeerToolPath1 = "test/peer1";
string testHlfPeerToolPath2 = "test/peer2";

ChaincodeInvoker chaincodeInvoker(testHlfPeerToolPath1);
HlfHandler hlf_handler(&chaincodeInvoker);
Logger* logger = nullptr;

// Define TestStorage
class TestStorage : public Blockchain::ILocalKeyValueStorageReadOnly,
                    public Blockchain::IBlocksAppender {
 private:
  Blockchain::InMemoryDBClient db_ = Blockchain::InMemoryDBClient(
      (Blockchain::IDBClient::KeyComparator)&Blockchain::RocksKeyComparator::
          InMemKeyComp);

 public:
  Blockchain::Status get(Blockchain::Key key,
                         Blockchain::Value& outValue) const override {
    Blockchain::BlockId outBlockId;
    get(0, key, outValue, outBlockId);
  }

  Blockchain::Status get(Blockchain::BlockId readVersion,
                         Blockchain::Sliver key, Blockchain::Sliver& outValue,
                         Blockchain::BlockId& outBlock) const override {
    outBlock = 0;
    return db_.get(key, outValue);
  }

  Blockchain::BlockId getLastBlock() const override { return 0; }

  Blockchain::Status getBlockData(
      Blockchain::BlockId blockId,
      Blockchain::SetOfKeyValuePairs& outBlockData) const override {
    EXPECT_TRUE(false) << "Test should not cause getBlockData to be called";
    return Blockchain::Status::IllegalOperation(
        "getBlockData not supported in test");
  }

  Blockchain::Status mayHaveConflictBetween(Blockchain::Sliver key,
                                            Blockchain::BlockId fromBlock,
                                            Blockchain::BlockId toBlock,
                                            bool& outRes) const override {
    EXPECT_TRUE(false)
        << "Test should not cause mayHaveConflictBetween to be called";
    return Blockchain::Status::IllegalOperation(
        "mayHaveConflictBetween not supported in test");
  }

  Blockchain::ILocalKeyValueStorageReadOnlyIterator* getSnapIterator()
      const override {
    EXPECT_TRUE(false) << "Test should not cause getSnapIterator to be called";
    return nullptr;
  }

  Blockchain::Status freeSnapIterator(
      Blockchain::ILocalKeyValueStorageReadOnlyIterator* iter) const override {
    EXPECT_TRUE(false) << "Test should not cause freeSnapIterator to be called";
    return Blockchain::Status::IllegalOperation(
        "freeSnapIterator not supported in test");
  }

  void monitor() const override {
    EXPECT_TRUE(false) << "Test should not cause monitor to be called";
  }

  Blockchain::Status addBlock(const Blockchain::SetOfKeyValuePairs& updates,
                              Blockchain::BlockId& outBlockId) override {
    for (auto u : updates) {
      Blockchain::Status status = db_.put(u.first, u.second);
      if (!status.isOK()) {
        return status;
      }
    }
    outBlockId = 1;
    return Blockchain::Status::OK();
  }
};

// Single unit test for chaincode invoker
TEST(hlf_test, chaincode_invoker_peer_command_tool) {
  // Verify constructor
  ASSERT_EQ(testHlfPeerToolPath1, chaincodeInvoker.GetHlfPeerTool());

  // Call and verify set function
  chaincodeInvoker.SetHlfPeerTool(testHlfPeerToolPath2);
  ASSERT_EQ(testHlfPeerToolPath2, chaincodeInvoker.GetHlfPeerTool());
}

// Unit tests for HLF handler
TEST(hlf_test, hlf_handler_kv_service_port) {
  string listenAddress = "0.0.0.0:50051";

  // service address should not be set
  ASSERT_EQ(hlf_handler.GetHlfKvService(), "");

  // set service address
  chaincodeInvoker.SetHlfKvServiceAddress(listenAddress);

  // Fetch service address by hlf handler
  ASSERT_EQ(listenAddress, hlf_handler.GetHlfKvService());
}

TEST(hlf_test, hlf_handler_kv_service_api_put) {
  TestStorage testStorage;
  concord::hlf::HlfKvbStorage kvbHlfStorage(testStorage, &testStorage, 0);
  hlf_handler.SetKvbHlfStoragePointer(&kvbHlfStorage);
  EXPECT_TRUE(hlf_handler.PutState("key1", "value1").isOK());
  hlf_handler.RevokeKvbHlfStoragePointer();
}

TEST(hlf_test, hlf_handler_kv_service_api_get) {
  TestStorage testStorage;
  concord::hlf::HlfKvbStorage kvbHlfStorage(testStorage, &testStorage, 0);

  hlf_handler.SetKvbHlfStoragePointer(&kvbHlfStorage);
  ASSERT_EQ("", hlf_handler.GetState("key1"));
  hlf_handler.RevokeKvbHlfStoragePointer();
}

TEST(hlf_test, hlf_handler_kvb_service_api_write_block) {
  TestStorage testStorage;
  concord::hlf::HlfKvbStorage kvbHlfStorage(testStorage, &testStorage, 0);

  hlf_handler.SetKvbHlfStoragePointer(&kvbHlfStorage);
  EXPECT_TRUE(hlf_handler.WriteBlock().isOK());
  hlf_handler.RevokeKvbHlfStoragePointer();
}

// Unit tests for KVB HLF Storage
TEST(hlf_test, hlf_handler_kvb_storage_add_tx) {
  TestStorage testStorage;
  concord::hlf::HlfKvbStorage kvbHlfStorage(testStorage, &testStorage, 0);
  // mock up HLFRequest
  HlfRequest hlfRequest;
  hlfRequest.set_input("input");
  hlfRequest.set_version("version");
  hlfRequest.set_chaincode_name("chaincode_name");
  hlfRequest.set_chain_id("chain_id");

  EXPECT_TRUE(kvbHlfStorage.AddHlfTransaction(hlfRequest).isOK());
}

};  // namespace

int main(int argc, char* argv[]) {
  ::testing::InitGoogleTest(&argc, argv);

  logger = new Logger(Logger::getInstance("com.vmware.test"));
  initialize();
  Hierarchy& hierarchy = Logger::getDefaultHierarchy();
  // hierarchy.disableDebug();
  BasicConfigurator config(hierarchy, false);
  config.configure();

  return RUN_ALL_TESTS();
}
