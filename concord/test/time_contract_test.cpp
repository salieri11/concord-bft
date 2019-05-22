// Copyright 2018 VMware, all rights reserved
/**
 * Test the concord::time::TimeContract class.
 */

#define USE_ROCKSDB
#include "time/time_contract.hpp"
#include "config/configuration_manager.hpp"
#include "consensus/kvb/BlockchainDBTypes.hpp"
#include "consensus/kvb/Comparators.h"
#include "consensus/kvb/InMemoryDBClient.h"
#include "consensus/kvb/status.hpp"
#include "ethereum/eth_kvb_storage.hpp"
#include "gtest/gtest.h"

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>

using namespace std;

using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;

namespace {

// A small shim to prevent having to set up even more framework to create a
// ReplicaImp. (TODO: move the DB interfaces out of ReplicaImp.)
//
// This shim just reads and writes to block zero in an in-memory DB.
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
    return get(0, key, outValue, outBlockId);
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
    outBlockId = 0;
    return Blockchain::Status::OK();
  }
};

static ConcordConfiguration::ParameterStatus NodeScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  *output = ((std::vector<string>*)state)->size();
  return ConcordConfiguration::ParameterStatus::VALID;
}

// The time contract initializes itself with default values for all known
// sources. This function generates a configuration object with the test sources
// named.
ConcordConfiguration TestConfiguration(std::vector<string> sourceIDs) {
  ConcordConfiguration config;
  config.declareScope("node", "Node scope", NodeScopeSizer, &sourceIDs);
  config.subscope("node");
  config.instantiateScope("node");

  int i = 0;
  for (std::string name : sourceIDs) {
    ConcordConfiguration& nodeScope = config.subscope("node", i);
    nodeScope.declareParameter("time_source_id", "Time Source ID");
    nodeScope.loadValue("time_source_id", name);
    i++;
  }

  return config;
}

// Since we're using "median", and odd number of sources gives the most direct,
// obvious answer.
TEST(time_contract_test, five_source_happy_path) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config = TestConfiguration({"A", "B", "C", "D", "E"});
  concord::time::TimeContract tc(storage, config);

  std::vector<std::pair<std::string, uint64_t>> samples = {
      {"A", 1}, {"B", 2}, {"C", 3}, {"D", 4}, {"E", 5}};

  for (auto s : samples) {
    tc.Update(s.first, s.second);
  }

  ASSERT_EQ(tc.GetTime(), 3);
}

// Since we're using "median", verify that an even number of sources gives the
// answer between the middle two.
TEST(time_contract_test, six_source_happy_path) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config =
      TestConfiguration({"A", "B", "C", "D", "E", "F"});
  concord::time::TimeContract tc(storage, config);

  std::vector<std::pair<std::string, uint64_t>> samples = {
      {"A", 1}, {"B", 2}, {"C", 3}, {"D", 5}, {"E", 6}, {"F", 7}};

  for (auto s : samples) {
    tc.Update(s.first, s.second);
  }

  ASSERT_EQ(tc.GetTime(), 4);
}

// Verify that a single source moves forward as expected
TEST(time_contract_test, source_moves_forward) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config = TestConfiguration({"baz"});

  std::string source_id = "baz";

  for (uint64_t fake_time = 1; fake_time < 10; fake_time++) {
    concord::time::TimeContract tc(storage, config);
    ASSERT_EQ(tc.Update(source_id, fake_time), fake_time);
  }
}

// Verify that time is saved and restored correctly
TEST(time_contract_test, save_restore) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config = TestConfiguration({"foo", "bar", "baz", "qux"});

  std::string source_foo = "foo";
  std::string source_bar = "bar";
  std::string source_baz = "baz";
  std::string source_qux = "qux";

  uint64_t expected_time;
  {
    concord::time::TimeContract tc(storage, config);
    tc.Update(source_foo, 12345);
    tc.Update(source_bar, 54321);
    tc.Update(source_baz, 10293);
    tc.Update(source_qux, 48576);
    expected_time = tc.GetTime();
  }

  // It's not actually necessary to push tc out of scope, since a new
  // TimeContract object would reinitialize itself from storage anyway, but
  // we've done so for completeness, and now we get to reuse the name anyway.

  concord::time::TimeContract tc(storage, config);
  ASSERT_EQ(tc.GetTime(), expected_time);
}

// Verify that the correct source is updated.
TEST(time_contract_test, update_correct_source) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config = TestConfiguration({"A", "B", "C"});

  // The idea here is to exploit the fact that the median of a three-reading
  // system will always be equal to one of those three reading. So, by asserting
  // that the resulting summary is equal to a particular reading, we can assert
  // that it was that reading that changed.

  std::string source_A = "A";
  std::string source_B = "B";
  std::string source_C = "C";

  concord::time::TimeContract tc(storage, config);
  tc.Update(source_A, 1);
  tc.Update(source_B, 10);
  tc.Update(source_C, 20);

  // sanity: B is the median
  ASSERT_EQ(tc.GetTime(), 10);

  // directly observe the median reading (B) being updated
  ASSERT_EQ(tc.Update(source_B, 11), 11);

  // first move one of the other values, then make it the median
  ASSERT_EQ(tc.Update(source_C, 21), 11);
  // either A or B moved, because the new summary is C's value
  ASSERT_EQ(tc.Update(source_A, 30), 21);

  // and one more leapfrog, either B or C moved, because the summary is A
  ASSERT_EQ(tc.Update(source_B, 40), 30);
}

// Verify that a source cannot move its own time backward.
TEST(time_contract_test, prevent_source_rollback) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config = TestConfiguration({"foo"});

  std::string source_foo = "foo";

  concord::time::TimeContract tc1(storage, config);
  const uint64_t first_time = tc1.Update(source_foo, 1000);

  // first make sure a source can't rollback a cached copy
  const uint64_t second_time = tc1.Update(source_foo, 500);
  ASSERT_EQ(second_time, first_time);

  // then make sure a fresh read is also protected
  concord::time::TimeContract tc2(storage, config);
  const uint64_t third_time = tc2.Update(source_foo, 250);
  ASSERT_EQ(third_time, first_time);
}

// Only accept times from preconfigured sources.
TEST(time_contract_test, ignore_unknown_source) {
  TestStorage database;
  concord::ethereum::EthKvbStorage storage(database, &database, 0);
  ConcordConfiguration config = TestConfiguration({"A", "B", "C"});

  concord::time::TimeContract tc1(storage, config);
  tc1.Update("X", 1000);
  tc1.Update("Y", 1000);
  tc1.Update("Z", 1000);

  // Config specified A,B,C as sources, so all of X,Y,Z updates should be
  // ignored.
  ASSERT_EQ(tc1.GetTime(), 0);
}

}  // end namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  log4cplus::Hierarchy& hierarchy = log4cplus::Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  log4cplus::BasicConfigurator config(hierarchy, false);
  config.configure();
  return RUN_ALL_TESTS();
}
