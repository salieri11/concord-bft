// Copyright 2020 VMware, all rights reserved

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <boost/lockfree/spsc_queue.hpp>
#include "blockchain/db_adapter.h"
#include "blockchain/db_interfaces.h"
#include "config/configuration_manager.hpp"
#include "gtest/gtest.h"
#include "hash_defs.h"
#include "memorydb/client.h"
#include "memorydb/key_comparator.h"
#include "status.hpp"
#include "time/time_contract.hpp"

#include <cassert>
#include <exception>
#include <memory>
#include <string>
#include <vector>
#include "storage/kvb_app_filter.h"

using boost::lockfree::spsc_queue;
using com::vmware::concord::kvb::ValueWithTrids;
using concord::storage::InvalidBlockRange;
using concord::storage::kKvbKeyDaml;
using concord::storage::kKvbKeyEthBlock;
using concord::storage::KvbAppFilter;
using concord::storage::KvbReadError;
using concord::storage::SetOfKeyValuePairs;
using concord::storage::blockchain::BlockId;
using concord::storage::blockchain::ILocalKeyValueStorageReadOnly;
using concord::storage::blockchain::ILocalKeyValueStorageReadOnlyIterator;
using concord::storage::blockchain::Key;
using concord::storage::blockchain::Status;
using concord::storage::blockchain::Value;
using concord::storage::memorydb::Sliver;
using std::chrono_literals::operator""ms;
typedef std::pair<concordUtils::BlockId, concordUtils::SetOfKeyValuePairs>
    KvbUpdate;
namespace {

constexpr auto kLastBlockId = BlockId{150};

class FakeStorage : public ILocalKeyValueStorageReadOnly {
 public:
  std::vector<KvbUpdate> data_;

  Status get(const Key&, Value&) const override {
    ADD_FAILURE() << "get() should not be called by this test";
    return Status::IllegalOperation("get() should not be called by this test");
  }

  Status get(BlockId, const Sliver&, Sliver&, BlockId&) const override {
    ADD_FAILURE() << "get() should not be called by this test";
    return Status::IllegalOperation("get() should not be called by this test");
  }

  BlockId getLastBlock() const override { return blockId_; }

  Status getBlockData(BlockId b_id, SetOfKeyValuePairs& kvp) const override {
    if (b_id >= 0 && b_id < blockId_) {
      kvp = data_.at(b_id).second;
      return Status::OK();
    }
    return Status::IllegalOperation("block id not reachable");
  }

  Status mayHaveConflictBetween(const Sliver&, BlockId, BlockId,
                                bool&) const override {
    ADD_FAILURE()
        << "mayHaveConflictBetween() should not be called by this test";
    return Status::IllegalOperation(
        "mayHaveConflictBetween() not supported in test mode");
  }

  ILocalKeyValueStorageReadOnlyIterator* getSnapIterator() const override {
    ADD_FAILURE() << "getSnapIterator() should not be called by this test";
    return nullptr;
  }

  Status freeSnapIterator(
      ILocalKeyValueStorageReadOnlyIterator*) const override {
    ADD_FAILURE() << "freeSnapIterator() should not be called by this test";
    return Status::IllegalOperation(
        "freeSnapIterator() not supported in test mode");
  }

  void monitor() const override {
    ADD_FAILURE() << "monitor() should not be called by this test";
  }

  string concatKeyType(string key, char keyType) {
    std::string str1;
    str1.push_back(keyType);
    str1 += key;
    return str1;
  }

  // Dummy method to fill DB for testing, each client id can watch only the
  // block id that equals to his client id.
  // each KvbUpdate will hold this kind of pair:
  // <blockId,<"0Key",ValueWithTrids that only the clientID == blockId is able
  // to watch>>
  void fillWithData(BlockId num_of_blocks) {
    blockId_ = num_of_blocks;
    std::string str1 = concatKeyType("Key", kKvbKeyDaml);
    Sliver key1(std::move(str1));
    for (BlockId i = 0; i <= num_of_blocks; i++) {
      ValueWithTrids proto;
      proto.add_trid(std::to_string(i));
      proto.set_value("TridVal" + std::to_string(i));
      Sliver buf(proto.SerializeAsString());
      const SetOfKeyValuePairs block{std::pair{key1, buf}};
      KvbUpdate t{std::pair{i, block}};
      data_.push_back(t);
    }
  }

 private:
  BlockId blockId_{kLastBlockId};
};

TEST(kvbfilter_test, kvbfilter_update_success) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "Ke";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  std::string expected_key1 = storage.concatKeyType("Key1", kKvbKeyDaml);
  Sliver key1(storage.concatKeyType("Key1", kKvbKeyDaml));
  std::string expected_key2 = storage.concatKeyType("Key2", kKvbKeyDaml);
  Sliver key2(storage.concatKeyType("Key2", kKvbKeyDaml));

  ValueWithTrids proto1, proto2;
  proto1.add_trid("0");
  proto1.add_trid("1");
  proto1.set_value("TridVal1");
  proto2.add_trid("0");
  proto2.set_value("TridVal2");
  std::string serialized = proto1.SerializeAsString();
  Sliver buf1(std::move(serialized));
  serialized = proto2.SerializeAsString();
  Sliver buf2(std::move(serialized));
  const SetOfKeyValuePairs block{
      {std::make_pair(key1, buf1), std::make_pair(key2, buf2)}};
  BlockId block_id = 0;

  auto filtered = kvb_filter.FilterUpdate(std::make_pair(block_id, block));

  EXPECT_EQ(filtered.first, block_id);
  SetOfKeyValuePairs filtered_kv_pairs = filtered.second;
  EXPECT_EQ(filtered_kv_pairs.size(), 1);
  for (const auto& [key, val] : filtered_kv_pairs) {
    EXPECT_EQ(key.toString(), expected_key1.substr(1));
    EXPECT_EQ(val.toString(), proto1.value());
  }
};

TEST(kvbfilter_test, kvbfilter_update_message_prefix_only_one_matched) {
  FakeStorage storage;
  int client_id = 0;
  string key_prefix = "Ke";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  std::string expected_key = storage.concatKeyType("Rey", kKvbKeyDaml);
  Sliver key(storage.concatKeyType("Rey", kKvbKeyDaml));
  std::string expected_key1 = storage.concatKeyType("Key", kKvbKeyDaml);
  Sliver key1(storage.concatKeyType("Key", kKvbKeyDaml));
  ValueWithTrids proto;
  proto.add_trid("0");
  proto.set_value("TridVal");
  std::string serialized = proto.SerializeAsString();
  Sliver buf(std::move(serialized));
  const SetOfKeyValuePairs block{
      {std::make_pair(key, buf), std::make_pair(key1, buf)}};
  BlockId block_id = 0;

  auto filtered = kvb_filter.FilterUpdate(std::make_pair(block_id, block));

  EXPECT_EQ(filtered.first, block_id);
  SetOfKeyValuePairs filtered_kv_pairs = filtered.second;
  EXPECT_EQ(filtered_kv_pairs.size(), 1);
  for (const auto& [key, val] : filtered_kv_pairs) {
    EXPECT_EQ(key.toString(), expected_key1.substr(1));
    EXPECT_EQ(val.toString(), proto.value());
  }
};

TEST(kvbfilter_test, kvbfilter_update_message_empty_prefix) {
  FakeStorage storage;
  int client_id = 0;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  std::unordered_set<std::string> expected_key_set;
  expected_key_set.insert(storage.concatKeyType("Rey", kKvbKeyDaml));
  expected_key_set.insert(storage.concatKeyType("Key", kKvbKeyDaml));
  Sliver key(storage.concatKeyType("Rey", kKvbKeyDaml));
  Sliver key1(storage.concatKeyType("Key", kKvbKeyDaml));
  ValueWithTrids proto;
  proto.add_trid("0");
  proto.set_value("TridVal");
  std::string serialized = proto.SerializeAsString();
  Sliver buf(std::move(serialized));
  const SetOfKeyValuePairs block{
      {std::make_pair(key, buf), std::make_pair(key1, buf)}};
  BlockId block_id = 0;

  auto filtered = kvb_filter.FilterUpdate(std::make_pair(block_id, block));

  EXPECT_EQ(filtered.first, block_id);
  SetOfKeyValuePairs filtered_kv_pairs = filtered.second;
  EXPECT_EQ(filtered_kv_pairs.size(), 2);
  int index = 0;
  for (const auto& [key, val] : filtered_kv_pairs) {
    EXPECT_TRUE(expected_key_set.count(
        storage.concatKeyType(key.toString(), kKvbKeyDaml)));
    EXPECT_EQ(val.toString(), proto.value());
    index++;
  }
};

TEST(kvbfilter_test, kvbfilter_update_client_has_no_trids) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  Sliver key(storage.concatKeyType("Rey", kKvbKeyDaml));
  Sliver key1(storage.concatKeyType("Key", kKvbKeyDaml));
  ValueWithTrids proto;
  proto.add_trid("0");
  proto.set_value("TridVal");
  std::string serialized = proto.SerializeAsString();
  Sliver buf(std::move(serialized));
  const SetOfKeyValuePairs block{
      {std::make_pair(key, buf), std::make_pair(key1, buf)}};
  BlockId block_id = 0;

  auto filtered = kvb_filter.FilterUpdate(std::make_pair(block_id, block));

  EXPECT_EQ(filtered.first, block_id);
  SetOfKeyValuePairs filtered_kv_pairs = filtered.second;
  EXPECT_EQ(filtered_kv_pairs.size(), 0);
};

TEST(kvbfilter_test, kvbfilter_hash_update_success) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  Sliver key(storage.concatKeyType("Rey", kKvbKeyDaml));
  Sliver key1(storage.concatKeyType("Key", kKvbKeyDaml));
  ValueWithTrids proto;
  proto.add_trid("0");
  proto.set_value("TridVal");
  std::string serialized = proto.SerializeAsString();
  Sliver buf(std::move(serialized));
  const SetOfKeyValuePairs block{
      {std::make_pair(key, buf), std::make_pair(key1, buf)}};
  BlockId block_id = 0;

  auto return_hash_val = kvb_filter.HashUpdate(std::make_pair(block_id, block));

  // make hash value for check
  size_t expected_hash_val = std::hash<string>{}(std::to_string(block_id));
  expected_hash_val ^= std::hash<string>{}(string{key.data(), key.length()}) ^
                       std::hash<string>{}(string{buf.data(), buf.length()});
  expected_hash_val ^= std::hash<string>{}(string{key1.data(), key1.length()}) ^
                       std::hash<string>{}(string{buf.data(), buf.length()});
  EXPECT_EQ(return_hash_val, expected_hash_val);
};

TEST(kvbfilter_test, kvbfilter_success_get_blocks_in_range) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  storage.fillWithData(kLastBlockId);
  BlockId block_id_start = 0;
  BlockId block_id_end = 10;
  KvbUpdate temporary;
  spsc_queue<KvbUpdate> queue_out{storage.getLastBlock()};
  std::atomic_bool stop_exec = false;
  kvb_filter.ReadBlockRange(block_id_start, block_id_end, queue_out, stop_exec);
  std::vector<KvbUpdate> filtered_kv_pairs;
  while (queue_out.pop(temporary)) {
    filtered_kv_pairs.push_back(temporary);
  }
  EXPECT_EQ(filtered_kv_pairs.size(), block_id_end - block_id_start + 1);
  for (int i = 0; i < filtered_kv_pairs.size(); i++) {
    auto x = filtered_kv_pairs.at(i);
    if (i != client_id)
      EXPECT_EQ(x.second.size(), 0);
    else
      EXPECT_EQ(x.second.size(), 1);
  }
}

TEST(kvbfilter_test, kvbfilter_stop_exec_in_the_middle) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  storage.fillWithData(1000);
  BlockId block_id = 1;
  KvbUpdate temporary;
  spsc_queue<KvbUpdate> queue_out{storage.getLastBlock()};
  std::atomic_bool stop_exec = false;
  auto kvb_reader =
      std::async(std::launch::async, &KvbAppFilter::ReadBlockRange, kvb_filter,
                 0, kLastBlockId, std::ref(queue_out), std::ref(stop_exec));
  std::this_thread::sleep_for(2ms);
  stop_exec = true;
  kvb_reader.get();
  int num_of_blocks = 0;
  while (queue_out.pop(temporary)) {
    num_of_blocks++;
  }
  EXPECT_GT(num_of_blocks, 0);
  EXPECT_LE(num_of_blocks, 1000);
};

TEST(kvbfilter_test, kvbfilter_block_out_of_range) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  BlockId block_id = kLastBlockId + 5;
  KvbUpdate temporary;
  spsc_queue<KvbUpdate> queue_out{storage.getLastBlock()};
  std::atomic_bool stop_exec = false;
  EXPECT_THROW(
      kvb_filter.ReadBlockRange(block_id, block_id, queue_out, stop_exec);
      , KvbReadError);
};

TEST(kvbfilter_test, kvbfilter_end_block_greater_then_start_block) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  storage.fillWithData(kLastBlockId);
  BlockId block_id_end = 0;
  BlockId block_id_start = 10;
  KvbUpdate temporary;
  spsc_queue<KvbUpdate> queue_out{storage.getLastBlock()};
  std::atomic_bool stop_exec = false;
  EXPECT_THROW(kvb_filter.ReadBlockRange(block_id_start, block_id_end,
                                         queue_out, stop_exec);
               , InvalidBlockRange);
};

TEST(kvbfilter_test, kvbfilter_success_hash_of_blocks_in_range) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  storage.fillWithData(kLastBlockId);

  // add Trid 1 to watch on another value -> now Trid 1 watches on 2 values
  std::string str1 = storage.concatKeyType("Key", kKvbKeyDaml);
  Sliver key1(std::move(str1));
  ValueWithTrids proto;
  proto.add_trid(std::to_string(2));
  proto.add_trid(std::to_string(client_id));
  proto.set_value("TridVal" + std::to_string(2));
  std::string serialized = proto.SerializeAsString();
  Sliver buf(std::move(serialized));
  const SetOfKeyValuePairs block{std::pair{key1, buf}};
  KvbUpdate t{std::pair{2, block}};
  storage.data_.at(2) = t;

  BlockId block_id_start = 0;
  BlockId block_id_end = 10;
  KvbUpdate temporary;
  spsc_queue<KvbUpdate> queue_out{storage.getLastBlock()};
  std::atomic_bool stop_exec = false;
  kvb_filter.ReadBlockRange(block_id_start, block_id_end, queue_out, stop_exec);
  std::vector<KvbUpdate> filtered_kv_pairs;
  while (queue_out.pop(temporary)) {
    filtered_kv_pairs.push_back(temporary);
  }

  auto hash_value = kvb_filter.ReadBlockRangeHash(block_id_start, block_id_end);
  size_t expected_hash_val = 0;
  for (int i = block_id_start; i <= block_id_end; i++) {
    expected_hash_val ^= kvb_filter.HashUpdate(filtered_kv_pairs.at(i));
  }

  EXPECT_EQ(hash_value, expected_hash_val);
}

TEST(kvbfilter_test, kvbfilter_success_hash_of_block) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  storage.fillWithData(kLastBlockId);
  BlockId block_id_start = 1;

  auto hash_value = kvb_filter.ReadBlockHash(block_id_start);
  auto v = kvb_filter.FilterUpdate(storage.data_[1]);
  size_t expected_hash_val = 0;

  expected_hash_val ^= std::hash<string>{}(std::to_string(v.first));
  for (const auto& [key, value] : v.second) {
    auto key_hash = std::hash<string>{}(string{key.data(), key.length()});
    key_hash ^= std::hash<string>{}(string{value.data(), value.length()});
    expected_hash_val ^= key_hash;
  }

  EXPECT_EQ(hash_value, expected_hash_val);
}

TEST(kvbfilter_test, kvbfilter_hash_filter_block_out_of_range) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  storage.fillWithData(kLastBlockId);
  BlockId block_id_start = 1;
  BlockId block_id_end = kLastBlockId + 5;

  EXPECT_THROW(kvb_filter.ReadBlockRangeHash(block_id_start, block_id_end);
               , KvbReadError);
};

TEST(kvbfilter_test, kvbfilter_update_for_not_daml_prefix) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "Ke";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);
  std::string expected_key1 = storage.concatKeyType("Key", kKvbKeyEthBlock);
  Sliver key1(std::move(expected_key1));

  ValueWithTrids proto1;
  proto1.add_trid("1");
  proto1.set_value("TridVal1");
  std::string serialized = proto1.SerializeAsString();
  Sliver buf1(std::move(serialized));
  const SetOfKeyValuePairs block{{std::make_pair(key1, buf1)}};
  BlockId block_id = 0;

  auto filtered = kvb_filter.FilterUpdate(std::make_pair(block_id, block));

  EXPECT_EQ(filtered.first, block_id);
  SetOfKeyValuePairs filtered_kv_pairs = filtered.second;
  EXPECT_EQ(filtered_kv_pairs.size(), 0);
};

TEST(kvbfilter_test, kvbfilter_update_empty_kv_pair) {
  FakeStorage storage;
  int client_id = 1;
  string key_prefix = "Ke";
  auto kvb_filter = KvbAppFilter(&storage, {KvbAppFilter::AppType::kDaml},
                                 std::to_string(client_id), key_prefix);

  const SetOfKeyValuePairs block{};
  BlockId block_id = 0;

  auto filtered = kvb_filter.FilterUpdate(std::make_pair(block_id, block));

  EXPECT_EQ(filtered.first, block_id);
  SetOfKeyValuePairs filtered_kv_pairs = filtered.second;
  EXPECT_EQ(filtered_kv_pairs.size(), 0);
};

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  auto& hierarchy = log4cplus::Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  log4cplus::BasicConfigurator config{hierarchy, false};
  config.configure();
  auto logger = log4cplus::Logger::getInstance("kvb_filter_test");
  return RUN_ALL_TESTS();
}
