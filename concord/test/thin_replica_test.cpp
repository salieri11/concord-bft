// Copyright 2020 VMware, all rights reserved

#include <grpcpp/impl/codegen/server_context.h>
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include <iterator>
#include <map>
#include <sstream>
#include <string>
#include "../src/thin_replica/grpc_services.hpp"

#include "kv_types.hpp"
#include "thin_replica/subscription_buffer.hpp"
#include "thin_replica/thin_replica_impl.hpp"

#include "config/configuration_manager.hpp"
#include "db_interfaces.h"
#include "gtest/gtest.h"

namespace {

using concord::kvbc::BlockId;
using concord::kvbc::Key;
using concord::kvbc::SetOfKeyValuePairs;
using concord::kvbc::Value;
using concordUtils::Sliver;

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;

using concord::kvbc::ILocalKeyValueStorageReadOnly;
using concordUtils::Status;

using concord::thin_replica::SubUpdateBuffer;

using Block = std::pair<BlockId, SetOfKeyValuePairs>;
using BlockMap = std::map<BlockId, SetOfKeyValuePairs>;

constexpr uint64_t kLastBlockId{5u};

Block generate_block(BlockId block_id) {
  return {block_id,
          SetOfKeyValuePairs{
              {std::string{"key block#"} + std::to_string(block_id),
               std::string{"value block#"} + std::to_string(block_id)}}};
}

BlockMap generate_kvp(BlockId start, BlockId end) {
  BlockMap blocks;
  for (BlockId block = start; block <= end; ++block) {
    blocks.emplace(generate_block(block));
  }
  return blocks;
}

class FakeStorage : public ILocalKeyValueStorageReadOnly {
 public:
  FakeStorage(BlockMap&& db) : db_(std::move(db)) {}

  void addBlocks(const BlockMap& db) {
    db_.insert(std::cbegin(db), std::cend(db));
  }

  Status get(const Key&, Value&) const override {
    ADD_FAILURE() << "get(key) should not be called by this test";
    return Status::IllegalOperation("get() is not supported in test mode");
  }

  Status get(BlockId, const Sliver&, Sliver&, BlockId&) const override {
    ADD_FAILURE() << "get(key) should not be called by this test";
    return Status::IllegalOperation("get() is not supported in test mode");
  }
  BlockId getLastBlock() const override {
    EXPECT_FALSE(db_.cbegin() == db_.end());
    return db_.cbegin()->first + db_.size() - 1;
  }

  Status getBlockData(BlockId id, SetOfKeyValuePairs& kvp) const override {
    auto res = db_.find(id);
    if (res == db_.end()) {
      return Status::NotFound("Not found");
    }
    kvp = res->second;
    return Status::OK();
  }

  Status mayHaveConflictBetween(const Sliver&, BlockId, BlockId,
                                bool&) const override {
    ADD_FAILURE()
        << "mayHaveConflictBetween() should not be called by this test";
    return Status::IllegalOperation(
        "mayHaveConflictBetween() is not supported in test mode");
  }

 private:
  BlockMap db_;
};

class TestServerContext {
  std::multimap<std::string, std::string> metadata_ = {
      {"client_id", "TEST ID"}};

 public:
  const std::multimap<std::string, std::string>& client_metadata() const {
    return metadata_;
  }
  void erase_client_metadata() { metadata_.clear(); }
};

template <typename DataT>
class TestStateMachine {
 public:
  TestStateMachine(FakeStorage& storage, const BlockMap& live_update_blocks,
                   uint64_t start_block_id)
      : storage_(storage),
        live_update_blocks_(std::cbegin(live_update_blocks),
                            std::cend(live_update_blocks)),
        current_block_to_send_(start_block_id) {
    last_block_to_send_ = storage_.getLastBlock();
    if (live_update_blocks_.size()) {
      last_block_to_send_ += live_update_blocks_.size() - 1;
      // the gap blocks
      last_block_to_send_ +=
          live_update_blocks_.begin()->first - storage_.getLastBlock();
    }
    // the last pushed block
    last_block_to_send_++;
  }

  ~TestStateMachine() {
    EXPECT_EQ(last_block_to_send_, current_block_to_send_);
  }

  void set_expected_last_block_to_send(BlockId block_id) {
    last_block_to_send_ = block_id;
  }

  void on_live_update_buffer_added(std::shared_ptr<SubUpdateBuffer> buffer) {
    for (const auto& block : live_update_blocks_) {
      buffer->Push(block);
    }
    live_buffer_ = buffer;
  }

  void return_false_on_last_block(bool on) { return_false_on_last_block_ = on; }

  bool on_server_write(const DataT& data) {
    EXPECT_EQ(current_block_to_send_, data.block_id());
    if (current_block_to_send_ == last_block_to_send_) {
      return !return_false_on_last_block_;
    }

    if (live_buffer_) {
      if (current_block_to_send_ == last_block_to_send_ - 1) {
        on_finished_dropping_blocks();
      } else if (live_buffer_->Full() || live_buffer_->OldestBlockId() >
                                             (storage_.getLastBlock() + 1)) {
        // There is a gap that is supposed to be filled with blocks from the
        // storage
        on_sync_with_kvb_finished();
      }
    }
    ++current_block_to_send_;
    return true;
  }

  void on_sync_with_kvb_finished() {
    auto gap_blocks = generate_kvp(storage_.getLastBlock() + 1,
                                   live_buffer_->NewestBlockId());
    storage_.addBlocks(gap_blocks);
  }

  void on_finished_dropping_blocks() {
    live_buffer_->Push(generate_block(storage_.getLastBlock() + 1));
  }

 private:
  FakeStorage& storage_;
  BlockMap live_update_blocks_;
  std::shared_ptr<SubUpdateBuffer> live_buffer_;
  size_t current_block_to_send_{0u};
  size_t last_block_to_send_{0};
  bool return_false_on_last_block_{true};
};

template <typename T>
class TestServerWriter {
  TestStateMachine<T>& state_machine_;

 public:
  TestServerWriter(TestStateMachine<T>& state_machine)
      : state_machine_(state_machine) {}
  bool Write(T& msg) { return state_machine_.on_server_write(msg); }
};

template <typename DataT>
class TestSubBufferList : public concord::thin_replica::SubBufferList {
  TestStateMachine<DataT>& state_machine_;

 public:
  TestSubBufferList(TestStateMachine<DataT>& state_machine)
      : state_machine_(state_machine) {}

  // Add a subscriber
  void AddBuffer(std::shared_ptr<SubUpdateBuffer> elem) override {
    state_machine_.on_live_update_buffer_added(elem);
    concord::thin_replica::SubBufferList::AddBuffer(elem);
  }
};

TEST(thin_replica_test, SubscribeToUpdatesAlreadySynced) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(kLastBlockId + 1, kLastBlockId + 5);
  TestStateMachine<Data> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Data> buffer{state_machine};
  TestServerWriter<Data> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  SubscriptionRequest request;
  request.set_key_prefix("");
  request.set_block_id(1u);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Data>, Data>(
              &context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
}

TEST(thin_replica_test, SubscribeToUpdatesWithGap) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(kLastBlockId + 2, kLastBlockId + 5);
  TestStateMachine<Data> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Data> buffer{state_machine};
  TestServerWriter<Data> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  SubscriptionRequest request;
  request.set_key_prefix("");
  request.set_block_id(1u);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Data>, Data>(
              &context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
}

TEST(thin_replica_test, SubscribeToUpdatesWithGapFromTheMiddleBlock) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(kLastBlockId + 2, kLastBlockId + 5);
  TestStateMachine<Data> state_machine{storage, live_update_blocks, 3};
  TestSubBufferList<Data> buffer{state_machine};
  TestServerWriter<Data> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  SubscriptionRequest request;
  request.set_key_prefix("");
  request.set_block_id(3u);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Data>, Data>(
              &context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
}

TEST(thin_replica_test, SubscribeToUpdateHashesAlreadySynced) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(kLastBlockId + 1, kLastBlockId + 5);
  TestStateMachine<Hash> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Hash> buffer{state_machine};
  TestServerWriter<Hash> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  SubscriptionRequest request;
  request.set_key_prefix("");
  request.set_block_id(1u);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Hash>, Hash>(
              &context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
}

TEST(thin_replica_test, SubscribeToUpdateHashesWithGap) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(kLastBlockId + 2, kLastBlockId + 5);
  TestStateMachine<Hash> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Hash> buffer{state_machine};
  TestServerWriter<Hash> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  SubscriptionRequest request;
  request.set_key_prefix("");
  request.set_block_id(1u);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Hash>, Hash>(
              &context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
}

TEST(thin_replica_test, ReadState) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(0, 0);
  TestStateMachine<Data> state_machine{storage, live_update_blocks, 1};
  state_machine.set_expected_last_block_to_send(kLastBlockId);
  state_machine.return_false_on_last_block(false);
  TestSubBufferList<Data> buffer{state_machine};
  TestServerWriter<Data> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  ReadStateRequest request;
  request.set_key_prefix("");
  auto status = replica.ReadState(&context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
}

TEST(thin_replica_test, ReadStateHash) {
  FakeStorage storage{generate_kvp(1, kLastBlockId)};
  auto live_update_blocks = generate_kvp(0, 0);
  TestStateMachine<Hash> state_machine{storage, live_update_blocks, 0u};
  state_machine.set_expected_last_block_to_send(0u);
  TestSubBufferList<Hash> buffer{state_machine};
  TestServerWriter<Hash> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  ReadStateHashRequest request;
  request.set_key_prefix("");
  request.set_block_id(kLastBlockId);
  Hash hash;
  auto status = replica.ReadStateHash(&context, &request, &hash);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::OK);
  EXPECT_EQ(hash.block_id(), kLastBlockId);
}

TEST(thin_replica_test, AckUpdate) {
  FakeStorage storage{generate_kvp(0, 0)};
  auto live_update_blocks = generate_kvp(0, 0);
  TestStateMachine<Hash> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Hash> buffer{state_machine};
  TestServerWriter<Hash> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  ReadStateHashRequest request;
  request.set_key_prefix("");
  request.set_block_id(kLastBlockId);

  com::vmware::concord::thin_replica::BlockId block_id;
  block_id.set_block_id(1u);
  auto status = replica.AckUpdate(&context, &block_id, nullptr);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::UNIMPLEMENTED);
}

TEST(thin_replica_test, Unsubscribe) {
  FakeStorage storage{generate_kvp(0, 0)};
  auto live_update_blocks = generate_kvp(0, 0);
  TestStateMachine<Hash> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Hash> buffer{state_machine};
  TestServerWriter<Hash> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  ReadStateHashRequest request;
  request.set_key_prefix("");
  request.set_block_id(kLastBlockId);

  com::vmware::concord::thin_replica::BlockId block_id;
  block_id.set_block_id(1u);
  auto status = replica.Unsubscribe(&context, nullptr, nullptr);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::UNIMPLEMENTED);
}

TEST(thin_replica_test, ContextWithoutClientIdData) {
  FakeStorage storage{generate_kvp(0, 0)};
  auto live_update_blocks = generate_kvp(0, 0);
  TestStateMachine<Data> data_state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Data> data_buffer{data_state_machine};
  TestServerWriter<Data> data_stream{data_state_machine};
  TestStateMachine<Hash> hash_state_machine{storage, live_update_blocks, 1};
  TestServerWriter<Hash> hash_stream{hash_state_machine};
  TestServerContext context;
  context.erase_client_metadata();
  ReadStateHashRequest read_state_hash_request;
  ReadStateRequest read_state_request;
  SubscriptionRequest subscription_request;
  Hash hash;

  concord::thin_replica::ThinReplicaImpl replica(&storage, data_buffer);
  EXPECT_EQ(replica.ReadState(&context, &read_state_request, &data_stream)
                .error_code(),
            grpc::StatusCode::UNKNOWN);
  EXPECT_EQ(replica.ReadStateHash(&context, &read_state_hash_request, &hash)
                .error_code(),
            grpc::StatusCode::UNKNOWN);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Data>, Data>(
              &context, &subscription_request, &data_stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::UNKNOWN);
  status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Hash>, Hash>(
              &context, &subscription_request, &hash_stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::UNKNOWN);
}

TEST(thin_replica_test, SubscribeWithWrongBlockId) {
  FakeStorage storage{generate_kvp(0, 0)};
  auto live_update_blocks = generate_kvp(0, 0);
  TestStateMachine<Data> state_machine{storage, live_update_blocks, 1};
  TestSubBufferList<Data> buffer{state_machine};
  TestServerWriter<Data> stream{state_machine};

  concord::thin_replica::ThinReplicaImpl replica(&storage, buffer);
  TestServerContext context;
  SubscriptionRequest request;
  request.set_key_prefix("");
  request.set_block_id(kLastBlockId + 100);
  auto status =
      replica
          .SubscribeToUpdates<TestServerContext, TestServerWriter<Data>, Data>(
              &context, &request, &stream);
  EXPECT_EQ(status.error_code(), grpc::StatusCode::FAILED_PRECONDITION);
}
}  // namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  auto& hierarchy = log4cplus::Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  log4cplus::BasicConfigurator config{hierarchy, false};
  config.configure();
  auto logger = log4cplus::Logger::getInstance("thin_replica_test");
  return RUN_ALL_TESTS();
}
