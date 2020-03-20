// Copyright 2020 VMware, all rights reserved

#include <log4cplus/configurator.h>
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "thin_replica_client_facade.hpp"
#include "thin_replica_client_facade_impl.hpp"
#include "thin_replica_client_mocks.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::ThinReplica;
using grpc::ClientContext;
using grpc::ClientReaderInterface;
using grpc::Status;
using grpc::StatusCode;
using std::condition_variable;
using std::hash;
using std::list;
using std::lock_guard;
using std::make_unique;
using std::min;
using std::mutex;
using std::out_of_range;
using std::pair;
using std::string;
using std::to_string;
using std::unique_lock;
using std::unique_ptr;
using std::vector;
using testing::Invoke;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::ThinReplicaClientFacade;
using thin_replica_client::Update;

// Helper functions and classes to the unit tests.

// Implementation of ThinReplicaClientFacade's templated friend function to
// access its private constructor in order to construct a facade with mock
// server objects.
template <typename ThinReplicaServer>
unique_ptr<ThinReplicaClientFacade>
thin_replica_client::ConstructThinReplicaClientFacade(
    const string& client_id, uint16_t max_faulty, const string& private_key,
    vector<pair<string, ThinReplicaServer>>& mock_servers,
    const std::string& jaeger_agent) {
  unique_ptr<ThinReplicaClientFacade::Impl> impl(
      new ThinReplicaClientFacade::Impl);
  impl->trc.reset(new ThinReplicaClient(
      client_id, impl->update_queue, max_faulty, private_key,
      mock_servers.begin(), mock_servers.end(), jaeger_agent));
  return unique_ptr<ThinReplicaClientFacade>(
      new ThinReplicaClientFacade(move(impl)));
}
using thin_replica_client::ConstructThinReplicaClientFacade;

Data FilterUpdate(const Data& raw_update, const string& filter) {
  Data filtered_update;
  filtered_update.set_block_id(raw_update.block_id());
  filtered_update.set_correlation_id(raw_update.correlation_id());
  for (const KVPair& raw_kvp : raw_update.data()) {
    const string key = raw_kvp.key();
    if ((key.length() >= filter.length()) &&
        (key.compare(0, filter.size(), filter) == 0)) {
      KVPair* filtered_kvp = filtered_update.add_data();
      *filtered_kvp = raw_kvp;
    }
  }
  return filtered_update;
}

class MockDataStreamPreparer {
 private:
  class DataQueue : public MockThinReplicaStream<Data>::State {
   public:
    virtual ~DataQueue() override {}
    Status Finish() {
      // Note this Finish implementation does not account for the case where the
      // Thin Replica Client ends the stream early with TryCancel; neglecting
      // this case should still be technically correct behavior that the Thin
      // Replica Client needs to handle as TryCancel is not guaranteed to
      // succeed.
      while (!queue_.empty()) {
        unique_lock<mutex> empty_condition_lock(empty_condition_mutex_);
        empty_condition_.wait(empty_condition_lock);
      }
      return Status::OK;
    }
    bool Read(Data* msg) {
      if (queue_.empty()) {
        return false;
      } else {
        {
          lock_guard<mutex> queue_lock(queue_mutex_);
          *msg = queue_.front();
          queue_.pop_front();
        }
        if (queue_.empty()) {
          empty_condition_.notify_all();
        }
        return true;
      }
    }
    list<Data> queue_;
    mutex queue_mutex_;
    condition_variable empty_condition_;
    mutex empty_condition_mutex_;
  };

  const vector<Data>& data_;

 public:
  // A MockDataStreamPreparer must not outlive the data vector it is constructed
  // with a reference to. As a precondition, MockDataStreamPreparer assumes the
  // entries in data are in monotonically increasing order of block number.
  MockDataStreamPreparer(const vector<Data>& data) : data_(data) {}

  ClientReaderInterface<Data>* PrepareFilteredDataStream(
      const string& filter) const {
    MockThinReplicaStream<Data>* data_stream =
        new MockThinReplicaStream<Data>();
    DataQueue* data_stream_state = new DataQueue();
    data_stream->state.reset(data_stream_state);
    list<Data>& data_queue = data_stream_state->queue_;
    for (const Data& raw_data : data_) {
      data_queue.push_back(FilterUpdate(raw_data, filter));
    }
    ON_CALL(*data_stream, Finish)
        .WillByDefault(Invoke(data_stream_state, &DataQueue::Finish));
    ON_CALL(*data_stream, Read)
        .WillByDefault(Invoke(data_stream_state, &DataQueue::Read));
    return data_stream;
  }
  ClientReaderInterface<Data>* ReadStateRaw(
      ClientContext* context, const ReadStateRequest& request) const {
    return PrepareFilteredDataStream(request.key_prefix());
  }
};

class MockHasher {
 private:
  const vector<Data>& data_;

  typedef size_t StateHashType;
  StateHashType AppendToStateHash(StateHashType cummulative_hash,
                                  const Data& update) const {
    StateHashType update_hash = hash<string>{}(to_string(update.block_id()));
    for (const KVPair& kvp : update.data()) {
      StateHashType kvp_hash = hash<string>{}(kvp.key());
      kvp_hash ^= hash<string>{}(kvp.value());
      update_hash ^= kvp_hash;
    }
    return update_hash;
  }
  Hash ComputeStateHash(uint64_t block_id, const string& key_prefix) const {
    if (block_id > data_.back().block_id()) {
      throw out_of_range(
          "Cannot compute state hash for given Block ID (" +
          to_string(block_id) +
          "); Block ID is outside of the range of available blocks.");
    }
    StateHashType hash_val = 0;
    for (size_t i = 0; i < data_.size() && data_[i].block_id() <= block_id;
         ++i) {
      hash_val =
          AppendToStateHash(hash_val, FilterUpdate(data_[i], key_prefix));
    }
    Hash hash;
    hash.set_block_id(block_id);
    hash.set_hash(&hash_val, sizeof(hash_val));
    return hash;
  }

 public:
  // A MockHasher must not outlive the data vector it is constructed with a
  // reference to. As a precondition, MockHasher assumes the entries in data are
  // in monotonically increasing order of block number.
  MockHasher(const vector<Data>& data) : data_(data) {}

  Status ReadStateHash(ClientContext* context,
                       const ReadStateHashRequest& request,
                       Hash* response) const {
    try {
      *response = ComputeStateHash(request.block_id(), request.key_prefix());
      return Status::OK;
    } catch (const out_of_range& e) {
      return Status(StatusCode::UNKNOWN,
                    "Attempting to read state hash from a block number that "
                    "does not exist.");
    }
  }
};

void SetMockServerInitialState(unique_ptr<MockThinReplicaServer>& server,
                               const MockDataStreamPreparer& data_preparer,
                               const MockHasher& hasher) {
  ON_CALL(*server, ReadStateRaw)
      .WillByDefault(
          Invoke(&data_preparer, &MockDataStreamPreparer::ReadStateRaw));
  ON_CALL(*server, ReadStateHash)
      .WillByDefault(Invoke(&hasher, &MockHasher::ReadStateHash));
}

void SetMockServerInitialState(
    vector<pair<string, unique_ptr<MockThinReplicaServer>>>& servers,
    const MockDataStreamPreparer& data_preparer, const MockHasher& hasher) {
  for (auto& server : servers) {
    SetMockServerInitialState(server.second, data_preparer, hasher);
  }
}

namespace {

TEST(thin_replica_client_test, test_receive_one_initial_update) {
  // This test case is added for now as something of a simple example or proof
  // of concept for the Thin Replica Client Library unit testing setup. The
  // developer of the Thin Replica Client Library unit tests reserves the right
  // to substantially expand upon, completely refactor or overhaul, or outright
  // remove this particular test case should it later be found this particular
  // test case doesn't add much to the suite overall as we start adding more
  // detailed tests to the suite.

  vector<Data> initial_updates;
  Data expected_update;
  expected_update.set_block_id(0);
  KVPair* expected_update_data = expected_update.add_data();
  expected_update_data->set_key("key");
  expected_update_data->set_value("value");
  initial_updates.push_back(expected_update);

  MockDataStreamPreparer stream_preparer(initial_updates);
  MockHasher hasher(initial_updates);

  vector<pair<string, unique_ptr<MockThinReplicaServer>>> mock_servers;
  for (size_t i = 0; i < 4; ++i) {
    mock_servers.push_back(make_pair("", make_unique<MockThinReplicaServer>()));
  }
  SetMockServerInitialState(mock_servers, stream_preparer, hasher);

  unique_ptr<ThinReplicaClientFacade> trc = ConstructThinReplicaClientFacade(
      "0", 1, "", mock_servers, "127.0.0.1:6831");
  trc->Subscribe("");

  unique_ptr<Update> update = trc->TryPop();
  ASSERT_TRUE((bool)update)
      << "Thin Replica Client did not return an update after Subscribe was "
         "called when initial state was available.";
  EXPECT_EQ(update->block_id, expected_update.block_id())
      << "Thin Replica Client returned an update from initial state with an "
         "incorrect block ID.";
  EXPECT_EQ(update->kv_pairs.size(), expected_update.data_size())
      << "Thin Replica Client returned an update changing the wrong number of "
         "key-value pairs.";
  size_t min_size =
      min(update->kv_pairs.size(), (size_t)(expected_update.data_size()));
  for (size_t i = 0; i < min_size; ++i) {
    EXPECT_EQ(update->kv_pairs[i].first, expected_update.data(i).key())
        << "thin Replica Client returned an update with an incorrect key.";
    EXPECT_EQ(update->kv_pairs[i].second, expected_update.data(i).value())
        << "thin Replica Client returned an update with an incorrect value.";
  }
}
}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  log4cplus::BasicConfigurator config;
  config.configure();
  return RUN_ALL_TESTS();
}
