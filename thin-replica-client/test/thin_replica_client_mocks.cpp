// Copyright 2020 VMware, all rights reserved

#include "thin_replica_client.hpp"

#include "gmock/gmock.h"
#include "thin_replica_client_mocks.hpp"
#include "trc_hash.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::MockThinReplicaStub;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
using grpc::ClientContext;
using grpc::ClientReaderInterface;
using grpc::Status;
using grpc::StatusCode;
using std::condition_variable;
using std::invalid_argument;
using std::list;
using std::lock_guard;
using std::make_unique;
using std::mutex;
using std::pair;
using std::shared_ptr;
using std::string;
using std::unique_lock;
using std::unique_ptr;
using std::vector;
using testing::Invoke;
using testing::InvokeWithoutArgs;
using testing::Return;
using thin_replica_client::HashUpdate;

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

VectorMockDataStreamPreparer::DataQueue::DataQueue(const list<Data>& data)
    : queue_(data) {}
VectorMockDataStreamPreparer::DataQueue::~DataQueue() {}
Status VectorMockDataStreamPreparer::DataQueue::Finish() {
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

bool VectorMockDataStreamPreparer::DataQueue::Read(Data* msg) {
  assert(msg);

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

ClientReaderInterface<Data>*
VectorMockDataStreamPreparer::PrepareInitialStateDataStream(
    const string& filter) const {
  auto data_stream = new MockThinReplicaStream<Data>();
  list<Data> data_queue;
  assert(num_updates_in_initial_state_ <= data_.size());
  for (size_t i = 0; i < num_updates_in_initial_state_; ++i) {
    data_queue.push_back(FilterUpdate(data_[i], filter));
  }
  auto data_stream_state = new DataQueue(data_queue);
  data_stream->state.reset(data_stream_state);

  ON_CALL(*data_stream, Finish)
      .WillByDefault(Invoke(data_stream_state, &DataQueue::Finish));
  ON_CALL(*data_stream, Read)
      .WillByDefault(Invoke(data_stream_state, &DataQueue::Read));
  return data_stream;
}

ClientReaderInterface<Data>*
VectorMockDataStreamPreparer::PrepareSubscriptionDataStream(
    uint64_t block_id, const string& filter) const {
  auto data_stream = new MockThinReplicaStream<Data>();
  list<Data> data_queue;
  size_t subscription_start = 0;
  while ((subscription_start < data_.size()) &&
         (data_[subscription_start].block_id() < block_id)) {
    ++subscription_start;
  }
  for (size_t i = subscription_start; i < data_.size(); ++i) {
    data_queue.push_back(FilterUpdate(data_[i], filter));
  }
  auto data_stream_state = new DataQueue(data_queue);
  data_stream->state.reset(data_stream_state);

  ON_CALL(*data_stream, Finish)
      .WillByDefault(Invoke(data_stream_state, &DataQueue::Finish));
  ON_CALL(*data_stream, Read)
      .WillByDefault(Invoke(data_stream_state, &DataQueue::Read));
  return data_stream;
}

VectorMockDataStreamPreparer::VectorMockDataStreamPreparer(
    const vector<Data>& data, size_t initial_state_size)
    : data_(data), num_updates_in_initial_state_(initial_state_size) {
  if (initial_state_size > data.size()) {
    throw invalid_argument(
        "Attempting to construct VectorMockDataStreamPreparer with initial "
        "state longer than provided data.");
  }
}

VectorMockDataStreamPreparer::~VectorMockDataStreamPreparer() {}
ClientReaderInterface<Data>* VectorMockDataStreamPreparer::ReadStateRaw(
    ClientContext* context, const ReadStateRequest& request) const {
  return PrepareInitialStateDataStream(request.key_prefix());
}

ClientReaderInterface<Data>*
VectorMockDataStreamPreparer::SubscribeToUpdatesRaw(
    ClientContext* context, const SubscriptionRequest& request) const {
  return PrepareSubscriptionDataStream(request.block_id(),
                                       request.key_prefix());
}

RepeatedMockDataStreamPreparer::DataRepeater::DataRepeater(
    const Data& data, uint64_t starting_block_id)
    : data_(data),
      current_block_id_(starting_block_id),
      finite_length_(false) {}

RepeatedMockDataStreamPreparer::DataRepeater::DataRepeater(
    const Data& data, uint64_t starting_block_id, size_t num_updates)
    : data_(data),
      current_block_id_(starting_block_id),
      finite_length_(true),
      num_updates_(num_updates) {}

RepeatedMockDataStreamPreparer::DataRepeater::~DataRepeater() {}
Status RepeatedMockDataStreamPreparer::DataRepeater::Finish() {
  // Note this Finish implementation does not account for the case where the
  // Thin Replica Client ends the stream early with TryCancel; neglecting
  // this case should still be technically correct behavior that the Thin
  // Replica Client needs to handle as TryCancel is not guaranteed to
  // succeed.
  // Note this loop will never finish if finite_length_ is false.
  while (!(finite_length_ && (num_updates_ < 1))) {
    unique_lock<mutex> finished_condition_lock(finished_condition_mutex_);
    finished_condition_.wait(finished_condition_lock);
  }
  return Status::OK;
}

bool RepeatedMockDataStreamPreparer::DataRepeater::Read(Data* msg) {
  assert(msg);

  lock_guard<mutex> block_id_lock(block_id_mutex_);
  if (finite_length_ && (num_updates_ < 1)) {
    return false;
  } else {
    data_.set_block_id(current_block_id_++);
    if (finite_length_ && (--num_updates_ < 1)) {
      finished_condition_.notify_all();
    }
    *msg = data_;
    return true;
  }
}

ClientReaderInterface<Data>*
RepeatedMockDataStreamPreparer::PrepareInitialStateDataStream(
    const string& filter) const {
  auto data_stream = new MockThinReplicaStream<Data>();
  auto data_stream_state =
      new DataRepeater(FilterUpdate(data_, filter), data_.block_id(),
                       num_updates_in_initial_state_);
  data_stream->state.reset(data_stream_state);

  ON_CALL(*data_stream, Finish)
      .WillByDefault(Invoke(data_stream_state, &DataRepeater::Finish));
  ON_CALL(*data_stream, Read)
      .WillByDefault(Invoke(data_stream_state, &DataRepeater::Read));
  return data_stream;
}

ClientReaderInterface<Data>*
RepeatedMockDataStreamPreparer::PrepareSubscriptionDataStream(
    uint64_t block_id, const string& filter) const {
  auto data_stream = new MockThinReplicaStream<Data>();
  auto data_stream_state =
      new DataRepeater(FilterUpdate(data_, filter), block_id);
  data_stream->state.reset(data_stream_state);

  ON_CALL(*data_stream, Finish)
      .WillByDefault(Invoke(data_stream_state, &DataRepeater::Finish));
  ON_CALL(*data_stream, Read)
      .WillByDefault(Invoke(data_stream_state, &DataRepeater::Read));
  return data_stream;
}

RepeatedMockDataStreamPreparer::RepeatedMockDataStreamPreparer(
    const Data& data, size_t initial_state_length)
    : data_(data), num_updates_in_initial_state_(initial_state_length) {}

RepeatedMockDataStreamPreparer::~RepeatedMockDataStreamPreparer() {}
ClientReaderInterface<Data>* RepeatedMockDataStreamPreparer::ReadStateRaw(
    ClientContext* context, const ReadStateRequest& request) const {
  return PrepareInitialStateDataStream(request.key_prefix());
}

ClientReaderInterface<Data>*
RepeatedMockDataStreamPreparer::SubscribeToUpdatesRaw(
    ClientContext* context, const SubscriptionRequest& request) const {
  return PrepareSubscriptionDataStream(request.block_id(),
                                       request.key_prefix());
}

DelayedMockDataStreamPreparer::DataDelayer::DataDelayer(
    ClientReaderInterface<Data>* data,
    const shared_ptr<condition_variable>& delay_condition,
    const shared_ptr<mutex>& delay_mutex,
    const shared_ptr<bool>& spurious_wakeup)
    : undelayed_data_(data),
      waiting_condition_(delay_condition),
      spurious_wakeup_(spurious_wakeup),
      condition_mutex_(delay_mutex) {}

DelayedMockDataStreamPreparer::DataDelayer::~DataDelayer() {}

Status DelayedMockDataStreamPreparer::DataDelayer::Finish() {
  Status status = undelayed_data_->Finish();
  while (*spurious_wakeup_) {
    unique_lock<mutex> condition_lock(*condition_mutex_);
    waiting_condition_->wait(condition_lock);
  }
  return status;
}

bool DelayedMockDataStreamPreparer::DataDelayer::Read(Data* msg) {
  assert(msg);

  while (*spurious_wakeup_) {
    unique_lock<mutex> condition_lock(*condition_mutex_);
    waiting_condition_->wait(condition_lock);
  }
  return undelayed_data_->Read(msg);
}

DelayedMockDataStreamPreparer::DelayedMockDataStreamPreparer(
    shared_ptr<MockDataStreamPreparer>& data,
    shared_ptr<condition_variable> condition,
    shared_ptr<bool> spurious_wakeup_indicator,
    shared_ptr<mutex> condition_mutex)
    : undelayed_data_preparer_(data),
      delay_condition_(condition),
      spurious_wakeup_(spurious_wakeup_indicator),
      condition_mutex_(condition_mutex) {}

DelayedMockDataStreamPreparer::~DelayedMockDataStreamPreparer() {}

ClientReaderInterface<Data>* DelayedMockDataStreamPreparer::ReadStateRaw(
    ClientContext* context, const ReadStateRequest& request) const {
  return undelayed_data_preparer_->ReadStateRaw(context, request);
}

ClientReaderInterface<Data>*
DelayedMockDataStreamPreparer::SubscribeToUpdatesRaw(
    ClientContext* context, const SubscriptionRequest& request) const {
  auto data_stream = new MockThinReplicaStream<Data>();
  auto data_stream_state = new DataDelayer(
      undelayed_data_preparer_->SubscribeToUpdatesRaw(context, request),
      delay_condition_, condition_mutex_, spurious_wakeup_);
  data_stream->state.reset(data_stream_state);

  ON_CALL(*data_stream, Finish)
      .WillByDefault(Invoke(data_stream_state, &DataDelayer::Finish));
  ON_CALL(*data_stream, Read)
      .WillByDefault(Invoke(data_stream_state, &DataDelayer::Read));
  return data_stream;
}

MockOrderedDataStreamHasher::StreamHasher::StreamHasher(
    ClientReaderInterface<Data>* data)
    : data_stream_(data) {}

MockOrderedDataStreamHasher::StreamHasher::~StreamHasher() {}

Status MockOrderedDataStreamHasher::StreamHasher::Finish() {
  return data_stream_->Finish();
}

bool MockOrderedDataStreamHasher::StreamHasher::Read(Hash* msg) {
  assert(msg);

  Data data;
  bool read_status = data_stream_->Read(&data);
  if (read_status) {
    msg->set_block_id(data.block_id());
    UpdateHashType hash_val = HashUpdate(data);
    msg->set_hash(&hash_val, sizeof(hash_val));
  }
  return read_status;
}

MockOrderedDataStreamHasher::MockOrderedDataStreamHasher(
    shared_ptr<MockDataStreamPreparer>& data)
    : data_preparer_(data) {
  // MockOrderedDataStreamHasher determines the starting block ID from the
  // stream prepared for ReadState by the MockDataStreamPreparer.
  auto context = make_unique<ClientContext>();
  ReadStateRequest request;
  request.set_key_prefix("");
  unique_ptr<ClientReaderInterface<Data>> data_stream(
      data->ReadStateRaw(context.get(), request));
  Data base_block;
  if (!data_stream->Read(&base_block)) {
    throw invalid_argument(
        "Attempting to construct a MockOrderedDataStreamHasher with a "
        "MockDataStreamPreparer that does not successfully provide any "
        "initial state.");
  }
  base_block_id_ = base_block.block_id();
}

Status MockOrderedDataStreamHasher::ReadStateHash(
    ClientContext* context, const ReadStateHashRequest& request,
    Hash* response) const {
  assert(response);

  StateHashType hash_val = 0;
  auto data_context = make_unique<ClientContext>();
  SubscriptionRequest data_request;
  data_request.set_key_prefix(request.key_prefix());
  data_request.set_block_id(base_block_id_);
  unique_ptr<ClientReaderInterface<Data>> data_stream(
      data_preparer_->SubscribeToUpdatesRaw(data_context.get(), data_request));

  Data data;
  uint64_t latest_block_id_in_hash = 0;
  while (data_stream->Read(&data) && data.block_id() <= request.block_id()) {
    hash_val ^= HashUpdate(data);
    latest_block_id_in_hash = data.block_id();
  }
  if (request.block_id() > latest_block_id_in_hash) {
    return Status(StatusCode::UNKNOWN,
                  "Attempting to read state hash from a block number that "
                  "does not exist.");
  }
  response->set_block_id(request.block_id());
  response->set_hash(&hash_val, sizeof(hash_val));
  return Status::OK;
}

ClientReaderInterface<Hash>*
MockOrderedDataStreamHasher::SubscribeToUpdateHashesRaw(
    ClientContext* context, const SubscriptionRequest& request) const {
  auto hash_stream = new MockThinReplicaStream<Hash>();
  auto hash_stream_state =
      new StreamHasher(data_preparer_->SubscribeToUpdatesRaw(context, request));
  hash_stream->state.reset(hash_stream_state);

  ON_CALL(*hash_stream, Finish)
      .WillByDefault(Invoke(hash_stream_state, &StreamHasher::Finish));
  ON_CALL(*hash_stream, Read)
      .WillByDefault(Invoke(hash_stream_state, &StreamHasher::Read));
  return hash_stream;
}

void SetMockServerBehavior(
    unique_ptr<MockThinReplicaStub>& server,
    const shared_ptr<MockDataStreamPreparer>& data_preparer,
    const MockOrderedDataStreamHasher& hasher) {
  ON_CALL(*server, ReadStateRaw)
      .WillByDefault(
          Invoke(data_preparer.get(), &MockDataStreamPreparer::ReadStateRaw));
  ON_CALL(*server, ReadStateHash)
      .WillByDefault(
          Invoke(&hasher, &MockOrderedDataStreamHasher::ReadStateHash));
  ON_CALL(*server, SubscribeToUpdatesRaw)
      .WillByDefault(Invoke(data_preparer.get(),
                            &MockDataStreamPreparer::SubscribeToUpdatesRaw));
  ON_CALL(*server, SubscribeToUpdateHashesRaw)
      .WillByDefault(Invoke(
          &hasher, &MockOrderedDataStreamHasher::SubscribeToUpdateHashesRaw));
}

void SetMockServerBehavior(
    vector<pair<string, unique_ptr<MockThinReplicaStub>>>& servers,
    const shared_ptr<MockDataStreamPreparer>& data_preparer,
    const MockOrderedDataStreamHasher& hasher) {
  for (auto& server : servers) {
    SetMockServerBehavior(server.second, data_preparer, hasher);
  }
}

void SetMockServerUnresponsive(unique_ptr<MockThinReplicaStub>& server) {
  ON_CALL(*server, ReadStateRaw)
      .WillByDefault(InvokeWithoutArgs(&CreateUnresponsiveMockStream<Data>));
  ON_CALL(*server, ReadStateHash)
      .WillByDefault(Return(
          Status(StatusCode::UNAVAILABLE, "This server is non-responsive")));
  ON_CALL(*server, SubscribeToUpdatesRaw)
      .WillByDefault(InvokeWithoutArgs(&CreateUnresponsiveMockStream<Data>));
  ON_CALL(*server, AckUpdate)
      .WillByDefault(Return(
          Status(StatusCode::UNAVAILABLE, "This server is non-responsive")));
  ON_CALL(*server, SubscribeToUpdateHashesRaw)
      .WillByDefault(InvokeWithoutArgs(&CreateUnresponsiveMockStream<Hash>));
  ON_CALL(*server, Unsubscribe)
      .WillByDefault(Return(
          Status(StatusCode::UNAVAILABLE, "This server is non-responsive")));
}

void SetMockServerUnresponsive(
    vector<pair<string, unique_ptr<MockThinReplicaStub>>>& servers) {
  for (auto& server : servers) {
    SetMockServerUnresponsive(server.second);
  }
}

void SetSomeMockServersUnresponsive(
    vector<pair<string, unique_ptr<MockThinReplicaStub>>>& servers,
    size_t num_unresponsive) {
  assert(num_unresponsive <= servers.size());
  for (size_t i = 0; i < num_unresponsive; ++i) {
    SetMockServerUnresponsive(servers[i].second);
  }
}

void InstantiateMockServers(
    vector<pair<string, unique_ptr<MockThinReplicaStub>>>& mock_servers,
    size_t num_servers) {
  mock_servers.clear();
  for (size_t i = 0; i < num_servers; ++i) {
    mock_servers.push_back(make_pair("", make_unique<MockThinReplicaStub>()));
  }
}
