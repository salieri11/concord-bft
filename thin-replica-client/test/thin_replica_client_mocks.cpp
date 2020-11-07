// Copyright 2020 VMware, all rights reserved

#include "thin_replica_client.hpp"
#include "trs_connection.hpp"

#include "gmock/gmock.h"
#include "thin_replica_client_mocks.hpp"
#include "trc_hash.hpp"

using com::vmware::concord::thin_replica::BlockId;
using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::MockThinReplicaStub;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
using google::protobuf::Empty;
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
using thin_replica_client::TrsConnection;

MockTrsConnection::MockTrsConnection()
    : TrsConnection("mock_address", "mock_client_id") {
  this->stub_.reset(new MockThinReplicaStub());
}
MockThinReplicaStub* MockTrsConnection::GetStub() {
  return dynamic_cast<MockThinReplicaStub*>(this->stub_.get());
}
bool MockTrsConnection::isConnected() { return true; }

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

void ThinReplicaCommunicationRecord::ClearRecords() {
  lock_guard<mutex> record_lock(record_mutex_);
  read_state_calls_.clear();
  read_state_hash_calls_.clear();
  subscribe_to_updates_calls_.clear();
  ack_update_calls_.clear();
  subscribe_to_update_hashes_calls_.clear();
  unsubscribe_calls_.clear();
}

void ThinReplicaCommunicationRecord::RecordReadState(
    size_t server_index, const ReadStateRequest& request) {
  lock_guard<mutex> record_lock(record_mutex_);
  read_state_calls_.emplace_back(server_index, request);
}

void ThinReplicaCommunicationRecord::RecordReadStateHash(
    size_t server_index, const ReadStateHashRequest& request) {
  lock_guard<mutex> record_lock(record_mutex_);
  read_state_hash_calls_.emplace_back(server_index, request);
}

void ThinReplicaCommunicationRecord::RecordSubscribeToUpdates(
    size_t server_index, const SubscriptionRequest& request) {
  lock_guard<mutex> record_lock(record_mutex_);
  subscribe_to_updates_calls_.emplace_back(server_index, request);
}

void ThinReplicaCommunicationRecord::RecordAckUpdate(size_t server_index,
                                                     const BlockId& block_id) {
  lock_guard<mutex> record_lock(record_mutex_);
  ack_update_calls_.emplace_back(server_index, block_id);
}

void ThinReplicaCommunicationRecord::RecordSubscribeToUpdateHashes(
    size_t server_index, const SubscriptionRequest& request) {
  lock_guard<mutex> record_lock(record_mutex_);
  subscribe_to_update_hashes_calls_.emplace_back(server_index, request);
}

void ThinReplicaCommunicationRecord::RecordUnsubscribe(size_t server_index) {
  lock_guard<mutex> record_lock(record_mutex_);
  unsubscribe_calls_.emplace_back(server_index);
}

const list<pair<size_t, ReadStateRequest>>&
ThinReplicaCommunicationRecord::GetReadStateCalls() const {
  return read_state_calls_;
}

const list<pair<size_t, ReadStateHashRequest>>&
ThinReplicaCommunicationRecord::GetReadStateHashCalls() const {
  return read_state_hash_calls_;
}

const list<pair<size_t, SubscriptionRequest>>&
ThinReplicaCommunicationRecord::GetSubscribeToUpdatesCalls() const {
  return subscribe_to_updates_calls_;
}

const list<pair<size_t, BlockId>>&
ThinReplicaCommunicationRecord::GetAckUpdateCalls() const {
  return ack_update_calls_;
}

const list<pair<size_t, SubscriptionRequest>>&
ThinReplicaCommunicationRecord::GetSubscribeToUpdateHashesCalls() const {
  return subscribe_to_update_hashes_calls_;
}

const list<size_t>& ThinReplicaCommunicationRecord::GetUnsubscribeCalls()
    const {
  return unsubscribe_calls_;
}

size_t ThinReplicaCommunicationRecord::GetTotalCallCount() const {
  return read_state_calls_.size() + read_state_hash_calls_.size() +
         subscribe_to_updates_calls_.size() + ack_update_calls_.size() +
         subscribe_to_update_hashes_calls_.size() + unsubscribe_calls_.size();
}

MockThinReplicaServerRecorder::MockThinReplicaServerRecorder(
    shared_ptr<MockDataStreamPreparer> data,
    shared_ptr<MockOrderedDataStreamHasher> hasher,
    shared_ptr<ThinReplicaCommunicationRecord> record, size_t server_index)
    : data_preparer_(data),
      hasher_(hasher),
      record_(record),
      server_index_(server_index) {}

ClientReaderInterface<Data>* MockThinReplicaServerRecorder::ReadStateRaw(
    ClientContext* context, const ReadStateRequest& request) {
  assert(data_preparer_);
  assert(record_);

  record_->RecordReadState(server_index_, request);
  return data_preparer_->ReadStateRaw(context, request);
}

Status MockThinReplicaServerRecorder::ReadStateHash(
    ClientContext* context, const ReadStateHashRequest& request,
    Hash* response) {
  assert(hasher_);
  assert(record_);

  record_->RecordReadStateHash(server_index_, request);
  return hasher_->ReadStateHash(context, request, response);
}

ClientReaderInterface<Data>*
MockThinReplicaServerRecorder::SubscribeToUpdatesRaw(
    ClientContext* context, const SubscriptionRequest& request) {
  assert(data_preparer_);
  assert(record_);

  record_->RecordSubscribeToUpdates(server_index_, request);
  return data_preparer_->SubscribeToUpdatesRaw(context, request);
}

Status MockThinReplicaServerRecorder::AckUpdate(ClientContext* context,
                                                const BlockId& block_id,
                                                Empty* response) {
  assert(record_);

  record_->RecordAckUpdate(server_index_, block_id);
  return Status::OK;
}

ClientReaderInterface<Hash>*
MockThinReplicaServerRecorder::SubscribeToUpdateHashesRaw(
    ClientContext* context, const SubscriptionRequest& request) {
  assert(hasher_);
  assert(record_);

  record_->RecordSubscribeToUpdateHashes(server_index_, request);
  return hasher_->SubscribeToUpdateHashesRaw(context, request);
}

Status MockThinReplicaServerRecorder::Unsubscribe(ClientContext* context,
                                                  const Empty& request,
                                                  Empty* response) {
  assert(record_);

  record_->RecordUnsubscribe(server_index_);
  return Status::OK;
}

vector<MockThinReplicaServerRecorder> CreateMockServerRecorders(
    size_t num_servers, shared_ptr<MockDataStreamPreparer> data,
    shared_ptr<MockOrderedDataStreamHasher> hasher,
    shared_ptr<ThinReplicaCommunicationRecord> record) {
  vector<MockThinReplicaServerRecorder> recorders;
  for (size_t i = 0; i < num_servers; ++i) {
    recorders.emplace_back(data, hasher, record, i);
  }
  return recorders;
}

void SetMockServerBehavior(
    MockTrsConnection* server,
    const shared_ptr<MockDataStreamPreparer>& data_preparer,
    const MockOrderedDataStreamHasher& hasher) {
  ON_CALL(*(server->GetStub()), ReadStateRaw)
      .WillByDefault(
          Invoke(data_preparer.get(), &MockDataStreamPreparer::ReadStateRaw));
  ON_CALL(*(server->GetStub()), ReadStateHash)
      .WillByDefault(
          Invoke(&hasher, &MockOrderedDataStreamHasher::ReadStateHash));
  ON_CALL(*(server->GetStub()), SubscribeToUpdatesRaw)
      .WillByDefault(Invoke(data_preparer.get(),
                            &MockDataStreamPreparer::SubscribeToUpdatesRaw));
  ON_CALL(*(server->GetStub()), SubscribeToUpdateHashesRaw)
      .WillByDefault(Invoke(
          &hasher, &MockOrderedDataStreamHasher::SubscribeToUpdateHashesRaw));
}

void SetMockServerBehavior(
    MockTrsConnection* server,
    MockThinReplicaServerRecorder& mock_server_recorder) {
  ON_CALL(*(server->GetStub()), ReadStateRaw)
      .WillByDefault(Invoke(&mock_server_recorder,
                            &MockThinReplicaServerRecorder::ReadStateRaw));
  ON_CALL(*(server->GetStub()), ReadStateHash)
      .WillByDefault(Invoke(&mock_server_recorder,
                            &MockThinReplicaServerRecorder::ReadStateHash));
  ON_CALL(*(server->GetStub()), SubscribeToUpdatesRaw)
      .WillByDefault(
          Invoke(&mock_server_recorder,
                 &MockThinReplicaServerRecorder::SubscribeToUpdatesRaw));
  ON_CALL(*(server->GetStub()), AckUpdate)
      .WillByDefault(Invoke(&mock_server_recorder,
                            &MockThinReplicaServerRecorder::AckUpdate));
  ON_CALL(*(server->GetStub()), SubscribeToUpdateHashesRaw)
      .WillByDefault(
          Invoke(&mock_server_recorder,
                 &MockThinReplicaServerRecorder::SubscribeToUpdateHashesRaw));
  ON_CALL(*(server->GetStub()), Unsubscribe)
      .WillByDefault(Invoke(&mock_server_recorder,
                            &MockThinReplicaServerRecorder::Unsubscribe));
}

void SetMockServerUnresponsive(MockTrsConnection* server) {
  ON_CALL(*(server->GetStub()), ReadStateRaw)
      .WillByDefault(InvokeWithoutArgs(&CreateUnresponsiveMockStream<Data>));
  ON_CALL(*(server->GetStub()), ReadStateHash)
      .WillByDefault(Return(
          Status(StatusCode::UNAVAILABLE, "This server is non-responsive")));
  ON_CALL(*(server->GetStub()), SubscribeToUpdatesRaw)
      .WillByDefault(InvokeWithoutArgs(&CreateUnresponsiveMockStream<Data>));
  ON_CALL(*(server->GetStub()), AckUpdate)
      .WillByDefault(Return(
          Status(StatusCode::UNAVAILABLE, "This server is non-responsive")));
  ON_CALL(*(server->GetStub()), SubscribeToUpdateHashesRaw)
      .WillByDefault(InvokeWithoutArgs(&CreateUnresponsiveMockStream<Hash>));
  ON_CALL(*(server->GetStub()), Unsubscribe)
      .WillByDefault(Return(
          Status(StatusCode::UNAVAILABLE, "This server is non-responsive")));
}

vector<unique_ptr<TrsConnection>> CreateTrsConnections(
    size_t num_servers, size_t num_unresponsive) {
  vector<unique_ptr<TrsConnection>> mock_servers;
  for (size_t i = 0; i < num_servers; ++i) {
    auto conn = new MockTrsConnection();
    if (num_unresponsive > 0) {
      SetMockServerUnresponsive(conn);
      num_unresponsive--;
    }
    auto server = dynamic_cast<TrsConnection*>(conn);
    mock_servers.push_back(unique_ptr<TrsConnection>(server));
  }
  return mock_servers;
}

vector<unique_ptr<TrsConnection>> CreateTrsConnections(
    size_t num_servers, shared_ptr<MockDataStreamPreparer> stream_preparer,
    MockOrderedDataStreamHasher& hasher, size_t num_unresponsive) {
  vector<unique_ptr<TrsConnection>> mock_servers;
  for (size_t i = 0; i < num_servers; ++i) {
    auto conn = new MockTrsConnection();
    SetMockServerBehavior(conn, stream_preparer, hasher);
    if (num_unresponsive > 0) {
      SetMockServerUnresponsive(conn);
      num_unresponsive--;
    }
    auto server = dynamic_cast<TrsConnection*>(conn);
    mock_servers.push_back(unique_ptr<TrsConnection>(server));
  }
  return mock_servers;
}

vector<unique_ptr<TrsConnection>> CreateTrsConnections(
    vector<MockThinReplicaServerRecorder>& mock_server_recorders) {
  vector<unique_ptr<TrsConnection>> mock_servers;
  for (size_t i = 0; i < mock_server_recorders.size(); ++i) {
    auto conn = new MockTrsConnection();
    SetMockServerBehavior(conn, mock_server_recorders[i]);
    auto server = dynamic_cast<TrsConnection*>(conn);
    mock_servers.push_back(unique_ptr<TrsConnection>(server));
  }
  return mock_servers;
}
