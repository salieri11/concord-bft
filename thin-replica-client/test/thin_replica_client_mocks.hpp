// Copyright 2020 VMware, all rights reserved

#ifndef THIN_REPLICA_CLIENT_MOCKS_HPP
#define THIN_REPLICA_CLIENT_MOCKS_HPP

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "thin_replica_mock.grpc.pb.h"
#include "trs_connection.hpp"

class MockTrsConnection : public thin_replica_client::TrsConnection {
 public:
  MockTrsConnection();
  ~MockTrsConnection();
  com::vmware::concord::thin_replica::MockThinReplicaStub* GetStub();
  bool isConnected() override;
};

template <class DataType>
class MockThinReplicaStream : public grpc::ClientReaderInterface<DataType> {
 public:
  // Functions the Thin Replica Client Library is known to currently use.
  MOCK_METHOD0_T(Finish, grpc::Status());
  MOCK_METHOD1_T(Read, bool(DataType* msg));

  // Other virtual StubInterface functions currently thought to be unused by the
  // Thin Replica Client Library.
  MOCK_METHOD1_T(NextMessageSize, bool(uint32_t* sz));
  MOCK_METHOD0_T(WaitForInitialMetadata, void());

  // Abstract state type and unique_ptr to state of that type that testing code
  // may attach to this MockThinReplicaStream. This can be used to tie the
  // lifespan and destruction of auxiliary state the testing code is using with
  // a MockThinReplicaStream object to the lifespan and destruction of that mock
  // object.
  class State {
   public:
    virtual ~State() {}
  };
  std::unique_ptr<State> state = std::unique_ptr<State>(nullptr);
};

// Abstract class specifying an interface for handlig mocked calls to ReadState
// and SubscribeToUpdates.
class MockDataStreamPreparer {
 public:
  virtual ~MockDataStreamPreparer() {}
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  ReadStateRaw(grpc::ClientContext* context,
               const com::vmware::concord::thin_replica::ReadStateRequest&
                   request) const = 0;
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  SubscribeToUpdatesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request)
      const = 0;
};

// Prepares mock data streams to return the contents of the vector it is
// constructed with in order; the block IDs given in the vector for the Data
// entries are preserved.
class VectorMockDataStreamPreparer : public MockDataStreamPreparer {
 private:
  class DataQueue : public MockThinReplicaStream<
                        com::vmware::concord::thin_replica::Data>::State {
   private:
    std::list<com::vmware::concord::thin_replica::Data> queue_;
    std::mutex queue_mutex_;
    std::condition_variable empty_condition_;
    std::mutex empty_condition_mutex_;

   public:
    DataQueue(const std::list<com::vmware::concord::thin_replica::Data>& data);
    virtual ~DataQueue() override;
    grpc::Status Finish();
    bool Read(com::vmware::concord::thin_replica::Data* msg);
  };
  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  PrepareInitialStateDataStream(const std::string& filter) const;
  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  PrepareSubscriptionDataStream(uint64_t block_id,
                                const std::string& filter) const;

  const std::vector<com::vmware::concord::thin_replica::Data> data_;
  size_t num_updates_in_initial_state_;

 public:
  VectorMockDataStreamPreparer(
      const std::vector<com::vmware::concord::thin_replica::Data>& data,
      size_t initial_state_size = 1);
  virtual ~VectorMockDataStreamPreparer() override;
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  ReadStateRaw(grpc::ClientContext* context,
               const com::vmware::concord::thin_replica::ReadStateRequest&
                   request) const override;
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  SubscribeToUpdatesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request)
      const override;
};

// Prepares mock data streams that repeatedly return the same update.
// Automatically increments the Block IDs of sequentially returned Data objects,
// beginning from the block ID the Data object this
// RepeatedMJockDataStreamPreparer was constructed with (for requests that do
// not specify a block ID), or beginning with the specified block ID (for
// requests that do specify a block ID).
class RepeatedMockDataStreamPreparer : public MockDataStreamPreparer {
 private:
  class DataRepeater : public MockThinReplicaStream<
                           com::vmware::concord::thin_replica::Data>::State {
   private:
    com::vmware::concord::thin_replica::Data data_;
    uint64_t current_block_id_;
    std::mutex block_id_mutex_;

    bool finite_length_;
    size_t num_updates_ = 0;

    std::condition_variable finished_condition_;
    std::mutex finished_condition_mutex_;

   public:
    DataRepeater(const com::vmware::concord::thin_replica::Data& data,
                 uint64_t starting_block_id);
    DataRepeater(const com::vmware::concord::thin_replica::Data& data,
                 uint64_t starting_block_id, size_t num_updates);
    virtual ~DataRepeater() override;
    grpc::Status Finish();
    bool Read(com::vmware::concord::thin_replica::Data* msg);
  };

  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  PrepareInitialStateDataStream(const std::string& filter) const;

  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  PrepareSubscriptionDataStream(uint64_t block_id,
                                const std::string& filter) const;

  com::vmware::concord::thin_replica::Data data_;
  size_t num_updates_in_initial_state_;

 public:
  RepeatedMockDataStreamPreparer(
      const com::vmware::concord::thin_replica::Data& data,
      size_t initial_state_length = 1);
  virtual ~RepeatedMockDataStreamPreparer() override;
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  ReadStateRaw(grpc::ClientContext* context,
               const com::vmware::concord::thin_replica::ReadStateRequest&
                   request) const override;
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  SubscribeToUpdatesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request)
      const override;
};

// Decorator of other MockDataStreamPreparers that waits on an external
// condition before sending each update (excluding those that are part of the
// initial state).
class DelayedMockDataStreamPreparer : public MockDataStreamPreparer {
 private:
  class DataDelayer : public MockThinReplicaStream<
                          com::vmware::concord::thin_replica::Data>::State {
   private:
    std::unique_ptr<
        grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>>
        undelayed_data_;

    std::shared_ptr<std::condition_variable> waiting_condition_;
    std::shared_ptr<bool> spurious_wakeup_;
    std::shared_ptr<std::mutex> condition_mutex_;

   public:
    DataDelayer(grpc::ClientReaderInterface<
                    com::vmware::concord::thin_replica::Data>* data,
                const std::shared_ptr<std::condition_variable>& delay_condition,
                const std::shared_ptr<std::mutex>& delay_mutex,
                const std::shared_ptr<bool>& spurious_wakeup);
    virtual ~DataDelayer() override;
    grpc::Status Finish();
    bool Read(com::vmware::concord::thin_replica::Data* msg);
  };

  std::shared_ptr<MockDataStreamPreparer> undelayed_data_preparer_;
  std::shared_ptr<std::condition_variable> delay_condition_;
  std::shared_ptr<bool> spurious_wakeup_;
  std::shared_ptr<std::mutex> condition_mutex_;

 public:
  DelayedMockDataStreamPreparer(
      std::shared_ptr<MockDataStreamPreparer>& data,
      std::shared_ptr<std::condition_variable> condition,
      std::shared_ptr<bool> spurious_wakeup_indicator,
      std::shared_ptr<std::mutex> condition_mutex);
  virtual ~DelayedMockDataStreamPreparer();
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  ReadStateRaw(grpc::ClientContext* context,
               const com::vmware::concord::thin_replica::ReadStateRequest&
                   request) const override;
  virtual grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  SubscribeToUpdatesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request)
      const override;
};

// Mock hasher class which provides a hashing implementation corresponding to
// what hashes a Thin Replica Server (or at least a Thin Replica Server that is
// not Byzantine-faulty in such a way that it returns hashes disagreeing with
// its own data) would return, given a MockDataStreamPreparer object returning
// the update data that server returns for any MockDataStreamPreparer object
// that is guaranteed to return its updates in a consistent and monotonically
// increasing order.
class MockOrderedDataStreamHasher {
 private:
  typedef size_t StateHashType;
  typedef size_t UpdateHashType;

  class StreamHasher : public MockThinReplicaStream<
                           com::vmware::concord::thin_replica::Hash>::State {
   private:
    std::unique_ptr<
        grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>>
        data_stream_;

   public:
    StreamHasher(
        grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
            data);
    virtual ~StreamHasher() override;
    grpc::Status Finish();
    bool Read(com::vmware::concord::thin_replica::Hash* msg);
  };

  std::shared_ptr<MockDataStreamPreparer> data_preparer_;
  uint64_t base_block_id_;

 public:
  MockOrderedDataStreamHasher(std::shared_ptr<MockDataStreamPreparer>& data);

  grpc::Status ReadStateHash(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::ReadStateHashRequest& request,
      com::vmware::concord::thin_replica::Hash* response) const;
  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Hash>*
  SubscribeToUpdateHashesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request)
      const;
};

// Class used for storing records of RPC calls to Thin Replica Servers made by a
// ThinReplicaClient; this is intended for use in testing that the
// ThinReplicaClient implementation complies with the intended use(s) of these
// RPC calls as described in the Thin Replica Mechanism design. Note this class
// handles synchronization of writes to these records across multiple threads.
// Call records are structured as lists of pairs of size_ts (representing the
// server the call was made to) and Protobuf messages (representing the
// parameter given for the call) or as lists of just size_ts (representing the
// server the call was made to) for RPC calls that do not use parameters.
class ThinReplicaCommunicationRecord {
 private:
  std::list<
      std::pair<size_t, com::vmware::concord::thin_replica::ReadStateRequest>>
      read_state_calls_;
  std::list<std::pair<size_t,
                      com::vmware::concord::thin_replica::ReadStateHashRequest>>
      read_state_hash_calls_;
  std::list<std::pair<size_t,
                      com::vmware::concord::thin_replica::SubscriptionRequest>>
      subscribe_to_updates_calls_;
  std::list<std::pair<size_t, com::vmware::concord::thin_replica::BlockId>>
      ack_update_calls_;
  std::list<std::pair<size_t,
                      com::vmware::concord::thin_replica::SubscriptionRequest>>
      subscribe_to_update_hashes_calls_;
  std::list<size_t> unsubscribe_calls_;

  std::mutex record_mutex_;

 public:
  void ClearRecords();

  void RecordReadState(
      size_t server_index,
      const com::vmware::concord::thin_replica::ReadStateRequest& request);
  void RecordReadStateHash(
      size_t server_index,
      const com::vmware::concord::thin_replica::ReadStateHashRequest& request);
  void RecordSubscribeToUpdates(
      size_t server_index,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request);
  void RecordAckUpdate(
      size_t server_index,
      const com::vmware::concord::thin_replica::BlockId& block_id);
  void RecordSubscribeToUpdateHashes(
      size_t server_index,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request);
  void RecordUnsubscribe(size_t server_index);

  const std::list<
      std::pair<size_t, com::vmware::concord::thin_replica::ReadStateRequest>>&
  GetReadStateCalls() const;
  const std::list<std::pair<
      size_t, com::vmware::concord::thin_replica::ReadStateHashRequest>>&
  GetReadStateHashCalls() const;
  const std::list<std::pair<
      size_t, com::vmware::concord::thin_replica::SubscriptionRequest>>&
  GetSubscribeToUpdatesCalls() const;
  const std::list<
      std::pair<size_t, com::vmware::concord::thin_replica::BlockId>>&
  GetAckUpdateCalls() const;
  const std::list<std::pair<
      size_t, com::vmware::concord::thin_replica::SubscriptionRequest>>&
  GetSubscribeToUpdateHashesCalls() const;
  const std::list<size_t>& GetUnsubscribeCalls() const;
  size_t GetTotalCallCount() const;
};

// Class for recording Thin Replica RPC calls to a given mocked Thin Replica
// Server, given the data stream preparer and hasher for that server and a
// ThinReplicaCommunicationRecord to record the calls to.
class MockThinReplicaServerRecorder {
 private:
  std::shared_ptr<MockDataStreamPreparer> data_preparer_;
  std::shared_ptr<MockOrderedDataStreamHasher> hasher_;
  std::shared_ptr<ThinReplicaCommunicationRecord> record_;
  size_t server_index_;

 public:
  MockThinReplicaServerRecorder(
      std::shared_ptr<MockDataStreamPreparer> data,
      std::shared_ptr<MockOrderedDataStreamHasher> hasher,
      std::shared_ptr<ThinReplicaCommunicationRecord> record,
      size_t server_index);

  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  ReadStateRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::ReadStateRequest& request);
  grpc::Status ReadStateHash(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::ReadStateHashRequest& request,
      com::vmware::concord::thin_replica::Hash* response);
  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*
  SubscribeToUpdatesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request);
  grpc::Status AckUpdate(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::BlockId& block_id,
      google::protobuf::Empty* response);
  grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Hash>*
  SubscribeToUpdateHashesRaw(
      grpc::ClientContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest& request);
  grpc::Status Unsubscribe(grpc::ClientContext* context,
                           const google::protobuf::Empty& request,
                           google::protobuf::Empty* response);
};

std::vector<MockThinReplicaServerRecorder> CreateMockServerRecorders(
    size_t num_servers, std::shared_ptr<MockDataStreamPreparer> data,
    std::shared_ptr<MockOrderedDataStreamHasher> hasher,
    std::shared_ptr<ThinReplicaCommunicationRecord> record);

void SetMockServerBehavior(
    MockTrsConnection* server,
    const std::shared_ptr<MockDataStreamPreparer>& data_preparer,
    const MockOrderedDataStreamHasher& hasher);
void SetMockServerBehavior(MockTrsConnection* server,
                           MockThinReplicaServerRecorder& mock_server_recorder);

void SetMockServerUnresponsive(thin_replica_client::TrsConnection* server);

std::vector<std::unique_ptr<thin_replica_client::TrsConnection>>
CreateTrsConnections(size_t num_servers, size_t num_unresponsive = 0);
std::vector<std::unique_ptr<thin_replica_client::TrsConnection>>
CreateTrsConnections(size_t num_servers,
                     std::shared_ptr<MockDataStreamPreparer> stream_preparer,
                     MockOrderedDataStreamHasher& hasher,
                     size_t num_unresponsive = 0);
std::vector<std::unique_ptr<thin_replica_client::TrsConnection>>
CreateTrsConnections(
    std::vector<MockThinReplicaServerRecorder>& mock_server_recorders);

template <class DataType>
grpc::ClientReaderInterface<DataType>* CreateUnresponsiveMockStream() {
  auto stream = new MockThinReplicaStream<DataType>();
  ON_CALL(*stream, Finish)
      .WillByDefault(testing::Return(grpc::Status(
          grpc::StatusCode::UNAVAILABLE, "This server is non-responsive")));
  ON_CALL(*stream, Read).WillByDefault(testing::Return(false));
  return stream;
}

#endif  // THIN_REPLICA_CLIENT_MOCKS_HPP
