// Copyright 2020 VMware, all rights reserved

#ifndef THIN_REPLICA_CLIENT_MOCKS_HPP
#define THIN_REPLICA_CLIENT_MOCKS_HPP

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "thin_replica.grpc.pb.h"

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

class MockThinReplicaServer
    : public com::vmware::concord::thin_replica::ThinReplica::StubInterface {
 public:
  // Functions the Thin Replica Client Library is known to currently use.
  MOCK_METHOD3(
      ReadStateHash,
      grpc::Status(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::ReadStateHashRequest&
              request,
          com::vmware::concord::thin_replica::Hash* response));
  MOCK_METHOD3(
      AckUpdate,
      grpc::Status(grpc::ClientContext* context,
                   const com::vmware::concord::thin_replica::BlockId& request,
                   google::protobuf::Empty* response));
  MOCK_METHOD3(Unsubscribe, grpc::Status(grpc::ClientContext* context,
                                         const google::protobuf::Empty& request,
                                         google::protobuf::Empty* response));
  MOCK_METHOD2(
      ReadStateRaw,
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::ReadStateRequest& request));
  MOCK_METHOD2(
      SubscribeToUpdatesRaw,
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::SubscriptionRequest&
              request));
  MOCK_METHOD2(
      SubscribeToUpdateHashesRaw,
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Hash>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::SubscriptionRequest&
              request));

  // Other virtual StubInterface functions currently thought to be unused by the
  // Thin Replica Client Library.
  MOCK_METHOD4(
      AsyncReadStateRaw,
      grpc::ClientAsyncReaderInterface<
          com::vmware::concord::thin_replica::Data>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::ReadStateRequest& request,
          grpc::CompletionQueue* cq, void* tag));

  MOCK_METHOD3(
      PrepareAsyncReadStateRaw,
      grpc::ClientAsyncReaderInterface<
          com::vmware::concord::thin_replica::Data>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::ReadStateRequest& request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD3(
      AsyncReadStateHashRaw,
      grpc::ClientAsyncResponseReaderInterface<
          com::vmware::concord::thin_replica::Hash>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::ReadStateHashRequest&
              request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD3(
      PrepareAsyncReadStateHashRaw,
      grpc::ClientAsyncResponseReaderInterface<
          com::vmware::concord::thin_replica::Hash>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::ReadStateHashRequest&
              request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD4(
      AsyncSubscribeToUpdatesRaw,
      grpc::ClientAsyncReaderInterface<
          com::vmware::concord::thin_replica::Data>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::SubscriptionRequest&
              request,
          grpc::CompletionQueue* cq, void* tag));

  MOCK_METHOD3(
      PrepareAsyncSubscribeToUpdatesRaw,
      grpc::ClientAsyncReaderInterface<
          com::vmware::concord::thin_replica::Data>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::SubscriptionRequest&
              request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD3(
      AsyncAckUpdateRaw,
      grpc::ClientAsyncResponseReaderInterface<google::protobuf::Empty>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::BlockId& request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD3(
      PrepareAsyncAckUpdateRaw,
      grpc::ClientAsyncResponseReaderInterface<google::protobuf::Empty>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::BlockId& request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD4(
      AsyncSubscribeToUpdateHashesRaw,
      grpc::ClientAsyncReaderInterface<
          com::vmware::concord::thin_replica::Hash>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::SubscriptionRequest&
              request,
          grpc::CompletionQueue* cq, void* tag));

  MOCK_METHOD3(
      PrepareAsyncSubscribeToUpdateHashesRaw,
      grpc::ClientAsyncReaderInterface<
          com::vmware::concord::thin_replica::Hash>*(
          grpc::ClientContext* context,
          const com::vmware::concord::thin_replica::SubscriptionRequest&
              request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD3(
      AsyncUnsubscribeRaw,
      grpc::ClientAsyncResponseReaderInterface<google::protobuf::Empty>*(
          grpc::ClientContext* context, const google::protobuf::Empty& request,
          grpc::CompletionQueue* cq));

  MOCK_METHOD3(
      PrepareAsyncUnsubscribeRaw,
      grpc::ClientAsyncResponseReaderInterface<google::protobuf::Empty>*(
          grpc::ClientContext* context, const google::protobuf::Empty& request,
          grpc::CompletionQueue* cq));
};

#endif  // THIN_REPLICA_CLIENT_MOCKS_HPP
