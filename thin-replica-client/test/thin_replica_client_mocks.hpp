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

#endif  // THIN_REPLICA_CLIENT_MOCKS_HPP
