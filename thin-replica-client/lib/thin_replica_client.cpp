// Copyright 2019 VMware, all rights reserved

#include "thin_replica_client.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::ReadStateRequest;
using grpc::ClientContext;
using grpc::Status;
using std::pair;
using std::runtime_error;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;
using thin_replica_client::RingBufferUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::Update;

RingBufferUpdateQueue::RingBufferUpdateQueue(size_t buffer_size) {}

RingBufferUpdateQueue::~RingBufferUpdateQueue() {}

RingBufferUpdateQueue::RingBufferUpdateQueue(
    const RingBufferUpdateQueue& other) {}

RingBufferUpdateQueue::RingBufferUpdateQueue(
    const RingBufferUpdateQueue&& other) {}

RingBufferUpdateQueue& RingBufferUpdateQueue::operator=(
    const RingBufferUpdateQueue& other) {
  return *this;
}

RingBufferUpdateQueue& RingBufferUpdateQueue::operator=(
    const RingBufferUpdateQueue&& other) {
  return *this;
}

void RingBufferUpdateQueue::Clear() {
  // TODO (Alex): Implement.
  log4cplus::Logger logger(log4cplus::Logger::getInstance(
      "concord::thin_replica_client::RingBufferUpdateQueue"));
  LOG4CPLUS_FATAL(logger,
                  "concord::thin_replica_client::RingBufferUpdateQueue::Clear "
                  "is unimplemented.");
}

void RingBufferUpdateQueue::Push(unique_ptr<Update> update) {
  // TODO (Alex): Implement.
  log4cplus::Logger logger(log4cplus::Logger::getInstance(
      "concord::thin_replica_client::RingBufferUpdateQueue"));
  LOG4CPLUS_FATAL(logger,
                  "concord::thin_replica_client::RingBufferUpdateQueue::Push "
                  "is unimplemented.");
}

unique_ptr<Update> RingBufferUpdateQueue::Pop() {
  // TODO (Alex): Implement.
  log4cplus::Logger logger(log4cplus::Logger::getInstance(
      "concord::thin_replica_client::RingBufferUpdateQueue"));
  LOG4CPLUS_FATAL(logger,
                  "concord::thin_replica_client::RingBufferUpdateQueue::Pop "
                  "is unimplemented.");
  return unique_ptr<Update>();
}

unique_ptr<Update> RingBufferUpdateQueue::TryPop() {
  // TODO (Alex): Implement.
  log4cplus::Logger logger(log4cplus::Logger::getInstance(
      "concord::thin_replica_client::RingBufferUpdateQueue"));
  LOG4CPLUS_FATAL(logger,
                  "concord::thin_replica_client::RingBufferUpdateQueue::"
                  "TryPop is unimplemented.");
  return unique_ptr<Update>();
}

ThinReplicaClient::~ThinReplicaClient() {}

void ThinReplicaClient::Subscribe(const string& key_prefix_bytes) {
  // XXX: The following implementation does not achieve Subscribe's specified
  //      interface and behavior (see the comments with Subscribe's declaration
  //      in the Thin Replica Client Library header file for documentation of
  //      that interface); this implementation is intended to establish minimal
  //      end-to-end connectivity with a non-faulty Thin Replica Server in order
  //      to preserve the general behavior of the example Thin Replica Client
  //      application (which at this time just connects to a server and checks
  //      the status returned for a ReadStateRequest).

  ReadStateRequest request;
  ClientContext context;

  assert(server_stubs_.size() > 0);
  assert(server_stubs_[0]);
  auto stream = server_stubs_[0]->ReadState(&context, request);

  Data response;
  stream->Read(&response);
  Status status = stream->Finish();

  if (!status.ok()) {
    stringstream error_message;
    error_message << "ThinReplicaClient subscription failed: response to "
                     "ReadStateRequest from ThinReplicaServer gave a failure "
                     "status (error code: "
                  << status.error_code() << ", error message: \""
                  << status.error_message() << "\").";
    throw runtime_error(error_message.str());
  }

  // TODO (Alex): Provide real implementation.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::Subscribe "
                  "is unimplemented.");
}

void ThinReplicaClient::Subscribe(const string& key_prefix_bytes,
                                  uint64_t last_known_block_id) {
  // TODO (Alex): Provide real implementation.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::Subscribe "
                  "is unimplemented.");
}

void ThinReplicaClient::Unsubscribe() {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::"
                  "Unsubscribe is unimplemented.");
}

void ThinReplicaClient::AcknowledgeBlockID(uint64_t block_id) {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::"
                  "AcknowledgeBlockID is unimplemented.");
}
