// Copyright 2020 VMware, all rights reserved

#include "trs_connection.hpp"

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>
#include "thin_replica.grpc.pb.h"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
using com::vmware::concord::thin_replica::ThinReplica;
using grpc_connectivity_state::GRPC_CHANNEL_READY;

using namespace std::chrono_literals;

namespace thin_replica_client {

void TrsConnection::createStub() {
  assert(channel_);
  stub_ = ThinReplica::NewStub(channel_);
}

void TrsConnection::createChannel() {
  grpc::ChannelArguments args;
  args.SetMaxReceiveMessageSize(kGrpcMaxInboundMsgSizeInBytes);
  channel_ =
      CreateCustomChannel(address_, grpc::InsecureChannelCredentials(), args);
}

void TrsConnection::connect() {
  if (!channel_) {
    createChannel();
    createStub();
  } else if (!stub_) {
    createStub();
  }
  // Initiate connection
  channel_->GetState(true);
}

bool TrsConnection::isConnected() {
  if (!channel_) {
    return false;
  }
  auto status = channel_->GetState(false);
  LOG4CPLUS_DEBUG(logger_,
                  "gRPC connection status (" << address_ << ") " << status);
  return status == GRPC_CHANNEL_READY;
}

void TrsConnection::disconnect() {
  closeStateStream();
  closeDataStream();
  closeHashStream();
  stub_.reset();
  channel_.reset();
}

void TrsConnection::openDataStream(const SubscriptionRequest& request) {
  assert(stub_);
  assert(!data_stream_);

  data_context_.reset(new grpc::ClientContext());
  data_context_->AddMetadata("client_id", client_id_);

  data_stream_ = stub_->SubscribeToUpdates(data_context_.get(), request);
}

void TrsConnection::closeDataStream() {
  if (!data_stream_) {
    return;
  }
  assert(data_context_);
  data_context_->TryCancel();
  data_stream_.reset();
}

bool TrsConnection::hasDataStream() { return bool(data_stream_); }

bool TrsConnection::readData(Data* data) {
  assert(data_stream_);
  return data_stream_->Read(data);
}

void TrsConnection::openStateStream(const ReadStateRequest& request) {
  assert(stub_);
  assert(!state_stream_);

  state_context_.reset(new grpc::ClientContext());
  state_context_->AddMetadata("client_id", client_id_);

  state_stream_ = stub_->ReadState(state_context_.get(), request);
}

bool TrsConnection::closeStateStream() {
  if (!state_stream_) {
    return true;
  }
  assert(state_context_);
  // "state" is not an infite data stream and we expect proper termination
  auto status = state_stream_->Finish();
  if (!status.ok()) {
    LOG4CPLUS_WARN(logger_, "Finishing ReadState from "
                                << address_ << " failed with error code: "
                                << status.error_code() << ", \""
                                << status.error_message() << "\").");
  }
  state_stream_.reset();
  return status.ok();
}

bool TrsConnection::hasStateStream() { return bool(state_stream_); }

bool TrsConnection::readState(Data* data) {
  assert(state_stream_);
  return state_stream_->Read(data);
}

bool TrsConnection::readStateHash(const ReadStateHashRequest& request,
                                  Hash* hash) {
  grpc::ClientContext hash_context;
  hash_context.AddMetadata("client_id", client_id_);
  auto status = stub_->ReadStateHash(&hash_context, request, hash);
  if (!status.ok()) {
    LOG4CPLUS_WARN(logger_, "ReadStateHash from "
                                << address_ << " failed with error code: "
                                << status.error_code() << ", \""
                                << status.error_message() << "\").");
  }
  return status.ok();
}

void TrsConnection::openHashStream(SubscriptionRequest& request) {
  assert(stub_);
  assert(!hash_stream_);

  hash_context_.reset(new grpc::ClientContext());
  hash_context_->AddMetadata("client_id", client_id_);

  hash_stream_ = stub_->SubscribeToUpdateHashes(hash_context_.get(), request);
}

void TrsConnection::closeHashStream() {
  if (!hash_stream_) {
    return;
  }
  assert(hash_context_);
  hash_context_->TryCancel();
  hash_stream_.reset();
}

bool TrsConnection::hasHashStream() { return bool(hash_stream_); }

bool TrsConnection::readHash(Hash* hash) {
  assert(hash_stream_);
  return hash_stream_->Read(hash);
}

}  // namespace thin_replica_client
