// Copyright 2019 VMware, all rights reserved

#include "thin_replica_client.hpp"

using grpc::ClientContext;
using grpc::Status;

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::ReadStateRequest;

grpc::Status ThinReplicaClient::ReadState() {
  ReadStateRequest request;
  ClientContext context;

  LOG4CPLUS_INFO(logger_, "ReadState()");
  auto stream = stub_->ReadState(&context, request);

  Data response;
  stream->Read(&response);

  return stream->Finish();
}
