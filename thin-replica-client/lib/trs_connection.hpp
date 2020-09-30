// Copyright 2020 VMware, all rights reserved

#ifndef THIN_REPLICA_CLIENT_TRS_CONNECTION_HPP_
#define THIN_REPLICA_CLIENT_TRS_CONNECTION_HPP_

#include <algorithm>
#include <fstream>
#include <sstream>

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>
#include "thin_replica.grpc.pb.h"

using namespace std::chrono_literals;

namespace thin_replica_client {

// The default message size for incoming data is 4MiB but certain workloads
// demand a higher limit. With the tested workloads, the incoming message size
// from the TRS is less than 16MiB. This correlates with the maximum message
// size that we specify for the SBFT protocol in Concord's configuration file.
// Note: We can set the upper bound to unlimited (-1) but we do want to know
// when & why the message size increases.
const int kGrpcMaxInboundMsgSizeInBytes = 1 << 24;

class TrsConnection {
 public:
  TrsConnection(const std::string& address, const std::string& client_id)
      : logger_(log4cplus::Logger::getInstance("thin_replica_client.trsconn")),
        address_(address),
        client_id_(client_id) {}
  ~TrsConnection() { this->disconnect(); }

  // Connect & disconnect from the TRS
  virtual void connect();
  virtual bool isConnected();
  virtual void disconnect();

  // Open a data subscription stream (connection has to be established before)
  virtual void openDataStream(
      const com::vmware::concord::thin_replica::SubscriptionRequest& request);
  virtual void closeDataStream();
  virtual bool hasDataStream();
  virtual bool readData(com::vmware::concord::thin_replica::Data* data);

  // Open a hash subscription stream (connection has to be established before)
  virtual void openHashStream(
      com::vmware::concord::thin_replica::SubscriptionRequest& request);
  virtual void closeHashStream();
  virtual bool hasHashStream();
  virtual bool readHash(com::vmware::concord::thin_replica::Hash* hash);

  // Open a state subscription stream (connection has to be established before)
  virtual void openStateStream(
      const com::vmware::concord::thin_replica::ReadStateRequest& request);
  virtual bool closeStateStream();
  virtual bool hasStateStream();
  virtual bool readState(com::vmware::concord::thin_replica::Data* data);

  // Read the hash of the state defined by the request (connection has to be
  // established before)
  virtual bool readStateHash(
      const com::vmware::concord::thin_replica::ReadStateHashRequest& request,
      com::vmware::concord::thin_replica::Hash* hash);

  // Helper to print/log connection details
  friend std::ostream& operator<<(std::ostream& os, const TrsConnection& trsc) {
    return os << trsc.client_id_ << " (" << trsc.address_ << ")";
  }

 protected:
  // Helper function to deal with gRPC
  void createStub();
  void createChannel();

  log4cplus::Logger logger_;

  // Connection identifiers
  std::string address_;
  std::string client_id_;

  // Subscription streams
  std::unique_ptr<grpc::ClientContext> data_context_;
  std::unique_ptr<
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>>
      data_stream_;
  std::unique_ptr<grpc::ClientContext> state_context_;
  std::unique_ptr<
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>>
      state_stream_;
  std::unique_ptr<grpc::ClientContext> hash_context_;
  std::unique_ptr<
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Hash>>
      hash_stream_;

  // gRPC connection
  std::shared_ptr<grpc::Channel> channel_;
  std::unique_ptr<
      com::vmware::concord::thin_replica::ThinReplica::StubInterface>
      stub_;

  // This method parses the thin replica environment variable
  // "THIN_REPLICA_SETTINGS" to get value for a specified key.
  std::string parseThinReplicaEnv(const std::string& env,
                                  const std::string& key);

  // This method reads certificates from file if TLS is enabled
  void readCert(const std::string& input_filename, std::string& out_data);
};

}  // namespace thin_replica_client

#endif  // THIN_REPLICA_CLIENT_TRS_CONNECTION_HPP_
