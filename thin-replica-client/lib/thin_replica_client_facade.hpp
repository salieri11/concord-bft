// Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential

// We intend to make this facade the primary supported interface between the
// Thin Replica Client Library and application(s) using it going forward, at
// least for the near-term future.
// TODO (Alex): As we would like to make this the primary interface, it and the
//              ThinReplicaClient class should be renamed to reframe
//              ThinReplicaClientFacade as the main outward-facing interface
//              (rather than a mere facade in front of another interface) and to
//              reframe ThinReplicaClient as the outwardly unexposed
//              implementation backing that interface.
// TODO (Alex): As we would like to make this the primary interface, comments
//              should be added to this header file to document this interface;
//              some of the comments from thin_replica_client.hpp can probably
//              be ported over here.

#ifndef THIN_REPLICA_CLIENT_FACADE_HPP_
#define THIN_REPLICA_CLIENT_FACADE_HPP_

#include "update.hpp"

#include <memory>

namespace thin_replica_client {

// ThinReplicaClientFacade provides a simplified interface to the
// ThinReplicaClient. Its include footprint is minimal, simplifying the build
// process of the caller code. Using this class does not enforce having grpc,
// log4plus or thin replica proto files in the include path.
class ThinReplicaClientFacade final {
 public:
  ThinReplicaClientFacade(
      const std::string& client_id, uint16_t max_faulty,
      const std::string& private_key,
      const std::vector<std::pair<std::string, std::string>>& servers,
      const std::string& jaeger_agent);
  ~ThinReplicaClientFacade();

  void Subscribe(const std::string& prefix);
  void Subscribe(const std::string& prefix, uint64_t block_id);
  void Unsubscribe();
  void ReleaseConsumers();

  std::unique_ptr<Update> Pop();
  std::unique_ptr<Update> TryPop();

  void AcknowledgeBlockID(uint64_t block_id);

 private:
  class Impl;
  std::unique_ptr<Impl> impl;

  // Constructor to create a Thin Replica Client manually given its internal
  // implementation object; this constructor is intended to facilitate testing
  // by enabling the construction of a ThinReplicaClientFacade object with mock
  // objects standing in for the Thin Replica Servers, and is not intended for
  // direct application use.
  ThinReplicaClientFacade(std::unique_ptr<Impl>&& impl);

  // Friend function testing code may implement in order to access the private
  // testing constructor.
  template <typename ThinReplicaServer>
  friend std::unique_ptr<ThinReplicaClientFacade>
  ConstructThinReplicaClientFacade(
      const std::string& client_id, uint16_t max_faulty,
      const std::string& private_key,
      std::vector<std::pair<std::string, ThinReplicaServer>>& mock_servers,
      const std::string& jaeger_agent);
};

}  // namespace thin_replica_client

#endif
