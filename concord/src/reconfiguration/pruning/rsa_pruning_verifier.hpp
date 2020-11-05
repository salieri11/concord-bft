// Copyright 2019-2020 VMware, all rights reserved

#ifndef CONCORD_PRUINING_RSA_PRUNING_VERIFIER_HPP
#define CONCORD_PRUINING_RSA_PRUNING_VERIFIER_HPP

#include "config/configuration_manager.hpp"
#include "src/bftengine/Crypto.hpp"

#include "concord.cmf.hpp"

#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace concord {
namespace reconfiguration {
namespace pruning {

// This class verifies pruning messages that were signed by serializing message
// contents via mechanisms provided in pruning_serialization.hpp/cpp . Public
// keys for verification are extracted from the passed configuration.
//
// Idea is to use the principal_id as an ID that identifies senders in pruning
// messages since it is unique across clients and replicas.
class RSAPruningVerifier {
 public:
  // Construct by passing the system configuration.
  RSAPruningVerifier(const concord::config::ConcordConfiguration &config);

  // Verify() methods verify that the message comes from the advertised sender.
  // Methods return true on successful verification and false on unsuccessful.
  // An exception is thrown on error.
  //
  // Note RSAPruningVerifier::Verify(const com::vmware::concord::PruneRequest&)
  // handles verification of the LatestPrunableBlock message(s) contained within
  // the PruneRequest, but does not itself handle verification of the issuing
  // operator's signature of the pruning command, as the operator's signature is
  // a dedicated application-level signature rather than one of the Concord-BFT
  // Principal's RSA signatures.
  bool Verify(const concord::messages::LatestPrunableBlock &) const;
  bool Verify(const concord::messages::PruneRequest &) const;

 private:
  struct Replica {
    std::uint64_t principal_id{0};
    bftEngine::impl::RSAVerifier verifier;
  };

  bool Verify(std::uint64_t sender, const std::string &ser,
              const std::string &signature) const;

  using ReplicaVector = std::vector<Replica>;

  // Get a replica from the replicas vector by its index.
  const Replica &GetReplica(ReplicaVector::size_type idx) const;

  // A vector of all the replicas in the system.
  ReplicaVector replicas_;
  // We map a principal_id to a replica index in the replicas_ vector to be able
  // to verify a message through the Replica's verifier that is associated with
  // its public key.
  std::unordered_map<std::uint64_t, ReplicaVector::size_type>
      principal_to_replica_idx_;

  // Contains a set of replica principal_ids for use in verification. Filled in
  // once during construction.
  std::unordered_set<std::uint64_t> replica_ids_;
};

}  // namespace pruning
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_PRUINING_RSA_PRUNING_VERIFIER_HPP
