// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_PRUNING_KVB_PRUNING_SM_HPP
#define CONCORD_PRUNING_KVB_PRUNING_SM_HPP

#include <log4cplus/logger.h>

#include "rsa_pruning_signer.hpp"
#include "rsa_pruning_verifier.hpp"

#include "blockchain/db_interfaces.h"
#include "config/configuration_manager.hpp"

#include "concord.pb.h"

#include <opentracing/tracer.h>

#include <cstdint>

namespace concord {
namespace pruning {

// This class implements the KVB pruning state machine. Main functionalities
// include executing pruning based on configuration policy and replica states as
// well as providing read-only information to the operator node.
//
// The following configuration options are honored by the state machine:
//  * pruning_num_blocks_to_keep - a system-wide configuration option that
//  specifies a minimum number of blocks to always keep in storage when pruning.
//  If not specified, a value of 0 is assumed.
//
// The LatestPrunableBlockRequest command returns the latest block ID from the
// replica's storage minus the configured pruning_num_blocks_to_keep . If
// pruning_num_blocks_to_keep is greater than the latest block, 0 is returned.
//
// The PruneRequest command prunes blocks from storage by:
//  - verifying the PruneRequest message's signature
//  - verifying that the number of LatestPrunableBlock messages in the
//  PruneRequest is equal to the number of replicas in the system
//  - verifying the signatures of individual LatestPrunableBlock messages in the
//  PruneRequest.
// If all above conditions are met, the state machine will prune blocks from the
// genesis block up to the the minimum of all the block IDs in
// LatestPrunableBlock messages in the PruneRequest message.
class KVBPruningSM {
 public:
  // Construct by providing an interface to the storage engine, configuration
  // and tracing facilities.
  KVBPruningSM(const storage::blockchain::ILocalKeyValueStorageReadOnly&,
               const config::ConcordConfiguration& config,
               const config::ConcordConfiguration& node_config);

  // Handles a ConcordRequest that contains a pruning request. If an invalid
  // request has been passed or no pruning request is contained, the request is
  // ignored.
  void Handle(const com::vmware::concord::ConcordRequest&,
              com::vmware::concord::ConcordResponse&, bool read_only,
              opentracing::Span& parent_span) const;

 private:
  void Handle(const com::vmware::concord::LatestPrunableBlockRequest&,
              com::vmware::concord::ConcordResponse&,
              opentracing::Span& parent_span) const;
  void Handle(const com::vmware::concord::PruneRequest&,
              com::vmware::concord::ConcordResponse&, bool read_only,
              opentracing::Span& parent_span) const;

 private:
  log4cplus::Logger logger_;
  RSAPruningSigner signer_;
  RSAPruningVerifier verifier_;
  const storage::blockchain::ILocalKeyValueStorageReadOnly& ro_storage_;
  std::uint64_t replica_id_{0};
  std::uint64_t num_blocks_to_keep_{0};
};

}  // namespace pruning
}  // namespace concord

#endif  // CONCORD_PRUNING_KVB_PRUNING_SM_HPP
