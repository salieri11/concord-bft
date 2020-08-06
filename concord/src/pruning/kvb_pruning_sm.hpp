// Copyright 2019-2020 VMware, all rights reserved

#ifndef CONCORD_PRUNING_KVB_PRUNING_SM_HPP
#define CONCORD_PRUNING_KVB_PRUNING_SM_HPP

#include "Logger.hpp"

#include "rsa_pruning_signer.hpp"
#include "rsa_pruning_verifier.hpp"

#include "IStateTransfer.hpp"
#include "config/configuration_manager.hpp"
#include "db_interfaces.h"
#include "kv_types.hpp"
#include "sliver.hpp"
#include "time/time_contract.hpp"
#include "utils/openssl_crypto_utils.hpp"

#include "concord.pb.h"

#include <opentracing/tracer.h>

#include <cstdint>
#include <optional>

namespace concord {
namespace pruning {

// This class implements the KVB pruning state machine. Main functionalities
// include executing pruning based on configuration policy and replica states as
// well as providing read-only information to the operator node.
//
// The following configuration options are honored by the state machine:
//  * pruning_enabled - a system-wide configuration option that indicates if
//  pruning is enabled for the replcia. If set to false,
//  LatestPrunableBlockRequest will return 0 as a latest block(indicating no
//  blocks can be pruned) and PruneRequest will return an error. If not
//  specified, a value of false is assumed.
//
//  * pruning_num_blocks_to_keep - a system-wide configuration option that
//  specifies a minimum number of blocks to always keep in storage when pruning.
//  If not specified, a value of 0 is assumed. If
//  pruning_duration_to_keep_minutes is specified too, the more conservative
//  pruning range of the two options will be used (the one that prunes less
//  blocks).
//
//  * pruning_duration_to_keep_minutes - a system-wide configuration option that
//  specifies a time range (in minutes) from now to the past that determines
//  which blocks to keep and which are older than (now -
//  pruning_duration_to_keep_minutes) and can, therefore, be pruned. If not
//  specified, a value of 0 is assumed. If pruning_num_blocks_to_keep is
//  specified too, the more conservative pruning range of the two will be used
//  (the one that prunes less blocks). This option requires the time service to
//  be enabled.
//
//  * pruning_operator_public_key - the public key for a priviliged operator
//  authorized to issue pruning commands. This parameter is required if pruning
//  is enabled. Pruning commands that do not have a correct signature produced
//  with the private key corresponding to this public key will be rejected.
//
// The LatestPrunableBlockRequest command returns the latest block ID from the
// replica's storage that is safe to prune. If no blocks can be pruned, 0 is
// returned.
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
  // Construct by providing an interface to the storage engine, state transfer,
  // configuration and tracing facilities. Note this constructor may throw an
  // exception if there is an issue with the configuration (for example, if the
  // configuration enables pruning but does not provide a purning operator
  // public key).
  KVBPruningSM(const kvbc::ILocalKeyValueStorageReadOnly&,
               kvbc::IBlocksAppender&, kvbc::IBlocksDeleter&,
               bftEngine::IStateTransfer&,
               const config::ConcordConfiguration& config,
               const config::ConcordConfiguration& node_config,
               concord::time::TimeContract*);

  // Handles a ConcordRequest that contains a pruning request. If an invalid
  // request has been passed or no pruning request is contained, the request is
  // ignored.
  void Handle(const com::vmware::concord::ConcordRequest&,
              com::vmware::concord::ConcordResponse&, bool read_only,
              opentracing::Span& parent_span) const;

  static concordUtils::Sliver LastAgreedPrunableBlockIdKey();

  // Given a PruneRequest Protobuf message, compute the exact byte string of
  // data that the pruning state machine expects the operator's signature to be
  // made over in order to prove the legitimacy of that PruneRequest. Note that
  // the (operator's) signature field itself of the PruneRequest message does
  // not contribute to this signable data, though (replicas') signature fields
  // in the individual LatestPrunableBlock messages contained in the
  // PruneRequest message may.
  static std::string GetSignablePruneCommandData(
      const com::vmware::concord::PruneRequest& prune_request);

 private:
  void Handle(const com::vmware::concord::LatestPrunableBlockRequest&,
              com::vmware::concord::ConcordResponse&,
              opentracing::Span& parent_span) const;
  void Handle(const com::vmware::concord::PruneRequest&,
              com::vmware::concord::ConcordResponse&, bool read_only,
              opentracing::Span& parent_span) const;

  kvbc::BlockId LatestBasedOnNumBlocksConfig() const;
  kvbc::BlockId LatestBasedOnTimeRangeConfig() const;

  kvbc::BlockId AgreedPrunableBlockId(
      const com::vmware::concord::PruneRequest&) const;
  // Returns the last agreed prunable block ID from storage, if existing.
  std::optional<kvbc::BlockId> LastAgreedPrunableBlockId() const;
  void PersistLastAgreedPrunableBlockId(kvbc::BlockId block_id) const;
  // Prune blocks in the [genesis, block_id] range (both inclusive).
  // Throws on errors.
  void PruneThroughBlockId(kvbc::BlockId block_id) const;
  void PruneThroughLastAgreedBlockId() const;
  void PruneOnStateTransferCompletion(uint64_t checkpoint_number) const
      noexcept;
  void PruneThroughBlockId(kvbc::BlockId block_id,
                           com::vmware::concord::PruneResponse&) const noexcept;

 private:
  logging::Logger logger_;
  RSAPruningSigner signer_;
  RSAPruningVerifier verifier_;
  const kvbc::ILocalKeyValueStorageReadOnly& ro_storage_;
  kvbc::IBlocksAppender& blocks_appender_;
  kvbc::IBlocksDeleter& blocks_deleter_;
  concord::time::TimeContract* time_contract_{nullptr};
  bool pruning_enabled_{false};
  std::uint64_t replica_id_{0};
  std::uint64_t num_blocks_to_keep_{0};
  std::uint32_t duration_to_keep_minutes_{0};
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPublicKey>
      operator_public_key_{nullptr};
  static const concordUtils::Sliver last_agreed_prunable_block_id_key_;
};

}  // namespace pruning
}  // namespace concord

#endif  // CONCORD_PRUNING_KVB_PRUNING_SM_HPP
