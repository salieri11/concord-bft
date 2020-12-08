// Copyright 2019-2020 VMware, all rights reserved

#include "kvb_pruning_sm.hpp"

#include "assertUtils.hpp"
#include "blockchain_view.h"
#include "config/configuration_manager.hpp"
#include "endianness.hpp"

#include <google/protobuf/timestamp.pb.h>
#include <google/protobuf/util/time_util.h>

#include <algorithm>
#include <cstdlib>
#include <exception>
#include <iterator>
#include <stdexcept>
#include <utility>

namespace concord {

using concord::time::TimeContract;
using concord::utils::openssl_crypto::DeserializePublicKey;
using concordUtils::Sliver;
using kvbc::BaseBlockInfo;
using kvbc::BlockchainView;
using kvbc::BlockId;
using kvbc::ILocalKeyValueStorageReadOnly;
using kvbc::SetOfKeyValuePairs;

using google::protobuf::Timestamp;
using google::protobuf::util::TimeUtil;
using std::invalid_argument;
using std::stringstream;

using namespace std::string_literals;

namespace reconfiguration {
namespace pruning {

const Sliver KVBPruningSM::last_agreed_prunable_block_id_key_{
    std::string{concord::storage::kKvbKeyLastAgreedPrunableBlockId}};

KVBPruningSM::KVBPruningSM(const ILocalKeyValueStorageReadOnly& ro_storage,
                           kvbc::IBlocksAppender& blocks_appender,
                           kvbc::IBlocksDeleter& blocks_deleter,
                           bftEngine::IStateTransfer& state_transfer,
                           const config::ConcordConfiguration& config,
                           const config::ConcordConfiguration& node_config,
                           TimeContract* time_contract)
    : logger_{logging::getLogger("concord.pruning")},
      signer_{node_config},
      verifier_{config},
      ro_storage_{ro_storage},
      blocks_appender_{blocks_appender},
      blocks_deleter_{blocks_deleter},
      time_contract_{time_contract},
      replica_id_{node_config.subscope("replica", 0)
                      .getValue<decltype(replica_id_)>("principal_id")} {
  if (config.hasValue<decltype(pruning_enabled_)>("pruning_enabled")) {
    pruning_enabled_ =
        config.getValue<decltype(pruning_enabled_)>("pruning_enabled");
  }

  if (config.hasValue<decltype(num_blocks_to_keep_)>(
          "pruning_num_blocks_to_keep")) {
    num_blocks_to_keep_ = config.getValue<decltype(num_blocks_to_keep_)>(
        "pruning_num_blocks_to_keep");
  }

  if (config.hasValue<decltype(duration_to_keep_minutes_)>(
          "pruning_duration_to_keep_minutes")) {
    duration_to_keep_minutes_ =
        config.getValue<decltype(duration_to_keep_minutes_)>(
            "pruning_duration_to_keep_minutes");
  }

  // Make sure that blocks from old genesis through the last agreed block are
  // pruned. That might be violated if there was a crash during pruning itself.
  // Therefore, call it every time on startup to ensure no old blocks are
  // present before we allow the system to proceed.
  PruneThroughLastAgreedBlockId();

  // If a replica has missed Prune commands for whatever reason, we still need
  // to execute them. We do that by saving pruning data in the state and later
  // using it to prune relevant blocks when we receive it from state transfer.
  state_transfer.addOnTransferringCompleteCallback(
      [this](uint64_t checkpoint_number) {
        PruneOnStateTransferCompletion(checkpoint_number);
      });
}

Sliver KVBPruningSM::LastAgreedPrunableBlockIdKey() {
  return last_agreed_prunable_block_id_key_;
}

// Helper function to GetSignablePruneCommandData.
static void UInt64BytesToStreamLittleEndian(const uint64_t* num,
                                            stringstream& stream) {
#ifndef BOOST_LITTLE_ENDIAN
#ifndef BOOST_BIG_ENDIAN
  static_assert(false,
                "Cannot determine architecture endianness (neither "
                "BOOST_BIG_ENDIAN nor BOOST_LITTLE_ENDIAN is defined).");
#endif  // BOOST_BIG_ENDIAN not defined
#endif  // BOOST_LITTLE_ENDIAN not defined

  const char* bytes = reinterpret_cast<const char*>(num);
#ifdef BOOST_LITTLE_ENDIAN
  for (size_t i = 0; i < sizeof(uint64_t); ++i) {
    stream.put(*bytes);
    ++bytes;
  }
#else   // BOOST_LITTLE_ENDIAN not defined in this case
  bytes += sizeof(uint64_t);
  for (size_t i = 0; i < sizeof(uint64_t); ++i) {
    --bytes;
    stream.put(*bytes);
  }
#endif  // if BOOST_LITTLE_ENDIAN defined/else
}

bool KVBPruningSM::Handle(
    const concord::messages::LatestPrunableBlockRequest& request,
    concord::messages::LatestPrunableBlock& latest_prunable_block,
    opentracing::Span& parent_span) const {
  auto latest_prunable_block_span = opentracing::Tracer::Global()->StartSpan(
      "latest_prunable_block_request",
      {opentracing::ChildOf(&parent_span.context())});

  // If pruning is disabled, return 0. Otherwise, be conservative and prune the
  // smaller block range.

  const auto latest_prunable_block_id =
      pruning_enabled_ ? std::min(LatestBasedOnNumBlocksConfig(),
                                  LatestBasedOnTimeRangeConfig())
                       : 0;
  latest_prunable_block.replica = replica_id_;
  latest_prunable_block.block_id = latest_prunable_block_id;
  signer_.Sign(latest_prunable_block);
  return true;
}

std::optional<kvbc::BlockId> KVBPruningSM::Handle(
    const concord::messages::PruneRequest& request, bool read_only,
    opentracing::Span& parent_span) const {
  auto prune_span = opentracing::Tracer::Global()->StartSpan(
      "prune_request", {opentracing::ChildOf(&parent_span.context())});

  if (read_only) {
    LOG_WARN(logger_,
             "KVBPruningSM ignoring PruneRequest in a read-only command");
    return {};
  }

  if (!pruning_enabled_) {
    const auto msg =
        "KVBPruningSM pruning is disabled, returning an error on PruneRequest";
    LOG_WARN(logger_, msg);
    return {};
  }

  const auto sender = request.sender;

  if (!verifier_.Verify(request)) {
    LOG_WARN(
        logger_,
        "KVBPruningSM failed to verify PruneRequest from principal_id "
            << sender
            << " on the grounds that the pruning request did not include "
               "LatestPrunableBlock responses from the required replicas, or "
               "on the grounds that some non-empty subset of those "
               "LatestPrunableBlock messages did not bear correct signatures "
               "from the claimed replicas.");
    return {};
  }

  const auto latest_prunable_block_id = AgreedPrunableBlockId(request);
  // Make sure we have persisted the agreed prunable block ID before proceeding.
  // Rationale is that we want to be able to pick up in case of a crash.
  PersistLastAgreedPrunableBlockId(latest_prunable_block_id);
  // Execute actual pruning.

  PruneThroughBlockId(latest_prunable_block_id);
  return latest_prunable_block_id;
}

BlockId KVBPruningSM::LatestBasedOnNumBlocksConfig() const {
  const auto last_block_id = ro_storage_.getLastBlock();
  if (last_block_id < num_blocks_to_keep_) {
    return 0;
  }
  return last_block_id - num_blocks_to_keep_;
}

namespace {
class TimestampedBlockInfo : public BaseBlockInfo {
 public:
  // Construct with a dummy value of 0 for the block info.
  TimestampedBlockInfo(const Timestamp& timestamp)
      : BaseBlockInfo{0}, timestamp_{timestamp} {}
  TimestampedBlockInfo(BlockId id, TimeContract* time_contract)
      : BaseBlockInfo{id}, time_contract_{time_contract} {}

  void loadIndices() {
    timestamp_ = time_contract_->GetSummarizedTimeAtBlock(id());
  }

  void loadData() {}

  const Timestamp& timestamp() const { return timestamp_; };

 private:
  Timestamp timestamp_;
  TimeContract* time_contract_{nullptr};
};

const auto TimestampCompare = [](const TimestampedBlockInfo& lhs,
                                 const TimestampedBlockInfo& rhs) {
  return lhs.timestamp() < rhs.timestamp();
};
}  // namespace

BlockId KVBPruningSM::LatestBasedOnTimeRangeConfig() const {
  const auto last_block_id = ro_storage_.getLastBlock();
  if (last_block_id == 0) {
    // Assume there is no block with ID of 0.
    return 0;
  } else if (!time_contract_ || duration_to_keep_minutes_ == 0) {
    // If time-based pruning is not configured or the time service is disabled,
    // don't impose any restrictions on the pruning range and prune up to the
    // last block ID.
    return last_block_id;
  }

  const auto view = BlockchainView<TimestampedBlockInfo, TimeContract*>{
      ro_storage_.getGenesisBlock(), last_block_id, time_contract_};
  const auto now = time_contract_->GetTime();
  const auto prune_to_ts_info = TimestampedBlockInfo{
      now - TimeUtil::MinutesToDuration(duration_to_keep_minutes_)};

  // Binary search over the blockchain. std::lower_bound() will return the first
  // block that has a timestamp that is greater than or equal to
  // prune_to_ts_info . Therefore, we will prune from genesis to this block or
  // the previous one. Reason is that std::lower_bound() might not find an exact
  // match and, in this case, we are conservative and prune up to the previous
  // block.
  auto block_it = std::lower_bound(std::cbegin(view), std::cend(view),
                                   prune_to_ts_info, TimestampCompare);
  if (block_it == std::cend(view)) {
    // This can happen if the TimeContract has been updated, but the current
    // block hasn't been written to storage. In this case, return the current
    // last block, because std::lower_bound() is telling us that no block
    // timestamp is greater than or equal to prune_to_ts_info (meaning all
    // blocks are older).
    return last_block_id;
  }
  // If we get the genesis block or we have an exact match, return it as the
  // latest prunable block ID. Otherwise, return the previous one.
  else if (block_it == std::cbegin(view) ||
           block_it->timestamp() == prune_to_ts_info.timestamp()) {
    return block_it->id();
  }
  return (block_it - 1)->id();
}

kvbc::BlockId KVBPruningSM::AgreedPrunableBlockId(
    const concord::messages::PruneRequest& prune_request) const {
  const auto latest_prunable_blocks = prune_request.latest_prunable_block;
  const auto begin = std::cbegin(latest_prunable_blocks);
  const auto end = std::cend(latest_prunable_blocks);
  ConcordAssertNE(begin, end);
  return std::min_element(begin, end,
                          [](const auto& a, const auto& b) {
                            return (a.block_id < b.block_id);
                          })
      ->block_id;
}

std::optional<BlockId> KVBPruningSM::LastAgreedPrunableBlockId() const {
  auto raw = Sliver{};
  const auto status = ro_storage_.get(last_agreed_prunable_block_id_key_, raw);
  // TODO: Handle get()'s behavior of returning an empty value in case the key
  // is not found. Check the status for isNotFound() too, in case get()'s
  // implementation changes. This is ugly.
  if (!status.isOK()) {
    if (status.isNotFound()) {
      return std::nullopt;
    }
    throw std::runtime_error{
        "KVBPruningSM failed to get latest agreed prunable block ID, reason: " +
        status.toString()};
  } else if (raw.empty()) {
    return std::nullopt;
  }
  ConcordAssertEQ(raw.length(), sizeof(BlockId));
  return concordUtils::fromBigEndianBuffer<BlockId>(raw.data());
}

void KVBPruningSM::PersistLastAgreedPrunableBlockId(
    kvbc::BlockId block_id) const {
  const auto block = SetOfKeyValuePairs{
      std::make_pair(last_agreed_prunable_block_id_key_,
                     concordUtils::toBigEndianStringBuffer(block_id))};
  auto added_block_id = BlockId{};
  const auto status = blocks_appender_.addBlock(block, added_block_id);
  if (!status.isOK()) {
    throw std::runtime_error{
        "KVBPruningSM failed to persist last agreed prunable block ID, "
        "reason: " +
        status.toString()};
  }
}

void KVBPruningSM::PruneThroughBlockId(BlockId block_id) const {
  const auto genesis_block_id = ro_storage_.getGenesisBlock();
  if (block_id >= genesis_block_id) {
    blocks_deleter_.deleteBlocksUntil(block_id + 1);
  }
}

void KVBPruningSM::PruneThroughLastAgreedBlockId() const {
  const auto last_agreed = LastAgreedPrunableBlockId();
  if (last_agreed.has_value()) {
    PruneThroughBlockId(*last_agreed);
  }
}

void KVBPruningSM::PruneOnStateTransferCompletion(uint64_t) const noexcept {
  try {
    PruneThroughLastAgreedBlockId();
  } catch (const std::exception& e) {
    LOG_FATAL(logger_,
              "KVBPruningSM stopping replica due to failure to prune blocks on "
              "state transfer completion, reason: "
                  << e.what());
    std::exit(-1);
  } catch (...) {
    LOG_FATAL(logger_,
              "KVBPruningSM stopping replica due to failure to prune blocks on "
              "state transfer completion");
    std::exit(-1);
  }
}

}  // namespace pruning
}  // namespace reconfiguration
}  // namespace concord
