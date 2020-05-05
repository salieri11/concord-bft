// Copyright 2019-2020 VMware, all rights reserved

#include "kvb_pruning_sm.hpp"

#include "blockchain_view.h"
#include "config/configuration_manager.hpp"

#include <google/protobuf/timestamp.pb.h>
#include <google/protobuf/util/time_util.h>

#include <algorithm>
#include <exception>
#include <iterator>

namespace concord {

using com::vmware::concord::LatestPrunableBlock;
using com::vmware::concord::PruneRequest;
using concord::time::TimeContract;
using concord::utils::openssl_crypto::DeserializePublicKey;
using kvbc::BaseBlockInfo;
using kvbc::BlockchainView;
using kvbc::BlockId;
using kvbc::ILocalKeyValueStorageReadOnly;

using google::protobuf::Timestamp;
using google::protobuf::util::TimeUtil;
using std::invalid_argument;
using std::stringstream;

namespace pruning {

KVBPruningSM::KVBPruningSM(const ILocalKeyValueStorageReadOnly& ro_storage,
                           const config::ConcordConfiguration& config,
                           const config::ConcordConfiguration& node_config,
                           TimeContract* time_contract)
    : logger_{log4cplus::Logger::getInstance("concord.pruning")},
      signer_{node_config},
      verifier_{config},
      ro_storage_{ro_storage},
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

  if (pruning_enabled_) {
    if (!config.hasValue<string>("pruning_operator_public_key")) {
      throw invalid_argument(
          "Cannot initialize pruning state machine: pruning has been enabled, "
          "but no value has been provided for pruning_operator_public_key; a "
          "public key for the operator is required if pruning is enabled.");
    }

    // Note DeserializePublicKey should handle throwing an exception in the
    // event the operator public key is malformed or otherwise cannot be parsed.
    operator_public_key_ = DeserializePublicKey(
        config.getValue<string>("pruning_operator_public_key"));
  }
}

void KVBPruningSM::Handle(const com::vmware::concord::ConcordRequest& request,
                          com::vmware::concord::ConcordResponse& response,
                          bool read_only,
                          opentracing::Span& parent_span) const {
  try {
    com::vmware::concord::ConcordResponse internal_response;

    if (request.has_latest_prunable_block_request()) {
      Handle(request.latest_prunable_block_request(), internal_response,
             parent_span);
    }

    if (request.has_prune_request()) {
      Handle(request.prune_request(), internal_response, read_only,
             parent_span);
    }

    response.CopyFrom(internal_response);
  } catch (const std::exception& e) {
    response.add_error_response()->set_description(e.what());
    LOG4CPLUS_ERROR(
        logger_, "KVBPruningSM encountered an exception: [" << e.what() << ']');
  } catch (...) {
    response.add_error_response()->set_description(
        "KVBPruningSM encountered an unknown exception");
    LOG4CPLUS_ERROR(logger_, "KVBPruningSM encountered an unknown exception");
  }
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

string KVBPruningSM::GetSignablePruneCommandData(
    const PruneRequest& prune_request) {
  // Note it is structurally possible for there to exist multiple different
  // PruneRequest message objects that yield identical signable byte strings in
  // the event one of those prune requests is missing expected fields or has
  // values of unexpected length for a variable-length field; however, we
  // currently believe it is not necessary to, say, add boolean "value present"
  // or integer "value length" fields to the signable data we prepare here, as
  // we believe PruneRequest messages missing expected fields or having
  // variable-length fields of unexpected length will fail validation elsewhere.

  stringstream signable_data("");

  uint64_t sender = prune_request.sender();
  UInt64BytesToStreamLittleEndian(&sender, signable_data);

  for (const LatestPrunableBlock& latest_prunable_block_message :
       prune_request.latest_prunable_block()) {
    uint64_t replica = latest_prunable_block_message.replica();
    UInt64BytesToStreamLittleEndian(&replica, signable_data);

    uint64_t block_id = latest_prunable_block_message.block_id();
    UInt64BytesToStreamLittleEndian(&block_id, signable_data);

    const string& replica_signature = latest_prunable_block_message.signature();
    signable_data.write(replica_signature.c_str(), replica_signature.length());
  }

  return signable_data.str();
}

void KVBPruningSM::Handle(
    const com::vmware::concord::LatestPrunableBlockRequest& request,
    com::vmware::concord::ConcordResponse& concord_response,
    opentracing::Span& parent_span) const {
  auto latest_prunable_block_span = opentracing::Tracer::Global()->StartSpan(
      "latest_prunable_block_request",
      {opentracing::ChildOf(&parent_span.context())});

  auto response = concord_response.mutable_latest_prunable_block_response();
  auto block_list = response->mutable_block();
  auto block = block_list->Add();
  block->set_replica(replica_id_);

  // If pruning is disabled, return 0. Otherwise, be conservative and prune the
  // smaller block range.
  const auto latest_prunable_block_id =
      pruning_enabled_
          ? std::min(LatestBasedOnNumBlocks(), LatestBasedOnTimeRange())
          : 0;
  block->set_block_id(latest_prunable_block_id);
  signer_.Sign(*block);
}

void KVBPruningSM::Handle(
    const com::vmware::concord::PruneRequest& request,
    com::vmware::concord::ConcordResponse& concord_response, bool read_only,
    opentracing::Span& parent_span) const {
  auto prune_span = opentracing::Tracer::Global()->StartSpan(
      "prune_request", {opentracing::ChildOf(&parent_span.context())});

  if (read_only) {
    LOG4CPLUS_WARN(logger_,
                   "KVBPruningSM ignoring PruneRequest in a read-only command");
    return;
  }

  if (!pruning_enabled_) {
    const auto msg =
        "KVBPruningSM pruning is disabled, returning an error on PruneRequest";
    LOG4CPLUS_WARN(logger_, msg);
    concord_response.add_error_response()->set_description(msg);
    return;
  }

  const auto sender =
      request.has_sender() ? request.sender() : decltype(request.sender()){0};

  if (!request.has_signature() ||
      !operator_public_key_->Verify(GetSignablePruneCommandData(request),
                                    request.signature())) {
    LOG4CPLUS_WARN(
        logger_, "KVBPruingSM failed to verify PruneRequest from principal_id "
                     << sender
                     << " on the grounds that it did not have a verifiable "
                        "legitimate signature from an/the operator authorized "
                        "to issue a pruning command.");
    return;
  }

  if (!verifier_.Verify(request)) {
    LOG4CPLUS_WARN(
        logger_,
        "KVBPruningSM failed to verify PruneRequest from principal_id "
            << sender
            << " on the grounds that the pruning request did not include "
               "LatestPrunableBlock responses from the required replicas, or "
               "on the grounds that some non-empty subset of those "
               "LatestPrunableBlock messages did not bear correct signatures "
               "from the claimed replicas.");
    return;
  }

  // TODO: Execute actual pruning.

  auto response = concord_response.mutable_prune_response();
  response->set_ok(true);
}

BlockId KVBPruningSM::LatestBasedOnNumBlocks() const {
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

BlockId KVBPruningSM::LatestBasedOnTimeRange() const {
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

  // TODO: Handle dynamic genesis block IDs.
  const auto view = BlockchainView<TimestampedBlockInfo, TimeContract*>{
      1, last_block_id, time_contract_};
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

}  // namespace pruning
}  // namespace concord
