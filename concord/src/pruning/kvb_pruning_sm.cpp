// Copyright 2019 VMware, all rights reserved

#include "kvb_pruning_sm.hpp"
#include "pruning_exception.hpp"

#include "config/configuration_manager.hpp"

#include <exception>

namespace concord {
namespace pruning {

KVBPruningSM::KVBPruningSM(
    const storage::blockchain::ILocalKeyValueStorageReadOnly& ro_storage,
    const config::ConcordConfiguration& config,
    const config::ConcordConfiguration& node_config)
    : logger_{log4cplus::Logger::getInstance("concord.pruning")},
      signer_{node_config},
      verifier_{config},
      ro_storage_{ro_storage},
      replica_id_{node_config.subscope("replica", 0)
                      .getValue<std::uint64_t>("principal_id")} {
  if (config.hasValue<std::uint64_t>("pruning_num_blocks_to_keep")) {
    num_blocks_to_keep_ =
        config.getValue<std::uint64_t>("pruning_num_blocks_to_keep");
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

  auto block_id = ro_storage_.getLastBlock();
  if (block_id < num_blocks_to_keep_) {
    block_id = 0;
  } else {
    block_id -= num_blocks_to_keep_;
  }
  block->set_block_id(block_id);

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

  const auto sender =
      request.has_sender() ? request.sender() : decltype(request.sender()){0};

  if (!verifier_.Verify(request)) {
    LOG4CPLUS_WARN(
        logger_, "KVBPruningSM failed to verify PruneRequest from principal_id "
                     << sender);
    return;
  }

  // TODO: Execute actual pruning.

  auto response = concord_response.mutable_prune_response();
  response->set_ok(true);
}

}  // namespace pruning
}  // namespace concord
