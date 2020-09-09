// Copyright 2019 VMware, all rights reserved
//
// Shim between generic KVB and Concord-specific commands handlers.

#include "concord_commands_handler.hpp"
#include "OpenTracing.hpp"
#include "reconfiguration/upgrade_plugin.hpp"
#include "reconfiguration/wedge_plugin.hpp"
#include "thin_replica/subscription_buffer.hpp"
#include "time/time_contract.hpp"

#include <opentracing/noop.h>
#include <opentracing/tracer.h>
#include <prometheus/counter.h>
#include <chrono>
#include <string>
#include <utils/open_tracing_utils.hpp>
#include <vector>

using com::vmware::concord::ErrorResponse;
using com::vmware::concord::TimeRequest;
using com::vmware::concord::TimeResponse;
using com::vmware::concord::TimeSample;
using concord::kvbc::BlockId;
using concord::kvbc::SetOfKeyValuePairs;
using concord::kvbc::Value;
using concord::thin_replica::SubUpdate;
using concord::thin_replica::SubUpdateBuffer;
using concordUtils::Sliver;

using google::protobuf::Timestamp;

namespace concord {
namespace consensus {

// Resets the TimeContract on construction and destruction.
class TimeContractResetter {
 public:
  TimeContractResetter(time::TimeContract *time_contract) noexcept
      : time_contract_{time_contract} {
    Reset();
  }

  ~TimeContractResetter() noexcept { Reset(); }

  TimeContractResetter(const TimeContractResetter &) = delete;
  TimeContractResetter &operator=(const TimeContractResetter &) = delete;

 private:
  void Reset() noexcept {
    if (time_contract_) time_contract_->Reset();
  }

  time::TimeContract *time_contract_;
};

ConcordCommandsHandler::ConcordCommandsHandler(
    const concord::config::ConcordConfiguration &config,
    const concord::config::ConcordConfiguration &node_config,
    const concord::kvbc::ILocalKeyValueStorageReadOnly &storage,
    concord::kvbc::IBlocksAppender &appender,
    concord::kvbc::IBlocksDeleter &deleter,
    bftEngine::IStateTransfer &state_transfer,
    concord::thin_replica::SubBufferList &subscriber_list,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry,
    concord::time::TimeContract *time_contract)
    : logger_(logging::getLogger("concord.consensus.ConcordCommandsHandler")),
      executing_bft_sequence_num_(0),
      subscriber_list_(subscriber_list),
      concord_control_handlers_(
          std::make_shared<reconfiguration::ConcordControlHandler>()),
      storage_(storage),
      metadata_storage_(storage),
      command_handler_counters_{prometheus_registry->createCounterFamily(
          "concord_command_handler_operation_counters_total",
          "counts how many operations the command handler has done", {})},
      command_handler_histograms_{prometheus_registry->createHistogramFamily(
          "concord_command_handler_histograms",
          "measure quantities such as durations", {})},
      command_handler_summaries_{prometheus_registry->createSummaryFamily(
          "concord_command_handler_summaries",
          "summarizes quantities such as key-values size", {})},
      written_blocks_{prometheus_registry->createCounter(
          command_handler_counters_, {{"layer", "ConcordCommandsHandler"},
                                      {"operation", "written_blocks"}})},
      internal_kv_size_summary_{prometheus_registry->createSummary(
          command_handler_summaries_,
          {{"item", "internal_kv_size"}, {"layer", "ConcordCommandsHandler"}},
          {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_duration_ms_summary_{prometheus_registry->createSummary(
          command_handler_summaries_,
          {{"item", "pre_processing_execution_duration"},
           {"layer", "ConcordCommandsHandler"}},
          {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_read_keys_size_summary_{prometheus_registry->createSummary(
          command_handler_summaries_,
          {{"item", "pre_processing_read_keys_size_summary"},
           {"layer", "ConcordCommandsHandler"}},
          {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_read_values_size_summary_{
          prometheus_registry->createSummary(
              command_handler_summaries_,
              {{"item", "pre_processing_read_values_size_summary"},
               {"layer", "ConcordCommandsHandler"}},
              {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_write_keys_size_summary_{prometheus_registry->createSummary(
          command_handler_summaries_,
          {{"item", "pre_processing_write_keys_size_summary"},
           {"layer", "ConcordCommandsHandler"}},
          {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_write_values_size_summary_{
          prometheus_registry->createSummary(
              command_handler_summaries_,
              {{"item", "pre_processing_write_values_size_summary"},
               {"layer", "ConcordCommandsHandler"}},
              {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_read_kv_set_size_summary_{
          prometheus_registry->createSummary(
              command_handler_summaries_,
              {{"item", "pre_processing_read_kv_set_size_summary"},
               {"layer", "ConcordCommandsHandler"}},
              {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      pre_execution_write_kv_set_size_summary_{
          prometheus_registry->createSummary(
              command_handler_summaries_,
              {{"item", "pre_processing_write_kv_set_size_summary"},
               {"layer", "ConcordCommandsHandler"}},
              {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
      appender_(appender) {
  if (concord::time::IsTimeServiceEnabled(config)) {
    if (time_contract) {
      time_ = std::unique_ptr<concord::time::TimeContract>(time_contract);
    } else {
      time_ = std::make_unique<concord::time::TimeContract>(storage_, config);
    }
  }

  auto &replicaConfig = node_config.subscope("replica", 0);
  replica_id_ = replicaConfig.getValue<uint16_t>("principal_id");
  enable_histograms_or_summaries =
      node_config.hasValue<bool>("enable_histograms_or_summaries")
          ? node_config.getValue<bool>("enable_histograms_or_summaries")
          : true;

  pruning_sm_ = std::make_unique<concord::pruning::KVBPruningSM>(
      storage, appender, deleter, state_transfer, config, node_config,
      time_.get());
  reconfiguration_sm_ =
      std::make_unique<concord::reconfiguration::ReconfigurationSM>(
          config, prometheus_registry);
  reconfiguration_sm_->LoadAllPlugins();
  reconfiguration_sm_->setControlHandlers(concord_control_handlers_);
}

uint32_t ConcordCommandsHandler::SerializeResponse(
    const opentracing::Span &parent_span, uint32_t max_response_size,
    com::vmware::concord::ConcordResponse &response,
    com::vmware::concord::ConcordReplicaSpecificInfoResponse &rsi_response,
    uint32_t &rsi_length, char *response_buffer) const {
  auto tracer = opentracing::Tracer::Global();
  std::unique_ptr<opentracing::Span> serialize_span =
      tracer->StartSpan("serialize_time_response",
                        {opentracing::ChildOf(&parent_span.context())});

  uint32_t response_size = 0;
  if (response.ByteSizeLong() == 0) {
    LOG_ERROR(logger_, "Request produced empty response.");
    ErrorResponse *err = response.add_error_response();
    err->set_description("Request produced empty response.");
  }

  if (response.SerializeToArray(response_buffer, max_response_size)) {
    response_size = response.GetCachedSize();
  } else {
    size_t response_size = response.ByteSizeLong();

    LOG_ERROR(logger_,
              "Cannot send response to a client request: Response is too large "
              "(size of this response: " +
                  std::to_string(response_size) +
                  ", maximum size allowed for this response: " +
                  std::to_string(max_response_size) + ").");

    response.Clear();
    ErrorResponse *err = response.add_error_response();
    err->set_description(
        "Concord could not send response: Response is too large (size of this "
        "response: " +
        std::to_string(response_size) +
        ", maximum size allowed for this response: " +
        std::to_string(max_response_size) + ").");

    if (response.SerializeToArray(response_buffer, max_response_size)) {
      response_size = response.GetCachedSize();
    } else {
      // This case should never occur; we intend to enforce a minimum buffer
      // size for the communication buffer size that Concord-BFT is configured
      // with, and this minimum should be significantly higher than the size of
      // this error messsage.
      LOG_FATAL(
          logger_,
          "Cannot send error response indicating response is too large: The "
          "error response itself is too large (error response size: " +
              std::to_string(response.ByteSizeLong()) +
              ", maximum size allowed for this response: " +
              std::to_string(max_response_size) + ").");

      // This will cause the replica to halt.
      response_size = 0;
    }
  }
  if (rsi_response.SerializeToArray(
          response_buffer + response_size,
          (int32_t)(max_response_size - response_size))) {
    rsi_length = rsi_response.GetCachedSize();
  } else {
    LOG_FATAL(logger_,
              "cannot attach replica specific information to message because "
              "its size is "
              "too large"
                  << KVLOG(max_response_size, response_size,
                           rsi_response.ByteSizeLong()));
    rsi_length = 0;
    // This will cause the replica to halt.
    response_size = 0;
  }
  return response_size + rsi_length;
}

void ConcordCommandsHandler::UpdateTime(
    const opentracing::Span &parent_span,
    const com::vmware::concord::ConcordRequest &request, bool read_only,
    uint16_t client_id) {
  if (time_ && request.has_time_request() &&
      request.time_request().has_sample()) {
    if (!read_only) {
      auto tracer = opentracing::Tracer::Global();
      auto time_update_span = tracer->StartSpan(
          "time_update", {opentracing::ChildOf(&parent_span.context())});
      const TimeRequest &tr = request.time_request();
      const TimeSample &ts = tr.sample();
      if (!(time_->SignaturesEnabled()) && ts.has_source() && ts.has_time()) {
        time_->Update(ts.source(), client_id, ts.time());
      } else if (ts.has_source() && ts.has_time() && ts.has_signature()) {
        std::vector<uint8_t> signature(ts.signature().begin(),
                                       ts.signature().end());
        time_->Update(ts.source(), client_id, ts.time(), &signature);
      } else {
        LOG_WARN(
            logger_,
            "Time Sample is missing:"
                << " [" << (ts.has_source() ? " " : "X") << "] source"
                << " [" << (ts.has_time() ? " " : "X") << "] time"
                << (time_->SignaturesEnabled()
                        ? (string(" [") + (ts.has_signature() ? " " : "X") +
                           "] signature")
                        : ""));
      }
    } else {
      LOG_INFO(logger_, "Ignoring time sample sent in read-only command");
    }
  }
}

void ConcordCommandsHandler::UpdateResponseWithTime(
    const opentracing::Span &parent_span,
    const com::vmware::concord::TimeRequest &time_request,
    com::vmware::concord::ConcordResponse &response) {
  auto tracer = opentracing::Tracer::Global();
  auto time_response_span = tracer->StartSpan(
      "time_response", {opentracing::ChildOf(&parent_span.context())});

  if (time_request.return_summary()) {
    TimeResponse *tp = response.mutable_time_response();
    Timestamp *sum = new Timestamp(time_->GetTime());
    tp->set_allocated_summary(sum);
  }

  if (time_request.return_samples()) {
    TimeResponse *tp = response.mutable_time_response();

    for (auto &s : time_->GetSamples()) {
      TimeSample *ts = tp->add_sample();
      ts->set_source(s.first);
      Timestamp *t = new Timestamp(s.second.time);
      ts->set_allocated_time(t);
      if (s.second.signature) {
        ts->set_signature(s.second.signature->data(),
                          s.second.signature->size());
      }
    }
  }
}

void ConcordCommandsHandler::AddTimeUpdateBlock(
    const opentracing::Span &parent_span,
    com::vmware::concord::ConcordResponse &response, int execute_result,
    bool read_only) {
  if (!time_->Changed()) {
    return;
  }
  // We had a sample that updated the time contract, and the execution of
  // the rest of the command did not write its state. What should we do?
  if (execute_result) {
    if (!read_only) {
      // The state machine might have had no commands in the request. Go
      // ahead and store just the time update.
      WriteEmptyBlock(time_.get(), parent_span);

      // Create an empty time response, so that out_response_size is not
      // zero.
      response.mutable_time_response();
    } else {
      // If this happens, there is a bug above. Either the logic ignoring
      // the update in this function is broken, or the subclass's Execute
      // function modified timeContract_. Log an error for us to deal
      // with, but otherwise ignore.
      LOG_ERROR(logger_,
                "Time Contract was modified during read-only operation");

      ErrorResponse *err = response.add_error_response();
      err->set_description("Ignoring time update during read-only operation");

      // Also reset the time contract now, so that the modification is not
      // accidentally written during the next command.
      time_->Reset();
    }
  } else {
    LOG_WARN(logger_, "Ignoring time update because Execute failed.");

    ErrorResponse *err = response.add_error_response();
    err->set_description(
        "Ignoring time update because state machine execution failed");
  }
}

int ConcordCommandsHandler::execute(uint16_t client_id, uint64_t sequence_num,
                                    uint8_t flags, uint32_t request_size,
                                    const char *request_buffer,
                                    uint32_t max_response_size,
                                    char *response_buffer,
                                    uint32_t &out_response_size,
                                    uint32_t &out_replica_specific_info_size,
                                    concordUtils::SpanWrapper &parent_span) {
  LOG_DEBUG(logger_, "ConcordCommandsHandler::execute, clientId: "
                         << client_id << ", seq: " << sequence_num);

  bool read_only = flags & bftEngine::MsgFlag::READ_ONLY_FLAG;
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
  assert(!(pre_execute && has_pre_executed));

  if (!pre_execute) {
    // This field is only used when creating blocks (during execution or
    // post-execution, both of which are sequential). In pre-execution,
    // `executing_bft_sequence_num_` is not used at all, so we may as well not
    // update it, in order to avoid concurrent reads/writes.
    executing_bft_sequence_num_ = sequence_num;

    // Make sure there is no leftover state in the TimeContract across execute()
    // calls by resetting the TimeContract now and when the execute() scope is
    // left.
    auto time_contract_resetter = TimeContractResetter{time_.get()};
  }

  com::vmware::concord::ConcordRequest request;
  com::vmware::concord::ConcordResponse response;
  com::vmware::concord::ConcordReplicaSpecificInfoResponse rsi_response;
  std::shared_ptr<opentracing::Tracer> tracer;
  std::unique_ptr<opentracing::Span> execute_span;
  if (parent_span) {
    const auto &ctx = parent_span.impl()->context();
    tracer = opentracing::Tracer::Global();
    execute_span = tracer->StartSpan("execute", {opentracing::ChildOf(&ctx)});
  } else {
    // If BFT client request does not have a span,
    // use Noop Tracer to avoid polluting dashboard with partial traces
    tracer = opentracing::MakeNoopTracer();
    execute_span = tracer->StartSpan("execute");
  }

  const ConcordRequestContext request_context{client_id, sequence_num,
                                              max_response_size};

  bool result = false;
  if ((request.ParseFromArray(request_buffer, request_size)) ||
      (has_pre_executed &&
       parseFromPreExecutionResponse(request_buffer, request_size, request))) {
    // For now, pre-execution is not supported for time requests,
    // due to the stateful nature of the TimeContract
    assert(!(pre_execute && request.has_time_request()));

    execute_span->SetTag(concord::utils::kRequestSeqNumTag, sequence_num);
    execute_span->SetTag(concord::utils::kClientIdTag, client_id);
    execute_span->SetTag(concord::utils::kReplicaIdTag, replica_id_);
    execute_span->SetTag(concord::utils::kRequestSizeTag, request_size);

    UpdateTime(*execute_span, request, read_only, client_id);
    std::chrono::steady_clock::time_point execute_transaction_start_time;
    if (enable_histograms_or_summaries && pre_execute && !has_pre_executed) {
      execute_transaction_start_time = std::chrono::steady_clock::now();
    }
    result = Execute(request, request_context, flags, time_.get(),
                     *execute_span, response);
    if (enable_histograms_or_summaries && pre_execute && !has_pre_executed) {
      auto pre_time = std::chrono::duration_cast<std::chrono::milliseconds>(
          std::chrono::steady_clock::now() - execute_transaction_start_time);
      pre_execution_duration_ms_summary_.Observe(pre_time.count());
    }

    if (time_ && request.has_time_request()) {
      AddTimeUpdateBlock(*execute_span, response, result, read_only);
      UpdateResponseWithTime(*execute_span, request.time_request(), response);
    } else if (!time_ && request.has_time_request()) {
      ErrorResponse *err = response.add_error_response();
      err->set_description("Time service is disabled.");
    }

    if (request.has_prune_request() ||
        request.has_latest_prunable_block_request()) {
      pruning_sm_->Handle(request, response, read_only, *execute_span);
    }
    if (request.has_reconfiguration_sm_request()) {
      result = reconfiguration_sm_->Handle(request.reconfiguration_sm_request(),
                                           response, sequence_num, read_only,
                                           rsi_response, *execute_span);
    }
  } else {
    ErrorResponse *err = response.add_error_response();
    err->set_description("Unable to parse concord request");

    // "true" means "resending this request is unlikely to change the outcome"
    result = true;
  }

  out_response_size = SerializeResponse(
      *execute_span, max_response_size, response, rsi_response,
      out_replica_specific_info_size, response_buffer);

  execute_span->SetTag(concord::utils::kExecResultTag, result);
  execute_span->SetTag(concord::utils::kExecRespSizeTag, out_response_size);

  LOG_DEBUG(logger_, "ConcordCommandsHandler::execute done, clientId: "
                         << client_id << ", seq: " << sequence_num);
  return result ? 0 : 1;
}  // namespace consensus

bool ConcordCommandsHandler::HasPreExecutionConflicts(
    const com::vmware::concord::ReadSet &read_set) const {
  const auto last_block_id = storage_.getLastBlock();
  for (const auto &kf : read_set.keys_with_fingerprints()) {
    const Sliver key{std::string{kf.key()}};
    const BlockId read_block_height = DeserializeFingerprint(kf.fingerprint());
    BlockId current_block_height;
    Value out;
    if (!storage_.get(last_block_id, key, out, current_block_height).isOK()) {
      std::stringstream msg;
      msg << "Key " << key << " is not available in storage.";
      throw std::runtime_error(msg.str());
    }
    if (enable_histograms_or_summaries) {
      pre_execution_read_keys_size_summary_.Observe(kf.key().length());
      pre_execution_read_values_size_summary_.Observe(out.length());
    }
    if (current_block_height != read_block_height) {
      return true;
    }
  }
  if (enable_histograms_or_summaries) {
    pre_execution_read_kv_set_size_summary_.Observe(
        read_set.keys_with_fingerprints_size());
  }
  return false;
}

bool ConcordCommandsHandler::parseFromPreExecutionResponse(
    const char *request_buffer, uint32_t request_size,
    com::vmware::concord::ConcordRequest &request) {
  // transform the ConcordResponse produced by pre-execution into a
  // ConcordRequest for seamless integration into the rest of the execution
  // flow
  com::vmware::concord::ConcordResponse pre_execution_response;
  if (pre_execution_response.ParseFromArray(request_buffer, request_size) &&
      pre_execution_response.has_pre_execution_result()) {
    auto *pre_execution_result = request.mutable_pre_execution_result();
    pre_execution_result->MergeFrom(
        pre_execution_response.pre_execution_result());
    return true;
  } else {
    return false;
  }
}

std::string ConcordCommandsHandler::SerializeFingerprint(
    const BlockId fingerprint) {
  char buf[sizeof(BlockId)];
  memcpy(buf, &fingerprint, sizeof(BlockId));
  std::string serialized(buf, sizeof(BlockId));
  return serialized;
}

BlockId ConcordCommandsHandler::DeserializeFingerprint(
    const std::string &serialized) {
  BlockId fingerprint;
  memcpy(&fingerprint, serialized.data(), sizeof(BlockId));
  return fingerprint;
}

concordUtils::Status ConcordCommandsHandler::addBlock(
    const concord::storage::SetOfKeyValuePairs &updates,
    concord::kvbc::BlockId &out_block_id,
    const concordUtils::SpanWrapper &parent_span) {
  auto span = concordUtils::startChildSpan("add_block", parent_span);

  // The IBlocksAppender interface specifies that updates must be const, but we
  // need to add items here, so we have to make a copy and work with that. In
  // the future, maybe we can figure out how to either make updates non-const,
  // or allow addBlock to take a list of const sets.
  LOG_DEBUG(logger_, "ConcordCommandsHandler::addBlock, enter, updates: "
                         << updates.size());
  SetOfKeyValuePairs amended_updates(updates);
  LOG_TRACE(logger_, "ConcordCommandsHandler::addBlock, copied, updates: "
                         << updates.size());

  if (time_) {
    LOG_TRACE(logger_, "ConcordCommandsHandler::addBlock, time enter, updates: "
                           << updates.size());
    if (time_->Changed()) {
      LOG_TRACE(logger_,
                "ConcordCommandsHandler::addBlock, time changed1, updates: "
                    << updates.size());
      auto time_data = time_->Serialize();
      amended_updates.insert(time_data);
      internal_kv_size_summary_.Observe(time_data.second.length());
      LOG_TRACE(logger_,
                "ConcordCommandsHandler::addBlock, time changed2, updates: "
                    << updates.size());
    }
    amended_updates.insert(time_->SerializeSummarizedTime());
  }

  amended_updates[metadata_storage_.getKey()] =
      metadata_storage_.serialize(executing_bft_sequence_num_);

  LOG_DEBUG(logger_,
            "ConcordCommandsHandler::addBlock, before add block, updates: "
                << updates.size());
  concordUtils::Status status =
      appender_.addBlock(amended_updates, out_block_id, span);
  if (!status.isOK()) {
    return status;
  }
  LOG_DEBUG(logger_,
            "ConcordCommandsHandler::addBlock, after add block, updates: "
                << updates.size());
  written_blocks_.Increment();

  auto start = std::chrono::steady_clock::now();
  PublishUpdatesToThinReplicaServer(out_block_id, amended_updates);
  auto end = std::chrono::steady_clock::now();
  auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
  LOG_INFO(logger_,
           "ConcordCommandsHandler::addBlock, exit, updates: "
               << updates.size() << ", dur: " << dur.count() << ", clock: "
               << std::chrono::steady_clock::now().time_since_epoch().count());
  return status;
}

void ConcordCommandsHandler::PublishUpdatesToThinReplicaServer(
    BlockId block_id, SetOfKeyValuePairs &updates) {
  // DD: Since Thin Replica mechanism is supposed to be replaced with the
  // Read-Only Replica, we don't want to extend ICommandsHandler interface with
  // spans. Thus, we transmit span context here
  std::string cid;
  auto iter = updates.find(cid_key_);
  if (iter != updates.end()) {
    cid = iter->second.toString();
  }
  auto block_read_span = opentracing::Tracer::Global()->StartSpan(
      "send_updates_to_thin_replica_server",
      {opentracing::SetTag{concord::utils::kCorrelationIdTag, cid}});
  utils::InjectSpan(block_read_span, updates);
  subscriber_list_.UpdateSubBuffers({block_id, updates});
}
void ConcordCommandsHandler::setControlStateManager(
    std::shared_ptr<bftEngine::ControlStateManager> controlStateManager) {
  controlStateManager_ = controlStateManager;
  reconfiguration_sm_->setControlStateManager(controlStateManager_);
}

std::shared_ptr<bftEngine::ControlHandlers>
ConcordCommandsHandler::getControlHandlers() {
  return concord_control_handlers_;
}

}  // namespace consensus
}  // namespace concord
