// Copyright 2019 VMware, all rights reserved
//
// Shim between generic KVB and Concord-specific commands handlers.

#include "concord_commands_handler.hpp"
#include "thin_replica/subscription_buffer.hpp"
#include "time/time_contract.hpp"

#include <opentracing/tracer.h>
#include <prometheus/counter.h>
#include <chrono>
#include <utils/open_tracing_utils.hpp>
#include <vector>

using com::vmware::concord::ErrorResponse;
using com::vmware::concord::TimeRequest;
using com::vmware::concord::TimeResponse;
using com::vmware::concord::TimeSample;
using concord::kvbc::BlockId;
using concord::kvbc::SetOfKeyValuePairs;
using concord::thin_replica::SubUpdate;
using concord::thin_replica::SubUpdateBuffer;
using concordUtils::Sliver;

using google::protobuf::Timestamp;

namespace concord {
namespace consensus {

ConcordCommandsHandler::ConcordCommandsHandler(
    const concord::config::ConcordConfiguration &config,
    const concord::config::ConcordConfiguration &node_config,
    const concord::kvbc::ILocalKeyValueStorageReadOnly &storage,
    concord::kvbc::IBlocksAppender &appender,
    concord::thin_replica::SubBufferList &subscriber_list,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry,
    concord::time::TimeContract *time_contract)
    : logger_(log4cplus::Logger::getInstance(
          "concord.consensus.ConcordCommandsHandler")),
      subscriber_list_(subscriber_list),
      storage_(storage),
      metadata_storage_(storage),
      command_handler_counters_{prometheus_registry->createCounterFamily(
          "concord_command_handler_operation_counters_total",
          "counts how many operations the command handler has done", {})},
      command_handler_histograms_{prometheus_registry->createHistogramFamily(
          "concord_command_handler_histograms",
          "measure quantities such as durations", {})},
      written_blocks_{prometheus_registry->createCounter(
          command_handler_counters_, {{"layer", "ConcordCommandsHandler"},
                                      {"operation", "written_blocks"}})},
      appender_(appender) {
  if (concord::time::IsTimeServiceEnabled(config)) {
    if (time_contract) {
      time_ = std::unique_ptr<concord::time::TimeContract>(time_contract);
    } else {
      time_ = std::unique_ptr<concord::time::TimeContract>(
          new concord::time::TimeContract(storage_, config));
    }
  }

  auto &replicaConfig = node_config.subscope("replica", 0);
  replica_id_ = replicaConfig.getValue<uint16_t>("principal_id");

  pruning_sm_ = std::make_unique<concord::pruning::KVBPruningSM>(
      storage, config, node_config, time_.get());
}

int ConcordCommandsHandler::execute(uint16_t client_id, uint64_t sequence_num,
                                    uint8_t flags, uint32_t request_size,
                                    const char *request_buffer,
                                    uint32_t max_response_size,
                                    char *response_buffer,
                                    uint32_t &out_response_size) {
  executing_bft_sequence_num_ = sequence_num;
  LOG4CPLUS_DEBUG(logger_, "ConcordCommandsHandler::execute, clientId: "
                               << client_id << ", seq: " << sequence_num);

  bool read_only = flags & bftEngine::MsgFlag::READ_ONLY_FLAG;
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
  assert(!(pre_execute && has_pre_executed));

  request_.Clear();
  response_.Clear();
  request_context_.reset(nullptr);

  auto tracer = opentracing::Tracer::Global();

  std::unique_ptr<opentracing::Span> execute_span;

  bool result;
  if ((!has_pre_executed &&
       request_.ParseFromArray(request_buffer, request_size)) ||
      (has_pre_executed &&
       parseFromPreExecutionResponse(request_buffer, request_size, request_))) {
    request_context_ = std::make_unique<ConcordRequestContext>();
    request_context_->client_id = client_id;
    request_context_->sequence_num = sequence_num;
    request_context_->max_response_size = max_response_size;

    if (request_.has_trace_context()) {
      std::istringstream tc_stream(request_.trace_context());
      auto trace_context = tracer->Extract(tc_stream);
      if (trace_context.has_value()) {
        execute_span = tracer->StartSpan(
            "execute", {opentracing::ChildOf(trace_context.value().get())});
      } else {
        LOG4CPLUS_WARN(logger_, "Command has corrupted trace context");
        execute_span = tracer->StartSpan("execute");
      }
    } else {
      LOG4CPLUS_DEBUG(logger_, "Command is missing trace context");
      execute_span = tracer->StartSpan("execute");
    }

    if (time_ && request_.has_time_request() &&
        request_.time_request().has_sample()) {
      if (!read_only) {
        auto time_update_span = tracer->StartSpan(
            "time_update", {opentracing::ChildOf(&execute_span->context())});
        TimeRequest tr = request_.time_request();
        TimeSample ts = tr.sample();
        if (!(time_->SignaturesEnabled()) && ts.has_source() && ts.has_time()) {
          time_->Update(ts.source(), client_id, ts.time());
        } else if (ts.has_source() && ts.has_time() && ts.has_signature()) {
          std::vector<uint8_t> signature(ts.signature().begin(),
                                         ts.signature().end());
          time_->Update(ts.source(), client_id, ts.time(), &signature);
        } else {
          LOG4CPLUS_WARN(
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
        LOG4CPLUS_INFO(logger_,
                       "Ignoring time sample sent in read-only command");
      }
    }

    // Stashing this span in our state, so that if the subclass calls addBlock,
    // we can use it as the parent for the add_block span.
    execute_span->SetTag(concord::utils::kRequestSeqNumTag, sequence_num);
    execute_span->SetTag(concord::utils::kClientIdTag, client_id);
    execute_span->SetTag(concord::utils::kRequestSizeTag, request_size);
    execute_span->SetTag(concord::utils::kReplicaIdTag, replica_id_);

    addBlock_parent_span = tracer->StartSpan(
        "sub_execute", {opentracing::ChildOf(&execute_span->context())});
    result = Execute(request_, flags, time_.get(), *addBlock_parent_span.get(),
                     response_);
    // Manually stopping the span after execute.
    addBlock_parent_span.reset();

    if (time_ && request_.has_time_request()) {
      TimeRequest tr = request_.time_request();

      if (time_->Changed()) {
        // We had a sample that updated the time contract, and the execution of
        // the rest of the command did not write its state. What should we do?
        if (result) {
          if (!read_only) {
            // WriteEmptyBlock is going to call addBlock, and we need to tell it
            // what tracing span to use as its parent.
            addBlock_parent_span = std::move(execute_span);

            // The state machine might have had no commands in the request. Go
            // ahead and store just the time update.
            WriteEmptyBlock(time_.get());

            // Reclaim control of the addBlock_span.
            execute_span = std::move(addBlock_parent_span);

            // Create an empty time response, so that out_response_size is not
            // zero.
            response_.mutable_time_response();
          } else {
            // If this happens, there is a bug above. Either the logic ignoring
            // the update in this function is broken, or the subclass's Execute
            // function modified timeContract_. Log an error for us to deal
            // with, but otherwise ignore.
            LOG4CPLUS_ERROR(
                logger_,
                "Time Contract was modified during read-only operation");

            ErrorResponse *err = response_.add_error_response();
            err->set_description(
                "Ignoring time update during read-only operation");

            // Also reset the time contract now, so that the modification is not
            // accidentally written during the next command.
            time_->Reset();
          }
        } else {
          LOG4CPLUS_WARN(logger_,
                         "Ignoring time update because Execute failed.");

          ErrorResponse *err = response_.add_error_response();
          err->set_description(
              "Ignoring time update because state machine execution failed");
        }
      }

      {  // scope for time_response_span
        auto time_response_span = tracer->StartSpan(
            "time_response", {opentracing::ChildOf(&execute_span->context())});
        if (tr.return_summary()) {
          TimeResponse *tp = response_.mutable_time_response();
          Timestamp *sum = new Timestamp(time_->GetTime());
          tp->set_allocated_summary(sum);
        }

        if (tr.return_samples()) {
          TimeResponse *tp = response_.mutable_time_response();

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
    } else if (!time_ && request_.has_time_request()) {
      ErrorResponse *err = response_.add_error_response();
      err->set_description("Time service is disabled.");
    }

    if (request_.has_prune_request() ||
        request_.has_latest_prunable_block_request()) {
      pruning_sm_->Handle(request_, response_, read_only, *execute_span);
    }
  } else {
    ErrorResponse *err = response_.add_error_response();
    err->set_description("Unable to parse concord request");

    // "true" means "resending this request is unlikely to change the outcome"
    result = true;
  }

  // Don't bother timing serialization of the response if we didn't successfully
  // parse the request.
  std::unique_ptr<opentracing::Span> serialize_span =
      execute_span == nullptr
          ? nullptr
          : tracer->StartSpan("serialize",
                              {opentracing::ChildOf(&execute_span->context())});

  if (response_.ByteSizeLong() == 0) {
    LOG4CPLUS_ERROR(logger_, "Request produced empty response.");
    ErrorResponse *err = response_.add_error_response();
    err->set_description("Request produced empty response.");
  }

  if (response_.SerializeToArray(response_buffer, max_response_size)) {
    out_response_size = response_.GetCachedSize();
  } else {
    size_t response_size = response_.ByteSizeLong();

    LOG4CPLUS_ERROR(
        logger_,
        "Cannot send response to a client request: Response is too large "
        "(size of this response: " +
            std::to_string(response_size) +
            ", maximum size allowed for this response: " +
            std::to_string(max_response_size) + ").");

    response_.Clear();
    ErrorResponse *err = response_.add_error_response();
    err->set_description(
        "Concord could not send response: Response is too large (size of this "
        "response: " +
        std::to_string(response_size) +
        ", maximum size allowed for this response: " +
        std::to_string(max_response_size) + ").");

    if (response_.SerializeToArray(response_buffer, max_response_size)) {
      out_response_size = response_.GetCachedSize();
    } else {
      // This case should never occur; we intend to enforce a minimum buffer
      // size for the communication buffer size that Concord-BFT is configured
      // with, and this minimum should be significantly higher than the size of
      // this error messsage.
      LOG4CPLUS_FATAL(
          logger_,
          "Cannot send error response indicating response is too large: The "
          "error response itself is too large (error response size: " +
              std::to_string(response_.ByteSizeLong()) +
              ", maximum size allowed for this response: " +
              std::to_string(max_response_size) + ").");

      // This will cause the replica to halt.
      out_response_size = 0;
    }
  }

  execute_span->SetTag(concord::utils::kExecResultTag, result);
  execute_span->SetTag(concord::utils::kExecRespSizeTag, out_response_size);

  LOG4CPLUS_DEBUG(logger_, "ConcordCommandsHandler::execute done, clientId: "
                               << client_id << ", seq: " << sequence_num);
  return result ? 0 : 1;
}

bool ConcordCommandsHandler::HasPreExecutionConflicts(
    const com::vmware::concord::PreExecutionResult &pre_execution_result)
    const {
  const auto &read_set = pre_execution_result.read_set();
  const auto &write_set = pre_execution_result.write_set();

  const uint read_set_version = pre_execution_result.read_set_version();
  const BlockId current_block_id = storage_.getLastBlock();

  // pessimistically assume there is a conflict
  bool has_conflict = true;

  // check read set for conflicts
  for (const auto &k : read_set.keys()) {
    const Sliver key{std::string{k}};
    storage_.mayHaveConflictBetween(key, read_set_version + 1, current_block_id,
                                    has_conflict);
    if (has_conflict) {
      return true;
    }
  }

  // check write set for conflicts
  for (const auto &kv : write_set.kv_writes()) {
    const auto &k = kv.key();
    const Sliver key{std::string{k}};
    storage_.mayHaveConflictBetween(key, read_set_version + 1, current_block_id,
                                    has_conflict);
    if (has_conflict) {
      return true;
    }
  }

  // the read and write set are free of conflicts
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

concordUtils::Status ConcordCommandsHandler::addBlock(
    const concord::storage::SetOfKeyValuePairs &updates,
    concord::kvbc::BlockId &out_block_id) {
  auto add_block_span = addBlock_parent_span->tracer().StartSpan(
      "add_block", {opentracing::ChildOf(&addBlock_parent_span->context())});
  // The IBlocksAppender interface specifies that updates must be const, but we
  // need to add items here, so we have to make a copy and work with that. In
  // the future, maybe we can figure out how to either make updates non-const,
  // or allow addBlock to take a list of const sets.
  LOG4CPLUS_DEBUG(logger_, "ConcordCommandsHandler::addBlock, enter, updates: "
                               << updates.size());
  SetOfKeyValuePairs amended_updates(updates);
  LOG4CPLUS_TRACE(logger_, "ConcordCommandsHandler::addBlock, copied, updates: "
                               << updates.size());

  if (time_) {
    LOG4CPLUS_TRACE(logger_,
                    "ConcordCommandsHandler::addBlock, time enter, updates: "
                        << updates.size());
    if (time_->Changed()) {
      LOG4CPLUS_TRACE(
          logger_, "ConcordCommandsHandler::addBlock, time changed1, updates: "
                       << updates.size());
      amended_updates.insert(time_->Serialize());
      LOG4CPLUS_TRACE(
          logger_, "ConcordCommandsHandler::addBlock, time changed2, updates: "
                       << updates.size());
    }
    // amended_updates.insert(time_->SerializeSummarizedTime());
  }

  amended_updates[metadata_storage_.getKey()] =
      metadata_storage_.serialize(executing_bft_sequence_num_);

  LOG4CPLUS_DEBUG(
      logger_, "ConcordCommandsHandler::addBlock, before add block, updates: "
                   << updates.size());
  concordUtils::Status status =
      appender_.addBlock(amended_updates, out_block_id);
  if (!status.isOK()) {
    return status;
  }
  LOG4CPLUS_DEBUG(logger_,
                  "ConcordCommandsHandler::addBlock, after add block, updates: "
                      << updates.size());
  written_blocks_.Increment();

  auto start = std::chrono::steady_clock::now();
  PublishUpdatesToThinReplicaServer(out_block_id, amended_updates);
  auto end = std::chrono::steady_clock::now();
  auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
  LOG4CPLUS_INFO(
      logger_,
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
  std::string cid = "";
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

}  // namespace consensus
}  // namespace concord
