// Copyright 2019 VMware, all rights reserved

#include "daml_validator_client.hpp"

#include <hex_tools.h>
#include <opentracing/tracer.h>
#include <chrono>
#include <sstream>
#include "sha3_256.h"
#include "sparse_merkle/base_types.h"
#include "utils/open_tracing_utils.hpp"

using com::vmware::concord::kvb::ValueWithTrids;
using concord::utils::InjectSpanAsMetadata;
using std::map;
using std::string;
using std::vector;

namespace da_kvbc = com::digitalasset::kvbc;
using namespace concord::kvbc::sparse_merkle;
using namespace concord::util;

namespace concord {
namespace daml {

grpc::Status DamlValidatorClient::Validate(
    const string& submission, const google::protobuf::Timestamp& record_time,
    const string& participant_id, const string& correlation_id,
    const opentracing::Span& parent_span, DamlKvbReadFunc read_from_storage,
    vector<string>* read_set,
    vector<KeyValuePairWithThinReplicaIds>* write_set) {
  auto validate_span = parent_span.tracer().StartSpan(
      "daml_validate", {opentracing::ChildOf(&parent_span.context())});

  grpc::ClientContext context;
  InjectSpanAsMetadata(*validate_span, &context);
  std::unique_ptr<grpc::ClientReaderWriterInterface<
      da_kvbc::EventToValidator, da_kvbc::EventFromValidator>>
      stream(stub_->Validate(&context));

  // Initiate the validation by sending the submission to the validator.
  {
    da_kvbc::EventToValidator event;
    da_kvbc::ValidateRequest* req = event.mutable_validate_request();
    req->set_submission(submission);
    // NOTE(JM): entry_id not used with new interface.
    req->set_replica_id(replica_id_);
    req->set_participant_id(participant_id);
    req->set_correlation_id(correlation_id);
    *req->mutable_record_time() = record_time;
    stream->Write(event);
  }

  // Handle events from the validator until validation has finished.
  bool done = false;
  da_kvbc::EventFromValidator event;
  while (!done && stream->Read(&event)) {
    switch (event.from_validator_case()) {
      case da_kvbc::EventFromValidator::kRead: {
        da_kvbc::EventToValidator output_event;
        auto start = std::chrono::steady_clock::now();
        HandleReadEvent(event.read(), read_from_storage, &output_event);
        auto readDur = std::chrono::duration_cast<std::chrono::milliseconds>(
                           std::chrono::steady_clock::now() - start)
                           .count();
        stream->Write(output_event);
        auto writeDur = std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::steady_clock::now() - start)
                            .count();
        LOG_INFO(
            logger_,
            "Done handling DAML read, cid: "
                << correlation_id << ", readDur: " << readDur
                << ", writeDur: " << writeDur << ", read set size: "
                << output_event.read_result().key_value_pairs_size()
                << ", clock: "
                << std::chrono::steady_clock::now().time_since_epoch().count());
        break;
      }

      case da_kvbc::EventFromValidator::kDone: {
        auto done_event = event.mutable_done();
        vector<string> received_read_set(done_event->read_set().begin(),
                                         done_event->read_set().end());
        read_set->swap(received_read_set);
        SwapWriteSet(done_event->mutable_write_set(), write_set);
        stream->WritesDone();
        done = true;
        break;
      }

      case da_kvbc::EventFromValidator::FROM_VALIDATOR_NOT_SET:
        throw std::runtime_error(
            "DamlValidatorClient::Validate: FROM_VALIDATOR_NOT_SET");
    }
  }

  // https://jira.eng.vmware.com/browse/BC-3662
  // Concord and the execution engine processes are tightly coupled.
  // In case of an error at the process level we should crash the replica
  // in order not to deviate its state from other replcias, and let state
  // transfer kick in.
  auto status = stream->Finish();
  if (!status.ok()) {
    LOG_FATAL(logger_,
              "Failed to execute command againt execution engine, grpc error ["
                  << status.error_message() << "]");
    assert(status.ok());
  }

  return status;
}

void DamlValidatorClient::HandleReadEvent(
    const da_kvbc::EventFromValidator::Read& read_event,
    DamlKvbReadFunc read_from_storage, da_kvbc::EventToValidator* event) {
  auto& keys = read_event.keys();
  const auto values = read_from_storage(keys);
  da_kvbc::EventToValidator_ReadResult* read_result =
      event->mutable_read_result();
  read_result->set_tag(read_event.tag());

  // Hash and log
  std::string currKV;
  currKV.reserve(200);
  std::string toHash;
  toHash.reserve(1000);
  int i = 0;

  for (auto const& entry : values) {
    da_kvbc::KeyValuePair* pair = read_result->add_key_value_pairs();
    pair->set_key(entry.first);
    pair->set_value(entry.second);
    currKV = entry.first;
    currKV += entry.second;
    toHash += currKV;
    LOG_DEBUG(determinism_logger_, "Read KV to stream ["
                                       << (concordUtils::HexPrintBuffer{
                                              currKV.c_str(), currKV.size()})
                                       << " order [" << i++ << "]");
  }
  LOG_DEBUG(
      determinism_logger_,
      "Hash of read stream ["
          << Hash(SHA3_256().digest(toHash.c_str(), toHash.size())).toString()
          << "]");
}

void DamlValidatorClient::SwapWriteSet(
    google::protobuf::RepeatedPtrField<
        com::digitalasset::kvbc::ProtectedKeyValuePair>* write_set,
    vector<KeyValuePairWithThinReplicaIds>* result) {
  std::string currKV;
  currKV.reserve(100);
  std::string toHash;
  toHash.reserve(1000);
  int i = 0;

  for (auto kv : *write_set) {
    ValueWithTrids value_with_thin_replica_ids;
    currKV = kv.key();
    currKV += kv.value();
    value_with_thin_replica_ids.set_value(std::move(*kv.release_value()));
    for (auto thin_replica_id : *(kv.mutable_trids())) {
      currKV += thin_replica_id;
      value_with_thin_replica_ids.add_trid(std::move(thin_replica_id));
    }
    toHash += currKV;
    result->push_back(
        {std::move(*kv.release_key()), value_with_thin_replica_ids});
    LOG_DEBUG(determinism_logger_, "Write KV from stream ["
                                       << (concordUtils::HexPrintBuffer{
                                              currKV.c_str(), currKV.size()})
                                       << " order [" << i++ << "]");
  }
  LOG_DEBUG(
      determinism_logger_,
      "Hash of write stream ["
          << Hash(SHA3_256().digest(toHash.c_str(), toHash.size())).toString()
          << "]");
}

void HandleReadRequestForPreExecution(
    const com::digitalasset::kvbc::PreExecuteResponse::ReadRequest&
        read_request,
    KeyValueWithFingerprintReaderFunc read_from_storage,
    com::digitalasset::kvbc::PreExecuteRequest* reply) {
  auto& keys = read_request.keys();
  const auto values = read_from_storage(keys);
  da_kvbc::PreExecuteRequest_ReadResult* read_result =
      reply->mutable_read_result();
  for (auto const& entry : values) {
    da_kvbc::KeyValueFingerprintTriple* key_value_fingerprint =
        read_result->add_key_value_pairs();
    key_value_fingerprint->set_key(entry.first);
    key_value_fingerprint->set_value(entry.second.first);
    key_value_fingerprint->set_fingerprint(entry.second.second);
  }
}

// This is just a safeguard to avoid an infinite loop in
// DamlValidatorClient::PreExecute. We expect all immutable key-value pairs to
// be in the cache of the DAML Execution Engine eventually and all mutable ones
// to be requested. In the normal case we expect 2 requests to generate
// pre-execution results (first receive read request for the submission, then
// send read result and receive pre-execution results). Specifically, the only
// case when we should have to make another roundtrip between the DAML Commands
// Handler and Execution Engine (i.e., after the first request) is when an item
// expires in the cache between sending a response with a read request and
// receiving the read results in the next iteration. In case we have to make
// more than 2 requests constantly the cache expiry policy needs to be adjusted
// in the DAML Execution Engine.
int DamlValidatorClient::kMaxRequestsDuringPreExecution = 4;

grpc::Status DamlValidatorClient::PreExecute(
    const std::string& submission, const std::string& participant_id,
    const std::string& correlation_id, const opentracing::Span& parent_span,
    KeyValueWithFingerprintReaderFunc read_from_storage,
    com::vmware::concord::PreExecutionResult* pre_execution_result) {
  if (pre_execute_uses_streaming_protocol_) {
    return PreExecuteViaStreamingProtocol(
        submission, participant_id, correlation_id, parent_span,
        read_from_storage, pre_execution_result);
  }

  auto child_span = parent_span.tracer().StartSpan(
      "daml_pre_execute", {opentracing::ChildOf(&parent_span.context())});

  da_kvbc::PreExecuteRequest pre_execute_request;
  da_kvbc::PreExecuteRequest_PreExecutionRequest* request =
      pre_execute_request.mutable_preexecution_request();
  request->set_submission(submission);
  request->set_submitting_participant_id(participant_id);
  request->set_replica_id(replica_id_);
  request->set_correlation_id(correlation_id);

  grpc::Status last_status = grpc::Status::OK;
  int request_count = 1;
  bool success = false;
  while (request_count <= kMaxRequestsDuringPreExecution) {
    LOG_DEBUG(logger_,
              "Sent " << request_count << " pre-execution request(s) so far");
    da_kvbc::PreExecuteResponse response;
    grpc::ClientContext context;
    InjectSpanAsMetadata(*child_span, &context);
    last_status =
        stub_->PreExecuteMultiStep(&context, pre_execute_request, &response);
    ++request_count;
    if (response.has_read_request()) {
      LOG_DEBUG(logger_, "Handling pre-execution read request: "
                             << response.read_request().ShortDebugString());
      HandleReadRequestForPreExecution(response.read_request(),
                                       read_from_storage, &pre_execute_request);
    } else if (response.has_preexecution_result()) {
      SetAndLogPreExecutionResult(response, pre_execution_result);
      success = true;
      break;
    } else {
      LOG_ERROR(
          logger_,
          "Failed pre-execution - invalid response from DAML Execution Engine: "
              << response.ShortDebugString());
      return grpc::Status(grpc::StatusCode::INTERNAL,
                          "No read request or result in response");
    }
  }
  if (success) {
    LOG_DEBUG(logger_, "Returning pre-execution status: "
                           << last_status.error_code() << " "
                           << last_status.error_message());
    return last_status;
  } else {
    LOG_DEBUG(logger_, "Did not receive a pre-execution result after "
                           << kMaxRequestsDuringPreExecution << " requests");
    return grpc::Status(grpc::StatusCode::INTERNAL, "Too many read iterations");
  }
}

void DamlValidatorClient::SetAndLogPreExecutionResult(
    da_kvbc::PreExecuteResponse& pre_execute_response,
    com::vmware::concord::PreExecutionResult* pre_execution_result) {
  auto result = pre_execute_response.mutable_preexecution_result();
  result->Swap(pre_execution_result);
  if (logger_.getChainedLogLevel() <= log4cplus::DEBUG_LOG_LEVEL) {
    da_kvbc::PreExecutionOutput pre_execution_output;
    if (pre_execution_output.ParseFromString(pre_execution_result->output())) {
      LOG_DEBUG(logger_, "Received pre-execution output: "
                             << pre_execution_output.ShortDebugString());
      std::string to_hash(pre_execution_result->output());
      LOG_DEBUG(logger_,
                "Hash of pre-execution output ["
                    << Hash(SHA3_256().digest(to_hash.c_str(), to_hash.size()))
                           .toString()
                    << "]");
    } else {
      LOG_DEBUG(logger_, "Could not parse pre-execution output");
    }
  }
}

grpc::Status DamlValidatorClient::PreExecuteViaStreamingProtocol(
    const std::string& submission, const std::string& participant_id,
    const std::string& correlation_id, const opentracing::Span& parent_span,
    KeyValueWithFingerprintReaderFunc read_from_storage,
    com::vmware::concord::PreExecutionResult* pre_execution_result) {
  auto child_span = parent_span.tracer().StartSpan(
      "daml_pre_execute", {opentracing::ChildOf(&parent_span.context())});

  grpc::ClientContext context;
  InjectSpanAsMetadata(*child_span, &context);
  std::unique_ptr<grpc::ClientReaderWriterInterface<
      da_kvbc::PreprocessorToEngine, da_kvbc::PreprocessorFromEngine>>
      stream(stub_->Preexecute(&context));

  // Initiate pre-execution by sending the submission to the validator.
  {
    da_kvbc::PreprocessorToEngine event;
    da_kvbc::PreprocessorToEngine_PreExecutionRequest* request =
        event.mutable_preexecution_request();
    request->set_submission(submission);
    request->set_submitting_participant_id(participant_id);
    request->set_replica_id(replica_id_);
    request->set_correlation_id(correlation_id);
    // FIXME(erdemik): Remove span_context from proto (sent as metadata).
    stream->Write(event);
  }

  // Handle events from the execution engine until pre-execution has finished.
  bool done = false;
  da_kvbc::PreprocessorFromEngine event;
  while (!done && stream->Read(&event)) {
    switch (event.from_engine_case()) {
      case da_kvbc::PreprocessorFromEngine::kReadRequest: {
        da_kvbc::PreprocessorToEngine reply;
        HandleReadEventForPreExecution(event.read_request(), read_from_storage,
                                       &reply);
        stream->Write(reply);
        break;
      }

      case da_kvbc::PreprocessorFromEngine::kPreexecutionResult: {
        auto result = event.mutable_preexecution_result();
        result->Swap(pre_execution_result);
        stream->WritesDone();
        done = true;
        break;
      }

      case da_kvbc::PreprocessorFromEngine::FROM_ENGINE_NOT_SET:
        throw std::runtime_error(
            "DamlValidatorClient::PreExecute: FROM_ENGINE_NOT_SET");
    }
  }

  return stream->Finish();
}

void DamlValidatorClient::HandleReadEventForPreExecution(
    const com::digitalasset::kvbc::PreprocessorFromEngine::ReadRequest&
        read_request,
    KeyValueWithFingerprintReaderFunc read_from_storage,
    com::digitalasset::kvbc::PreprocessorToEngine* reply) {
  auto& keys = read_request.keys();
  const auto values = read_from_storage(keys);
  da_kvbc::PreprocessorToEngine_ReadResult* read_result =
      reply->mutable_read_result();
  read_result->set_tag(read_request.tag());
  for (auto const& entry : values) {
    da_kvbc::KeyValueFingerprintTriple* key_value_fingerprint =
        read_result->add_key_value_pairs();
    key_value_fingerprint->set_key(entry.first);
    key_value_fingerprint->set_value(entry.second.first);
    key_value_fingerprint->set_fingerprint(entry.second.second);
  }
}

}  // namespace daml
}  // namespace concord
