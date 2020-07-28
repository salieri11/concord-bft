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

  return stream->Finish();
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
    LOG_DEBUG(dtrmnsm_logger_, "Read KV to stream ["
                                   << (concordUtils::HexPrintBuffer{
                                          currKV.c_str(), currKV.size()})
                                   << " order [" << i++ << "]");
  }
  LOG_DEBUG(
      dtrmnsm_logger_,
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
    LOG_DEBUG(dtrmnsm_logger_, "Write KV from stream ["
                                   << (concordUtils::HexPrintBuffer{
                                          currKV.c_str(), currKV.size()})
                                   << " order [" << i++ << "]");
  }
  LOG_DEBUG(
      dtrmnsm_logger_,
      "Hash of write stream ["
          << Hash(SHA3_256().digest(toHash.c_str(), toHash.size())).toString()
          << "]");
}

grpc::Status DamlValidatorClient::PreExecute(
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
