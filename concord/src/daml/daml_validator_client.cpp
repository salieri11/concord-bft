// Copyright 2019 VMware, all rights reserved

#include "daml_validator_client.hpp"

#include <opentracing/tracer.h>
#include <chrono>
#include <sstream>

#include "utils/open_tracing_utils.hpp"

using concord::utils::InjectSpanAsMetadata;
using std::map;
using std::string;
using std::vector;

namespace da_kvbc = com::digitalasset::kvbc;

namespace concord {
namespace daml {

grpc::Status DamlValidatorClient::Validate(
    const string& submission, const google::protobuf::Timestamp& record_time,
    const string& participant_id, const string& correlation_id,
    opentracing::Span& parent_span, vector<string>& out_read_set,
    KeyValueStorageOperations& storage_operations) {
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
        HandleReadEvent(event.read(), storage_operations, &output_event);
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

      case da_kvbc::EventFromValidator::kWrite: {
        HandleWriteEvent(event.write(), storage_operations);
        break;
      }

      case da_kvbc::EventFromValidator::kDone: {
        auto& done_event = event.done();
        vector<string> read_set(done_event.read_set().begin(),
                                done_event.read_set().end());
        out_read_set.swap(read_set);
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
    KeyValueStorageOperations& storage_operations,
    da_kvbc::EventToValidator* event) {
  auto& keys = read_event.keys();
  const auto values = storage_operations.Read(keys);
  da_kvbc::EventToValidator_ReadResult* read_result =
      event->mutable_read_result();
  read_result->set_tag(read_event.tag());
  for (auto const& entry : values) {
    da_kvbc::KeyValuePair* pair = read_result->add_key_value_pairs();
    pair->set_key(entry.first);
    pair->set_value(entry.second);
  }
}

void DamlValidatorClient::HandleWriteEvent(
    const da_kvbc::EventFromValidator::Write& write_event,
    KeyValueStorageOperations& storage_operations) {
  for (const auto& kv : write_event.updates()) {
    const auto& thin_replica_ids = kv.trids();
    vector<string> copied_thin_replica_ids(thin_replica_ids.begin(),
                                           thin_replica_ids.end());
    storage_operations.Write(kv.key(), kv.value(), copied_thin_replica_ids);
  }
}

}  // namespace daml
}  // namespace concord
