// Copyright 2019 VMware, all rights reserved

#include "daml_validator_client.hpp"

#include <opentracing/tracer.h>

using std::map;
using std::string;

namespace da_kvbc = com::digitalasset::kvbc;

namespace concord {
namespace daml {

grpc::Status DamlValidatorClient::ValidateSubmission(
    string entry_id, string submission,
    google::protobuf::Timestamp& record_time, string participant_id,
    string correlation_id, opentracing::Span& parent_span,
    da_kvbc::ValidateResponse* out) {
  da_kvbc::ValidateRequest req;
  req.set_submission(submission);
  req.set_entry_id(entry_id);
  req.set_replica_id(replica_id_);
  req.set_participant_id(participant_id);
  req.set_correlation_id(correlation_id);
  *req.mutable_record_time() = record_time;

  grpc::ClientContext context;
  auto validate_span = parent_span.tracer().StartSpan(
      "daml_validate", {opentracing::ChildOf(&parent_span.context())});
  return stub_->ValidateSubmission(&context, req, out);
}

grpc::Status DamlValidatorClient::ValidatePendingSubmission(
    string entry_id, const map<string, string>& input_state_entries,
    string correlation_id, opentracing::Span& parent_span,
    da_kvbc::ValidatePendingSubmissionResponse* out) {
  da_kvbc::ValidatePendingSubmissionRequest req;
  req.set_entry_id(entry_id);
  req.set_replica_id(replica_id_);
  req.set_correlation_id(correlation_id);

  for (auto const& entry : input_state_entries) {
    da_kvbc::KeyValuePair* kvpair = req.add_input_state();
    kvpair->set_key(entry.first);
    kvpair->set_value(entry.second);
  }
  grpc::ClientContext context;
  auto validate_span = parent_span.tracer().StartSpan(
      "daml_validate_pending", {opentracing::ChildOf(&parent_span.context())});
  return stub_->ValidatePendingSubmission(&context, req, out);
}

}  // namespace daml
}  // namespace concord
