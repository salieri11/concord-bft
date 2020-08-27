// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_DAML_VALIDATOR_CLIENT_HPP_
#define CONCORD_DAML_VALIDATOR_CLIENT_HPP_

#include <google/protobuf/timestamp.pb.h>
#include <grpcpp/grpcpp.h>
#include <opentracing/span.h>
#include <map>
#include <string>
#include <utils/concord_prometheus_metrics.hpp>
#include "Logger.hpp"
#include "concord_storage.pb.h"
#include "daml_validator.grpc.pb.h"

namespace da_kvbc = com::digitalasset::kvbc;

namespace concord {
namespace daml {

typedef std::function<std::map<std::string, std::string>(
    const google::protobuf::RepeatedPtrField<std::string>&)>
    DamlKvbReadFunc;

typedef std::pair<std::string, std::string> ValueFingerprintPair;
typedef std::function<std::map<std::string, ValueFingerprintPair>(
    const google::protobuf::RepeatedPtrField<std::string>&)>
    KeyValueWithFingerprintReaderFunc;

// Represents a key-value pair with an associated access control list.
// The access control list is defined as a list of thin replica IDs.
typedef std::pair<std::string, com::vmware::concord::kvb::ValueWithTrids>
    KeyValuePairWithThinReplicaIds;

class IDamlValidatorClient {
 public:
  virtual grpc::Status Validate(
      const std::string& submission,
      const google::protobuf::Timestamp& record_time,
      const std::string& participant_id, const std::string& correlation_id,
      const opentracing::Span& parent_span, DamlKvbReadFunc read_from_storage,
      std::vector<std::string>* read_set,
      std::vector<KeyValuePairWithThinReplicaIds>* write_set) = 0;

  virtual grpc::Status PreExecute(
      const std::string& submission, const std::string& participant_id,
      const std::string& correlation_id, const opentracing::Span& parent_span,
      KeyValueWithFingerprintReaderFunc read_from_storage,
      com::vmware::concord::PreExecutionResult* pre_execution_result) = 0;

  virtual ~IDamlValidatorClient() = default;
};

class DamlValidatorClient : public IDamlValidatorClient {
 public:
  static int kMaxRequestsDuringPreExecution;

  DamlValidatorClient(
      uint16_t replica_id,
      com::digitalasset::kvbc::ValidationService::StubInterface* stub)
      : stub_(stub),
        replica_id_(replica_id),
        pre_execute_uses_streaming_protocol_(false) {}

  DamlValidatorClient(
      uint16_t replica_id, bool pre_execute_uses_streaming_protocol,
      com::digitalasset::kvbc::ValidationService::StubInterface* stub)
      : stub_(stub),
        replica_id_(replica_id),
        pre_execute_uses_streaming_protocol_(
            pre_execute_uses_streaming_protocol) {}

  grpc::Status Validate(
      const std::string& submission,
      const google::protobuf::Timestamp& record_time,
      const std::string& participant_id, const std::string& correlation_id,
      const opentracing::Span& parent_span, DamlKvbReadFunc read_from_storage,
      std::vector<std::string>* read_set,
      std::vector<KeyValuePairWithThinReplicaIds>* write_set) override;

  grpc::Status PreExecute(
      const std::string& submission, const std::string& participant_id,
      const std::string& correlation_id, const opentracing::Span& parent_span,
      KeyValueWithFingerprintReaderFunc read_from_storage,
      com::vmware::concord::PreExecutionResult* pre_execution_result) override;

 private:
  grpc::Status PreExecuteViaStreamingProtocol(
      const std::string& submission, const std::string& participant_id,
      const std::string& correlation_id, const opentracing::Span& parent_span,
      KeyValueWithFingerprintReaderFunc read_from_storage,
      com::vmware::concord::PreExecutionResult* pre_execution_result);

  void HandleReadEvent(
      const com::digitalasset::kvbc::EventFromValidator::Read& read_event,
      DamlKvbReadFunc read_from_storage,
      com::digitalasset::kvbc::EventToValidator* event);

  void HandleReadEventForPreExecution(
      const com::digitalasset::kvbc::PreprocessorFromEngine::ReadRequest&
          read_request,
      KeyValueWithFingerprintReaderFunc read_from_storage,
      com::digitalasset::kvbc::PreprocessorToEngine* reply);

  void SwapWriteSet(
      google::protobuf::RepeatedPtrField<
          com::digitalasset::kvbc::ProtectedKeyValuePair>* write_set,
      std::vector<KeyValuePairWithThinReplicaIds>* result);

  static void MangleDamlPreExecutionOutputForConcord(
      da_kvbc::PreExecutionOutput& pre_execution_output);
  static void MangleDamlWriteSetForConcord(
      da_kvbc::WriteSet* mutable_write_set);
  static void MangleDamlAclForConcord(
      da_kvbc::AccessControlList* mutable_access);

  std::unique_ptr<com::digitalasset::kvbc::ValidationService::StubInterface>
      stub_;
  uint16_t replica_id_;
  bool pre_execute_uses_streaming_protocol_;
  logging::Logger logger_ = logging::getLogger("concord.daml.validator");
  logging::Logger determinism_logger_ =
      logging::getLogger("concord.daml.determinism");
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_VALIDATOR_CLIENT_HPP_
