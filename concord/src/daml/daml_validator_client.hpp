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

namespace concord {
namespace daml {

typedef std::function<std::map<std::string, std::string>(
    const google::protobuf::RepeatedPtrField<std::string>&)>
    DamlKvbReadFunc;

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

  virtual ~IDamlValidatorClient() = default;
};

class DamlValidatorClient : public IDamlValidatorClient {
 public:
  DamlValidatorClient(
      uint16_t replica_id,
      com::digitalasset::kvbc::ValidationService::StubInterface* stub)
      : stub_(stub), replica_id_(replica_id) {}

  grpc::Status Validate(
      const std::string& submission,
      const google::protobuf::Timestamp& record_time,
      const std::string& participant_id, const std::string& correlation_id,
      const opentracing::Span& parent_span, DamlKvbReadFunc read_from_storage,
      std::vector<std::string>* read_set,
      std::vector<KeyValuePairWithThinReplicaIds>* write_set) override;

 private:
  logging::Logger logger_ = logging::getLogger("concord.daml.validator");
  static void HandleReadEvent(
      const com::digitalasset::kvbc::EventFromValidator::Read& read_event,
      DamlKvbReadFunc read_from_storage,
      com::digitalasset::kvbc::EventToValidator* event);

  static void SwapWriteSet(
      google::protobuf::RepeatedPtrField<
          com::digitalasset::kvbc::ProtectedKeyValuePair>* write_set,
      std::vector<KeyValuePairWithThinReplicaIds>* result);

  std::unique_ptr<com::digitalasset::kvbc::ValidationService::StubInterface>
      stub_;
  uint16_t replica_id_;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_VALIDATOR_CLIENT_HPP_
