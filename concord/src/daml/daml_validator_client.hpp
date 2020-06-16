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
#include "daml_validator.grpc.pb.h"

namespace concord {
namespace daml {

class KeyValueStorageOperations {
 public:
  virtual ~KeyValueStorageOperations() = default;

  virtual std::map<std::string, std::string> Read(
      const google::protobuf::RepeatedPtrField<std::string>& keys) = 0;

  virtual void Write(const std::string& key, const std::string& value,
                     const std::vector<std::string>& thin_replica_ids) = 0;
};

class IDamlValidatorClient {
 public:
  virtual grpc::Status Validate(
      const std::string& submission,
      const google::protobuf::Timestamp& record_time,
      const std::string& participant_id, const std::string& correlation_id,
      opentracing::Span& parent_span, std::vector<std::string>& out_read_set,
      KeyValueStorageOperations& storage_operations) = 0;

  virtual ~IDamlValidatorClient() = default;
};

class DamlValidatorClient : public IDamlValidatorClient {
 public:
  DamlValidatorClient(
      uint16_t replica_id,
      com::digitalasset::kvbc::ValidationService::StubInterface* stub)
      : stub_(stub), replica_id_(replica_id) {}

  grpc::Status Validate(const std::string& submission,
                        const google::protobuf::Timestamp& record_time,
                        const std::string& participant_id,
                        const std::string& correlation_id,
                        opentracing::Span& parent_span,
                        std::vector<std::string>& out_read_set,
                        KeyValueStorageOperations& storage_operations) override;

 private:
  logging::Logger logger_ = logging::getLogger("concord.daml.validator");
  static void HandleReadEvent(
      const com::digitalasset::kvbc::EventFromValidator::Read& read_event,
      KeyValueStorageOperations& storage_operations,
      com::digitalasset::kvbc::EventToValidator* event);

  static void HandleWriteEvent(
      const com::digitalasset::kvbc::EventFromValidator::Write& write_event,
      KeyValueStorageOperations& storage_operations);

  std::unique_ptr<com::digitalasset::kvbc::ValidationService::StubInterface>
      stub_;
  uint16_t replica_id_;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_VALIDATOR_CLIENT_HPP_
