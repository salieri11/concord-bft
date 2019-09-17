// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_DAML_VALIDATOR_CLIENT_HPP_
#define CONCORD_DAML_VALIDATOR_CLIENT_HPP_

#include <google/protobuf/timestamp.pb.h>
#include <grpcpp/grpcpp.h>
#include <map>
#include <string>

#include "daml_validator.grpc.pb.h"

namespace concord {
namespace daml {

class DamlValidatorClient {
 public:
  DamlValidatorClient(uint16_t replica_id,
                      std::shared_ptr<grpc::ChannelInterface> channel)
      : stub_(com::digitalasset::kvbc::ValidationService::NewStub(channel)),
        replica_id_(replica_id) {}

  grpc::Status Validate(
      std::string entryId, std::string submission,
      google::protobuf::Timestamp& recordTime,
      const std::map<std::string, std::string>& input_state_entries,
      std::string participant_id,
      com::digitalasset::kvbc::ValidateResponse* out);

 private:
  std::unique_ptr<com::digitalasset::kvbc::ValidationService::Stub> stub_;
  uint16_t replica_id_;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_VALIDATOR_CLIENT_HPP_
