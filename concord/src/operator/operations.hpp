// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#pragma once

#include <chrono>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

#include "Logger.hpp"
#include "bftclient/bft_client.h"
#include "bftclient/seq_num_generator.h"
#include "concord.cmf.hpp"
#include "concord.pb.h"
#include "config.h"
#include "utils/openssl_crypto_utils.hpp"

namespace concord::op {
struct Response {
  std::vector<
      std::tuple<bft::client::ReplicaId,
                 com::vmware::concord::ConcordReplicaSpecificInfoResponse>>
      rsis;
  com::vmware::concord::ConcordResponse res;

  Response(const bft::client::Reply& reply);
};

class Operations {
  /*
   * Here we will implement the actual operator actions.
   */
 public:
  Operations(const Config& config, bft::client::Client& client);
  Response WedgeStatus(std::chrono::milliseconds timeout);
  Response initiateWedge(std::chrono::milliseconds timeout);
  Response initiateSwDownload(std::chrono::milliseconds timeout);
  Response initiateHasSwVersion(std::chrono::milliseconds timeout);
  Response initiateInstallSwVersion(std::chrono::milliseconds timeout);

 private:
  /*
   * This method sign on a reconfiguration message and set the
   * ReconfigurationSmRequest signature to that signature.
   */
  void signRequest(concord::messages::ReconfigurationRequest& request);
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPrivateKey>
      priv_key_;
  bft::client::Client& client_;
  Config config_;
  bft::client::SeqNumberGenerator snGen_;
  logging::Logger logger_ = logging::getLogger("operator.operations");

  Response initiateWriteRequest(
      const com::vmware::concord::ConcordRequest& request,
      const bft::client::WriteQuorum& quorum, std::chrono::milliseconds timeout,
      const std::string& cid, const std::string& span_context);

  Response initiateReadRequest(
      const com::vmware::concord::ConcordRequest& request,
      const bft::client::ReadQuorum& quorum, std::chrono::milliseconds timeout,
      const std::string& cid, const std::string& span_context);
};
}  // namespace concord::op
