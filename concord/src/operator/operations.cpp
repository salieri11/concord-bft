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

#include "operations.hpp"
#include <opentracing/tracer.h>
using namespace bft::client;

concord::op::Operations::Operations(const concord::op::Config& config,
                                    bft::client::Client& client)
    : priv_key_(concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
          config.signing_key_path + "/operator_priv.pem", "secp256r1")),
      client_(client) {
  if (priv_key_ == nullptr) {
    LOG_WARN(logger_,
             "The private key file does not exist, operator won't be able to "
             "sign its messages");
  }
}

std::optional<com::vmware::concord::ReconfigurationSmResponse>
concord::op::Operations::initiateMockCommand(
    std::chrono::milliseconds timeout) {
  auto span = opentracing::Tracer::Global()->StartSpan("OperatorMockCommand");
  // Prepare the reconfiguration request
  com::vmware::concord::ConcordRequest conc_req;
  auto reconfiguration_req = conc_req.mutable_reconfiguration_sm_request();
  auto data = "Hello From Operator";
  reconfiguration_req->set_command(data);
  auto sig = std::string();
  if (priv_key_) sig = priv_key_->Sign(data);
  reconfiguration_req->set_signature(sig);
  reconfiguration_req->set_pluginid(
      com::vmware::concord::ReconfigurationSmRequest_PluginId_MOCK);

  // Send the reconfiguration request
  auto seqNum = 1u;
  std::string cid = "operator-mock-command";

  WriteConfig wc{RequestConfig{false, seqNum, 64 * 1024, timeout, cid, ""},
                 LinearizableQuorum{}};
  Msg msg(conc_req.ByteSizeLong());
  conc_req.SerializeToArray(msg.data(), msg.size());
  auto res = client_.send(wc, std::move(msg));

  // Return the result
  com::vmware::concord::ConcordResponse conc_response;
  conc_response.ParseFromArray(res.matched_data.data(),
                               res.matched_data.size());
  if (!conc_response.has_reconfiguration_sm_response()) {
    return {};
  }
  return conc_response.reconfiguration_sm_response();
}
