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
#include "bftclient/exception.h"
using namespace bft::client;

concord::op::Operations::Operations(const concord::op::Config& config,
                                    bft::client::Client& client)
    : priv_key_(concord::utils::openssl_crypto::DeserializePrivateKeyFromPem(
          config.signing_key_path + "/operator_priv.pem", "secp256r1")),
      client_(client),
      config_(config),
      snGen_(config.client_config.id) {
  if (priv_key_ == nullptr) {
    LOG_WARN(logger_,
             "The private key file does not exist, operator won't be able to "
             "sign its messages");
  }
}

concord::op::Response concord::op::Operations::WedgeStatus(
    std::chrono::milliseconds timeout) {
  auto span =
      opentracing::Tracer::Global()->StartSpan("OperatorWedgeStatusCommand");
  com::vmware::concord::ConcordRequest conc_req;
  auto recong_req = conc_req.mutable_reconfiguration_sm_request();
  recong_req->mutable_wedge_cmd();
  signRequest(*recong_req);
  std::string cid = "operator-wedge-status-command";
  return initiateReadRequest(conc_req, All{}, timeout, cid, "");
}

void concord::op::Operations::signRequest(
    com::vmware::concord::ReconfigurationSmRequest& request) {
  auto str_req = request.SerializeAsString();
  auto sig = std::string();
  if (priv_key_) sig = priv_key_->Sign(str_req);
  request.set_signature(sig);
}

concord::op::Response concord::op::Operations::initiateWedge(
    std::chrono::milliseconds timeout) {
  auto span = opentracing::Tracer::Global()->StartSpan("OperatorWedgeCommand");
  // Prepare the reconfiguration request
  com::vmware::concord::ConcordRequest conc_req;
  auto reconfiguration_req = conc_req.mutable_reconfiguration_sm_request();
  reconfiguration_req->mutable_wedge_cmd();
  signRequest(*reconfiguration_req);
  std::string cid = "operator-wedge-command";
  return initiateWriteRequest(conc_req, LinearizableQuorum{}, timeout, cid, "");
}

concord::op::Response concord::op::Operations::initiateSwDownload(
    std::chrono::milliseconds timeout) {
  auto span = opentracing::Tracer::Global()->StartSpan(
      "OperatorDownloadSwVersionCommand");
  // Prepare the reconfiguration request
  com::vmware::concord::ConcordRequest conc_req;
  auto reconfiguration_req = conc_req.mutable_reconfiguration_sm_request();
  reconfiguration_req->mutable_download_sw_version_cmd();
  signRequest(*reconfiguration_req);
  std::string cid = "operator-download-sw-version-command";
  return initiateWriteRequest(conc_req, LinearizableQuorum{}, timeout, cid, "");
}

concord::op::Response concord::op::Operations::initiateWriteRequest(
    const com::vmware::concord::ConcordRequest& request,
    const WriteQuorum& quorum, std::chrono::milliseconds timeout,
    const std::string& cid, const std::string& span_context) {
  WriteConfig wc{RequestConfig{false, snGen_.unique(), 64 * 1024, timeout, cid,
                               span_context},
                 quorum};
  Msg msg(request.ByteSizeLong());
  request.SerializeToArray(msg.data(), msg.size());
  bft::client::Reply res;
  try {
    res = client_.send(wc, std::move(msg));
  } catch (bft::client::TimeoutException& e) {
    LOG_WARN(logger_, e.what());
  }
  return Response(res);
}
concord::op::Response concord::op::Operations::initiateReadRequest(
    const com::vmware::concord::ConcordRequest& request,
    const ReadQuorum& quorum, std::chrono::milliseconds timeout,
    const std::string& cid, const std::string& span_context) {
  ReadConfig rc{
      RequestConfig{false, snGen_.unique(), 64 * 1024, timeout, cid, ""},
      quorum};
  Msg msg(request.ByteSizeLong());
  request.SerializeToArray(msg.data(), msg.size());
  bft::client::Reply res;
  try {
    res = client_.send(rc, std::move(msg));
  } catch (bft::client::TimeoutException& e) {
    LOG_WARN(logger_, e.what());
  }
  return Response(res);
}
concord::op::Response concord::op::Operations::initiateHasSwVersion(
    std::chrono::milliseconds timeout) {
  auto span =
      opentracing::Tracer::Global()->StartSpan("OperatorHasSwVersionCommand");
  com::vmware::concord::ConcordRequest conc_req;
  auto recong_req = conc_req.mutable_reconfiguration_sm_request();
  recong_req->mutable_has_sw_version_cmd();
  signRequest(*recong_req);
  std::string cid = "operator-has-sw-version-command";
  return initiateReadRequest(conc_req, All{}, timeout, cid, "");
}
concord::op::Response concord::op::Operations::initiateInstallSwVersion(
    std::chrono::milliseconds timeout) {
  auto span = opentracing::Tracer::Global()->StartSpan(
      "OperatorUpgradeSwVersionCommand");
  // Prepare the reconfiguration request
  com::vmware::concord::ConcordRequest conc_req;
  auto reconfiguration_req = conc_req.mutable_reconfiguration_sm_request();
  reconfiguration_req->mutable_upgrade_sw_version_cmd();
  signRequest(*reconfiguration_req);
  std::string cid = "operator-upgrade-sw_version-command";
  return initiateWriteRequest(conc_req, LinearizableQuorum{}, timeout, cid, "");
}
concord::op::Response::Response(const bft::client::Reply& reply) {
  for (auto& rsi : reply.rsi) {
    com::vmware::concord::ConcordReplicaSpecificInfoResponse rsi_res;
    rsi_res.ParseFromArray(rsi.second.data(), rsi.second.size());
    rsis.emplace_back(rsi.first, rsi_res);
  }
  res.ParseFromArray(reply.matched_data.data(), reply.matched_data.size());
}
