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

#include "external_client.hpp"

namespace concord {
namespace external_client {

using bftEngine::ClientMsgFlag;
using concord::kvbc::ClientConfig;
using concord::kvbc::IClient;
using config::CommConfig;
using config::ConcordConfiguration;
using namespace config_pool;

using namespace bft::communication;

ConcordClient::ConcordClient(const ConcordConfiguration& config, int client_id,
                             const ClientPoolConfig& pool_config) {
  logger_ = log4cplus::Logger::getInstance("com.vmware.external_client_pool");
  client_id_ = client_id;
  CreateClient(config, pool_config);
}

ConcordClient::~ConcordClient() noexcept {
  try {
    comm_->Stop();
  } catch (...) {
  }
}

void ConcordClient::SendRequest(const void* request, std::uint32_t request_size,
                                ClientMsgFlag flags,
                                std::chrono::milliseconds timeout_ms,
                                std::uint32_t reply_size, void* out_reply,
                                std::uint32_t* out_actual_reply_size,
                                uint64_t seq_num,
                                const std::string& correlation_id) {
  auto res = client_->sendRequest(flags, static_cast<const char*>(request),
                                  request_size, seq_num, timeout_ms.count(),
                                  reply_size, static_cast<char*>(out_reply),
                                  *out_actual_reply_size, correlation_id);

  assert(res >= -2 && res < 1);

  if (res != 0)
    LOG4CPLUS_ERROR(logger_, "reqSeqNum=" << seq_num
                                          << " cid=" << correlation_id
                                          << " has failed to invoke");
  if (res == -1)
    throw ClientRequestException{
        concordUtils::Status::GeneralError("timeout"),
        "ConcordClient failed to send a synchronous request"};
  else if (res == -2)
    throw ClientRequestException{
        concordUtils::Status::InvalidArgument("small buffer"),
        "ConcordClient failed to send a synchronous request"};
}

void ConcordClient::CreateCommConfig(CommConfig& comm_config,
                                     const ConcordConfiguration& config,
                                     int num_replicas,
                                     ClientPoolConfig pool_config) {
  const auto nodes = config.subscope(pool_config.PARTICIPANT_NODES, 0);
  const auto node = nodes.subscope(pool_config.PARTICIPANT_NODE, 0);
  const auto external_clients_conf =
      node.subscope(pool_config.EXTERNAL_CLIENTS, client_id_);
  const auto client_conf =
      external_clients_conf.subscope(pool_config.CLIENT, 0);
  comm_config.commType = config.getValue<decltype(comm_config.commType)>(
      pool_config.COMM_PROTOCOL);
  comm_config.listenIp = "external_client";
  comm_config.listenPort =
      client_conf.getValue<decltype(comm_config.listenPort)>(
          pool_config.CLIENT_PORT);
  comm_config.bufferLength =
      config.getValue<decltype(comm_config.bufferLength)>(
          pool_config.COMM_BUFF_LEN);
  comm_config.selfId =
      client_conf.getValue<decltype(comm_config.selfId)>(pool_config.CLIENT_ID);

  if (comm_config.commType == "tcp" || comm_config.commType == "tls") {
    comm_config.maxServerId = num_replicas - 1;
  }

  if (comm_config.commType == "tls") {
    comm_config.certificatesRootPath =
        config.getValue<decltype(comm_config.certificatesRootPath)>(
            pool_config.CERT_FOLDER);
    comm_config.cipherSuite =
        config.getValue<decltype(comm_config.cipherSuite)>(
            pool_config.CIPHER_SUITE);
  }
  comm_config.statusCallback = nullptr;
}

void ConcordClient::CreateClient(const ConcordConfiguration& config,
                                 ClientPoolConfig pool_config) {
  const auto num_replicas =
      config.getValue<std::uint16_t>(pool_config.NUM_REPLICAS);
  ClientConfig client_config;
  const auto nodes = config.subscope(pool_config.PARTICIPANT_NODES, 0);
  const auto node = nodes.subscope(pool_config.PARTICIPANT_NODE, 0);
  const auto external_clients_conf =
      node.subscope(pool_config.EXTERNAL_CLIENTS, client_id_);
  const auto client_conf =
      external_clients_conf.subscope(pool_config.CLIENT, 0);
  client_config.fVal =
      config.getValue<decltype(client_config.fVal)>(pool_config.F_VAL);
  client_config.cVal =
      config.getValue<decltype(client_config.cVal)>(pool_config.C_VAL);
  client_config.clientId =
      client_conf.getValue<decltype(client_config.clientId)>(
          pool_config.CLIENT_ID);
  CommConfig comm_config;
  CreateCommConfig(comm_config, config, num_replicas, pool_config);
  for (auto i = 0u; i < num_replicas; ++i) {
    const auto& node_conf = config.subscope(pool_config.NODE_VAR, i);
    const auto replica_conf = node_conf.subscope(pool_config.REPLICA_VAR, 0);
    const auto replica_id =
        replica_conf.getValue<decltype(comm_config.nodes)::key_type>(
            pool_config.CLIENT_ID);
    NodeInfo node_info;
    node_info.host = replica_conf.getValue<decltype(node_info.host)>(
        pool_config.REPLICA_HOST);
    node_info.port = replica_conf.getValue<decltype(node_info.port)>(
        pool_config.REPLICA_PORT);
    node_info.isReplica = true;
    comm_config.nodes[replica_id] = node_info;
  }
  // Ensure exception safety by creating local pointers and only moving to
  // object members if construction and startup haven't thrown.
  LOG4CPLUS_DEBUG(
      logger_, "Client_id=" << client_id_ << " Creating communication module");
  auto comm = pool_config.ToCommunication(comm_config);
  LOG4CPLUS_DEBUG(
      logger_,
      "Client_id="
          << client_id_
          << " starting communication and creating simple client instance");
  comm_ = std::move(comm);
  comm_.get()->Start();
  auto client = std::unique_ptr<bftEngine::SimpleClient>{
      bftEngine::SimpleClient::createSimpleClient(
          comm_.get(), client_config.clientId, client_config.fVal,
          client_config.cVal)};
  seqGen_ = bftEngine::SeqNumberGeneratorForClientRequests::
      createSeqNumberGeneratorForClientRequests();
  client_ = std::move(client);
  LOG4CPLUS_INFO(logger_, "client_id=" << client_id_ << " creation succeeded");
}
int ConcordClient::getClientId() const { return client_id_; }

uint64_t ConcordClient::getClientSeqNum() const { return seq_num_; }

void ConcordClient::generateClientSeqNum() {
  seq_num_ = seqGen_->generateUniqueSequenceNumberForRequest();
}
void ConcordClient::setStartRequestTime() {
  start_job_time_ = std::chrono::duration_cast<std::chrono::milliseconds>(
                        std::chrono::steady_clock::now().time_since_epoch())
                        .count();
}

uint64_t ConcordClient::getStartRequestTime() const { return start_job_time_; }
bool ConcordClient::isRunning() const { return comm_->isRunning(); }

}  // namespace external_client
}  // namespace concord
