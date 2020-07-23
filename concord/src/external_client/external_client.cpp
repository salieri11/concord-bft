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

namespace concord::external_client {

using bftEngine::ClientMsgFlag;
using config::CommConfig;
using config::ConcordConfiguration;
using namespace config_pool;
using namespace bftEngine;

using namespace bft::communication;

std::shared_ptr<std::vector<char>> ConcordClient::reply_ =
    std::make_shared<std::vector<char>>(0);
uint16_t ConcordClient::required_num_of_replicas_ = 0;
uint16_t ConcordClient::num_of_replicas_ = 0;

ConcordClient::ConcordClient(const ConcordConfiguration& config, int client_id,
                             ClientPoolConfig& pool_config,
                             const SimpleClientParams& client_params) {
  logger_ = logging::getLogger("com.vmware.external_client_pool");
  client_id_ = client_id;
  CreateClient(config, pool_config, client_params);
}

ConcordClient::~ConcordClient() noexcept {
  try {
    comm_->Stop();
  } catch (...) {
  }
}

uint32_t ConcordClient::SendRequest(const void* request,
                                    std::uint32_t request_size,
                                    ClientMsgFlag flags,
                                    std::chrono::milliseconds timeout_ms,
                                    std::uint32_t reply_size, uint64_t seq_num,
                                    const std::string correlation_id) {
  uint32_t replyBufSize =
      externalReplyBufferSize ? externalReplyBufferSize : reply_->size();
  char* replyBuffer =
      externalReplyBuffer ? externalReplyBuffer : reply_->data();

  auto res = client_->sendRequest(flags, static_cast<const char*>(request),
                                  request_size, seq_num, timeout_ms.count(),
                                  reply_size, replyBuffer, replyBufSize,
                                  correlation_id);

  if (res == bftEngine::OperationResult::TIMEOUT)
    LOG_ERROR(logger_, "reqSeqNum=" << seq_num << " cid=" << correlation_id
                                    << " has failed to invoke, timeout="
                                    << timeout_ms.count() << " ms has reached");
  else if (res == bftEngine::OperationResult::BUFFER_TOO_SMALL)
    LOG_ERROR(logger_, "reqSeqNum=" << seq_num << " cid=" << correlation_id
                                    << " has failed to invoke, buffer size="
                                    << reply_size << " is too small");
  else if (res == bftEngine::OperationResult::NOT_READY)
    LOG_ERROR(logger_,
              "reqSeqNum=" << seq_num << " cid=" << correlation_id
                           << " has failed to invoke, replicas not ready");
  return replyBufSize;
}

void ConcordClient::CreateCommConfig(
    CommConfig& comm_config, const ConcordConfiguration& config,
    int num_replicas, const ClientPoolConfig& pool_config) const {
  const auto& nodes = config.subscope(pool_config.PARTICIPANT_NODES, 0);
  const auto& node = nodes.subscope(pool_config.PARTICIPANT_NODE, 0);
  const auto& external_clients_conf =
      node.subscope(pool_config.EXTERNAL_CLIENTS, client_id_);
  const auto& client_conf =
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
                                 ClientPoolConfig& pool_config,
                                 const SimpleClientParams& client_params) {
  const auto num_replicas =
      config.getValue<std::uint16_t>(pool_config.NUM_REPLICAS);
  const auto& nodes = config.subscope(pool_config.PARTICIPANT_NODES, 0);
  const auto& node = nodes.subscope(pool_config.PARTICIPANT_NODE, 0);
  const auto& external_clients_conf =
      node.subscope(pool_config.EXTERNAL_CLIENTS, client_id_);
  const auto& client_conf =
      external_clients_conf.subscope(pool_config.CLIENT, 0);
  auto fVal = config.getValue<uint16_t>(pool_config.F_VAL);
  auto cVal = config.getValue<uint16_t>(pool_config.C_VAL);
  auto clientId = client_conf.getValue<uint16_t>(pool_config.CLIENT_ID);
  CommConfig comm_config;
  CreateCommConfig(comm_config, config, num_replicas, pool_config);
  client_id_ = clientId;
  for (auto i = 0u; i < num_replicas; ++i) {
    const auto& node_conf = config.subscope(pool_config.NODE_VAR, i);
    const auto& replica_conf = node_conf.subscope(pool_config.REPLICA_VAR, 0);
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
  LOG_DEBUG(logger_,
            "Client_id=" << client_id_ << " Creating communication module");
  auto comm = pool_config.ToCommunication(comm_config);
  LOG_DEBUG(
      logger_,
      "Client_id="
          << client_id_
          << " starting communication and creating simple client instance");
  comm_ = std::move(comm);
  comm_->Start();
  auto client = std::unique_ptr<bftEngine::SimpleClient>{
      bftEngine::SimpleClient::createSimpleClient(comm_.get(), clientId, fVal,
                                                  cVal, client_params)};

  seqGen_ = bftEngine::SeqNumberGeneratorForClientRequests::
      createSeqNumberGeneratorForClientRequests();
  client_ = std::move(client);
  LOG_INFO(logger_, "client_id=" << client_id_ << " creation succeeded");
}

int ConcordClient::getClientId() const { return client_id_; }

uint64_t ConcordClient::getClientSeqNum() const { return seq_num_; }

void ConcordClient::generateClientSeqNum() {
  seq_num_ = seqGen_->generateUniqueSequenceNumberForRequest();
}
void ConcordClient::setStartRequestTime() {
  start_job_time_ = std::chrono::steady_clock::now();
}

std::chrono::steady_clock::time_point ConcordClient::getStartRequestTime()
    const {
  return start_job_time_;
}

bool ConcordClient::isServing() const {
  int connected = 0;
  for (int i = 0; i < num_of_replicas_; i++) {
    if (comm_->getCurrentConnectionStatus(i) == ConnectionStatus::Connected)
      connected++;
    if (connected == required_num_of_replicas_) return true;
  }
  return false;
}

void ConcordClient::setStatics(uint16_t required_num_of_replicas,
                               uint16_t num_of_replicas,
                               uint32_t max_reply_size) {
  ConcordClient::reply_->resize(max_reply_size);
  ConcordClient::required_num_of_replicas_ = required_num_of_replicas;
  ConcordClient::num_of_replicas_ = num_of_replicas;
}

void ConcordClient::setReplyBuffer(char* buf, uint32_t size) {
  externalReplyBuffer = buf;
  externalReplyBufferSize = size;
}

}  // namespace concord::external_client
