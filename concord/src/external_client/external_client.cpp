// Copyright 2020 VMware, all rights reserved

#include "external_client.hpp"

namespace concord {
namespace external_client {

using bftEngine::ClientMsgFlag;
using concord::kvbc::ClientConfig;
using concord::kvbc::IClient;
using config::CommConfig;
using config::ConcordConfiguration;
using namespace config_pool;

ConcordClient::ConcordClient(ConcordConfiguration const& config,
                             int client_id) {
  CreateClient(config, client_id);
}

ConcordClient::~ConcordClient() noexcept {
  try {
    comm_->Stop();
  } catch (...) {
  }

  try {
    client_->stop();
  } catch (...) {
  }
}

void ConcordClient::SendRequest(const void* request, std::uint32_t request_size,
                                ClientMsgFlag flags,
                                std::chrono::milliseconds timeout_ms,
                                std::uint32_t reply_size, void* out_reply,
                                std::uint32_t* out_actual_reply_size,
                                const std::string& correlation_id) {
  const auto status = client_->invokeCommandSynch(
      static_cast<const char*>(request), request_size, flags, timeout_ms,
      reply_size, static_cast<char*>(out_reply), out_actual_reply_size,
      correlation_id);

  if (!status.isOK()) {
    throw ClientRequestException{
        status, "ConcordClient failed to send a synchronous request"};
  }
}

void ConcordClient::CreateCommConfig(CommConfig& comm_config,
                                     ConcordConfiguration const& config,
                                     int num_replicas, int client_id) {
  const auto& external_clients_conf =
      config.subscope(EXTERNAL_CLIENTS_VAR, client_id);
  const auto client_conf = external_clients_conf.subscope(CLIENT_VAR, 0);
  comm_config.commType =
      config.getValue<decltype(comm_config.commType)>(COMM_USE_VAR);
  comm_config.listenIp = "external_client";
  comm_config.listenPort =
      client_conf.getValue<decltype(comm_config.listenPort)>(CLIENT_PORT_VAR);
  comm_config.bufferLength =
      config.getValue<decltype(comm_config.bufferLength)>(COMM_BUFF_LEN_VAR);
  comm_config.selfId =
      client_conf.getValue<decltype(comm_config.selfId)>(ID_VAR);

  if (comm_config.commType == "tcp" || comm_config.commType == "tls") {
    comm_config.maxServerId = num_replicas - 1;
  }

  if (comm_config.commType == "tls") {
    comm_config.certificatesRootPath =
        config.getValue<decltype(comm_config.certificatesRootPath)>(
            CERT_FOLDER_VAR);
    comm_config.cipherSuite =
        config.getValue<decltype(comm_config.cipherSuite)>(CIPHER_SUITE_VAR);
  }
  comm_config.statusCallback = nullptr;
}

void ConcordClient::CreateClient(ConcordConfiguration const& config,
                                 int client_id) {
  const auto num_replicas = config.getValue<std::uint16_t>(NUM_REPLICAS_VAR);
  ClientConfig client_config;
  const auto& external_clients_conf =
      config.subscope(EXTERNAL_CLIENTS_VAR, client_id);
  const auto client_conf = external_clients_conf.subscope(CLIENT_VAR, 0);
  client_config.fVal = config.getValue<decltype(client_config.fVal)>(F_VAL_VAR);
  client_config.cVal = config.getValue<decltype(client_config.cVal)>(C_VAL_VAR);
  client_config.clientId =
      client_conf.getValue<decltype(client_config.clientId)>(ID_VAR);
  CommConfig comm_config;
  CreateCommConfig(comm_config, config, num_replicas, client_id);
  for (auto i = 0u; i < num_replicas; ++i) {
    const auto& node_conf = config.subscope(NODE_VAR, i);
    const auto replica_conf = node_conf.subscope(REPLICA_VAR, 0);
    const auto replica_id =
        replica_conf.getValue<decltype(comm_config.nodes)::key_type>(ID_VAR);
    NodeInfo node_info;
    node_info.host =
        replica_conf.getValue<decltype(node_info.host)>(REPLICA_HOST_VAR);
    node_info.port =
        replica_conf.getValue<decltype(node_info.port)>(REPLICA_PORT_VAR);
    node_info.isReplica = true;
    comm_config.nodes[replica_id] = node_info;
  }
  // Ensure exception safety by creating local pointers and only moving to
  // object members if construction and startup haven't thrown.
  auto comm = config_pool::ToCommunication(comm_config);
  auto client = std::unique_ptr<IClient>{
      concord::kvbc::createClient(client_config, comm.get())};
  client->start();
  comm_ = std::move(comm);
  client_ = std::move(client);
}

}  // namespace external_client
}  // namespace concord
