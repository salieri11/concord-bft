// Copyright 2020 VMware, all rights reserved

#include "external_client.hpp"
#include <string>
#include <utility>
#include "CommDefs.hpp"
#include "ICommunication.hpp"
#include "KVBCInterfaces.h"
#include "client_pool_config.hpp"
#include "config/communication.hpp"
#include "config/configuration_manager.hpp"

namespace concord {
namespace external_client {

using bftEngine::ClientMsgFlag;
using concord::kvbc::ClientConfig;
using concord::kvbc::IClient;
using config::CommConfig;
using config::ConcordConfiguration;

ConcordClient::ConcordClient(ConcordConfiguration const& config,
                             int const& client_id) {
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
                                     int const& num_replicas,
                                     int const& client_id) {
  const auto& external_clients_conf =
      config.subscope("external_clients", client_id);
  const auto client_conf = external_clients_conf.subscope("client", 0);
  comm_config.commType =
      config.getValue<decltype(comm_config.commType)>("comm_to_use");
  comm_config.listenIp =
      client_conf.getValue<decltype(comm_config.listenIp)>("client_host");
  comm_config.listenPort =
      client_conf.getValue<decltype(comm_config.listenPort)>("client_port");
  comm_config.bufferLength =
      config.getValue<decltype(comm_config.bufferLength)>(
          "concord-bft_communication_buffer_length");
  comm_config.selfId =
      client_conf.getValue<decltype(comm_config.selfId)>("principal_id");

  if (comm_config.commType == "tcp" || comm_config.commType == "tls") {
    comm_config.maxServerId = num_replicas - 1;
  }

  if (comm_config.commType == "tls") {
    comm_config.certificatesRootPath =
        config.getValue<decltype(comm_config.certificatesRootPath)>(
            "tls_certificates_folder_path");
    comm_config.cipherSuite =
        config.getValue<decltype(comm_config.cipherSuite)>(
            "tls_cipher_suite_list");
  }
  comm_config.statusCallback = nullptr;
}

void ConcordClient::CreateClient(ConcordConfiguration const& config,
                                 int const& client_id) {
  const auto num_replicas = config.getValue<std::uint16_t>("num_replicas");
  ClientConfig client_config;
  const auto& external_clients_conf =
      config.subscope("external_clients", client_id);
  const auto client_conf = external_clients_conf.subscope("client", 0);
  client_config.fVal = config.getValue<decltype(client_config.fVal)>("f_val");
  client_config.cVal = config.getValue<decltype(client_config.cVal)>("c_val");
  client_config.clientId =
      client_conf.getValue<decltype(client_config.clientId)>("principal_id");
  CommConfig comm_config;
  CreateCommConfig(comm_config, config, num_replicas, client_id);
  for (auto i = 0u; i < num_replicas; ++i) {
    const auto& node_conf = config.subscope("node", i);
    const auto replica_conf = node_conf.subscope("replica", 0);
    const auto replica_id =
        replica_conf.getValue<decltype(comm_config.nodes)::key_type>(
            "principal_id");
    NodeInfo node_info;
    node_info.host =
        replica_conf.getValue<decltype(node_info.host)>("replica_host");
    node_info.port =
        replica_conf.getValue<decltype(node_info.port)>("replica_port");
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
