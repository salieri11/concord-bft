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

#include <config/communication.hpp>
#include <config/configuration_manager.hpp>
#include <string>
#include "KVBCInterfaces.h"
#include "communication/CommDefs.hpp"
#include "communication/CommFactory.hpp"
#include "communication/ICommunication.hpp"

namespace concord {

namespace config {
class ConcordConfiguration;
}  // namespace config
namespace kvbc {
class IClient;
}

namespace config_pool {
class ClientPoolConfig {
 public:
  const std::string FILE_NAME = "concord_external_client";
  const std::string F_VAL = "f_val";
  const std::string C_VAL = "c_val";
  const std::string NUM_REPLICAS = "num_replicas";
  const std::string PARTICIPANT_NODES = "participant_nodes";
  const std::string PARTICIPANT_NODE = "participant_node";
  const std::string NUM_EXTERNAL_CLIENTS = "clients_per_participant_node";
  const int MAX_EXTERNAL_CLIENTS = 4096;
  const std::string COMM_PROTOCOL = "comm_to_use";
  const std::string CERT_FOLDER = "tls_certificates_folder_path";
  const std::string CIPHER_SUITE = "tls_cipher_suite_list";
  const std::string COMM_BUFF_LEN = "concord-bft_communication_buffer_length";
  const std::string INITIAL_RETRY_TIMEOUT =
      "client_initial_retry_timeout_milli";
  const std::string MIN_RETRY_TIMEOUT = "client_min_retry_timeout_milli";
  const std::string MAX_RETRY_TIMEOUT = "client_max_retry_timeout_milli";
  const std::string FIRST_THRESH =
      "client_sends_request_to_all_replicas_first_thresh";
  const std::string PERIODIC_THRESH =
      "client_sends_request_to_all_replicas_period_thresh";
  const std::string RESET_THRESH = "client_periodic_reset_thresh";
  const std::string NODE_VAR = "node";
  const std::string REPLICA_VAR = "replica";
  const std::string CLIENT_ID = "principal_id";
  const std::string REPLICA_HOST = "replica_host";
  const std::string REPLICA_PORT = "replica_port";
  const std::string EXTERNAL_CLIENTS = "external_clients";
  const std::string CLIENT = "client";
  const std::string CLIENT_PORT = "client_port";
  const std::string PROMETHEUS_PORT = "prometheus_port";
  const std::string PROMETHEUS_HOST = "participant_node_host";

  ClientPoolConfig();

  void ParseConfig(std::istream& config_stream,
                   config::ConcordConfiguration& config);

  std::unique_ptr<bft::communication::ICommunication> ToCommunication(
      const config::CommConfig& comm_config);

 private:
  // Logger
  log4cplus::Logger logger_;
};

}  // namespace config_pool
}  // namespace concord