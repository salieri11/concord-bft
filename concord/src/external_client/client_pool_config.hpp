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
#include "CommDefs.hpp"
#include "CommFactory.hpp"
#include "ICommunication.hpp"
#include "KVBCInterfaces.h"

namespace bftEngine {
class ICommunication;
}

namespace concord {

namespace config {
class ConcordConfiguration;
}
namespace kvbc {
class IClient;
}

namespace config_pool {

const std::string FILE_NAME = "concord_external_client";
const std::string F_VAL = "f_val";
const std::string C_VAL = "c_val";
const std::string NUM_REPLICAS = "num_replicas";
const std::string NUM_EXTERNAL_CLIENTS = "num_of_external_clients";
const int MAX_EXTERNAL_CLIENTS = 4096;
const std::string COMM_PROTOCOL = "comm_to_use";
const std::string CERT_FOLDER = "tls_certificates_folder_path";
const std::string CIPHER_SUITE = "tls_cipher_suite_list";
const std::string COMM_BUFF_LEN = "concord-bft_communication_buffer_length";
const std::string INITIAL_RETRY_TIMEOUT = "client_initial_retry_timeout_milli";
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

config::ConcordConfiguration::ParameterStatus ValidateNumClients(
    const std::string& value, const config::ConcordConfiguration& config,
    const config::ConfigurationPath& path, std::string* failure_message,
    void* state);

config::ConcordConfiguration::ParameterStatus ValidateNumReplicas(
    const std::string& value, const config::ConcordConfiguration& config,
    const config::ConfigurationPath& path, std::string* failure_message,
    void* state);

config::ConcordConfiguration::ParameterStatus sizeExternalClients(
    const config::ConcordConfiguration& config,
    const config::ConfigurationPath& path, size_t* output, void* state);

config::ConcordConfiguration::ParameterStatus ValidateTimeOutMilli(
    const std::string& value, const config::ConcordConfiguration& config,
    const config::ConfigurationPath& path, std::string* failure_message,
    void* state);

void SpecifyClientConfiguration(config::ConcordConfiguration& config);

void SpecifyGeneralConfiguration(config::ConcordConfiguration& config);

void SpecifyReplicaConfiguration(config::ConcordConfiguration& config);

void SpecifySimpleClientParams(config::ConcordConfiguration& config);

void ParseConfig(std::istream& config_stream,
                 config::ConcordConfiguration& config);

std::unique_ptr<bftEngine::ICommunication> ToCommunication(
    const config::CommConfig& comm_config);

}  // namespace config_pool
}  // namespace concord