//
// Created by lgal on 4/19/20.
//
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
const std::string CONFIG_LABEL = "concord_external_client";
const std::string F_VAL_VAR = "f_val";
const std::string C_VAL_VAR = "c_val";
const std::string NUM_REPLICAS_VAR = "num_replicas";
const std::string NUM_EXTERN_VAR = "num_of_external_clients";
const int MAX_EXTERN_VAR = 4096;
const std::string COMM_USE_VAR = "comm_to_use";
const std::string CERT_FOLDER_VAR = "tls_certificates_folder_path";
const std::string CIPHER_SUITE_VAR = "tls_cipher_suite_list";
const std::string COMM_BUFF_LEN_VAR = "concord-bft_communication_buffer_length";
const std::string INIT_RETRY_VAR = "client_initial_retry_timeout_milli";
const std::string MIN_RETRY_VAR = "client_min_retry_timeout_milli";
const std::string MAX_RETRY_VAR = "client_max_retry_timeout_milli";
const std::string FIRST_THRESH_VAR =
    "client_sends_request_to_all_replicas_first_thresh";
const std::string PERIODIC_THRESH_VAR =
    "client_sends_request_to_all_replicas_period_thresh";
const std::string RESET_THRESH_VAR = "client_periodic_reset_thresh";
const std::string NODE_VAR = "node";
const std::string REPLICA_VAR = "replica";
const std::string ID_VAR = "principal_id";
const std::string REPLICA_HOST_VAR = "replica_host";
const std::string REPLICA_PORT_VAR = "replica_port";
const std::string EXTERNAL_CLIENTS_VAR = "external_clients";
const std::string CLIENT_VAR = "client";
const std::string CLIENT_PORT_VAR = "client_port";
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