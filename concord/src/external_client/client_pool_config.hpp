//
// Created by lgal on 4/19/20.
//
#include <config/communication.hpp>
#include <config/configuration_manager.hpp>
#include <string>
#include <utility>
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

config::ConcordConfiguration::ParameterStatus ValidateNumClients(
    const std::string& value, const config::ConcordConfiguration& config,
    const config::ConfigurationPath& path, std::string* failure_message,
    void* state);

config::ConcordConfiguration::ParameterStatus ValidateNumReplicas(
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