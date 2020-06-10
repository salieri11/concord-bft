// Copyright 2019 VMware, all rights reserved
//
// Concord configuration generation utility.
// This executable takes a YAML input file containing at least fundamental
// cluster size parameters and network configurations and optionally any
// non-default elections for optional parameters and outputs one YAML file per
// Concord node in the requested cluster containing the configuration for each
// replica (the --help option includes details on how to give the filename for
// the input and output to this utility).

#include <iostream>

#include <log4cplus/configurator.h>
#include <boost/program_options.hpp>
#include "Logger.hpp"
#include "config/configuration_manager.hpp"

using std::exception;
using std::ofstream;
using std::string;

using boost::program_options::command_line_parser;
using boost::program_options::options_description;
using boost::program_options::variables_map;

using concord::config::ConcordConfiguration;
using concord::config::ConfigurationResourceNotFoundException;
using concord::config::kConcGenconfigStateLabel;
using concord::config::YAMLConfigurationInput;
using concord::config::YAMLConfigurationOutput;

namespace po = boost::program_options;

void defineOptionsSpec(string& inputFilename, string& outputPrefix,
                       string& nodeMapFilename, bool& clientFlag,
                       options_description& optionsSpec) {
  // clang-format off
  optionsSpec.add_options()
      ("help,h", "Display help text and exit.")
      ("configuration-input", po::value<string>(&inputFilename),
       "Path to a YAML file containing input to the configuration generation "
       "utility, which includes cluster dimensions, network configuration, and "
       "any non-default value elections for optional parameters.")
      ("output-name", po::value<string>(&outputPrefix)->default_value("concord"),
       "Prefix to use as the base of the filename for the output configuration "
       "files. The output files will have names of the format "
       "<output-name><i>.config. For example, if output-name is \"concord\" and "
       "configuration is generated for a 4-node cluster, the output "
       "configuration files will be named concord1.config, concord2.config, "
       "concord3.config, and concord4.config.")
      ("report-principal-locations", po::value<string>(&nodeMapFilename),
       "Output a mapping reporting which Concord-BFT principals (by principal "
       "ID) are on each Concord node in the configured cluster, in JSON. One "
       "string parameter is expected with this option when it is used, naming a "
       "file to output this JSON mapping to. Note that the node IDs (the names "
       "in the name-value pairs) are 1-indexed, and correspond directly to the "
       "sequential numbers appearing in the filenames of the generated "
       "configuration files. Note it is not guaranteed that the nodes in the "
       "JSON Object and principal IDs in each node's array of them will be in "
       "any particular order. This report-principal-locations option is intended "
       "primarily for use by software that automates the deployment of Concord.")
      ("client-conf", po::value<bool>(&clientFlag),
       " An optional flag that specifies if the current input file is intended for a client configuration.");

  // clang-format on
}

void configurationSpec(ConcordConfiguration& config, bool clientFlag) {
  if (clientFlag) {
    specifyExternalClientConfiguration(config);
  } else {
    specifyConfiguration(config);
  }
  config.setConfigurationStateLabel(kConcGenconfigStateLabel);
}

int initiateConfigurationParams(YAMLConfigurationInput& yamlInput,
                                ConcordConfiguration& config, bool clientFlag,
                                const logging::Logger& concGenconfigLogger) {
  try {
    loadClusterSizeParameters(yamlInput, config, clientFlag);
  } catch (ConfigurationResourceNotFoundException& e) {
    LOG_FATAL(concGenconfigLogger,
              "Failed to load required parameters from input.");
    return -1;
  }
  try {
    if (clientFlag)
      instantiateClientTemplatedConfiguration(yamlInput, config);
    else
      instantiateTemplatedConfiguration(yamlInput, config);
  } catch (ConfigurationResourceNotFoundException& e) {
    LOG_FATAL(concGenconfigLogger,
              "Failed to size configuration for the requested dimensions.");
    return -1;
  }

  try {
    loadConfigurationInputParameters(yamlInput, config);
  } catch (ConfigurationResourceNotFoundException& e) {
    LOG_FATAL(concGenconfigLogger, "Failed to load required input parameters.");
    return -1;
  }
  if (config.loadAllDefaults(false, false) !=
      ConcordConfiguration::ParameterStatus::VALID) {
    LOG_FATAL(concGenconfigLogger,
              "Failed to load default values for configuration "
              "parameters not included in input.");
    return -1;
  }
  if (!clientFlag) {
    try {
      LOG_INFO(concGenconfigLogger,
               "Beginning key generation for the requested cluster. "
               "Depending on the cluster size, this may take a while...");
      generateConfigurationKeys(config);
      LOG_INFO(concGenconfigLogger, "Key generation complete.");
    } catch (std::exception& e) {
      LOG_FATAL(concGenconfigLogger,
                "An exception occurred while attempting key generation for "
                "the requested configuration.");
      LOG_FATAL(concGenconfigLogger,
                "Exception message: " + std::string(e.what()));
      return -1;
    }
  }
  if (config.generateAll(true, false) !=
      ConcordConfiguration::ParameterStatus::VALID) {
    LOG_FATAL(concGenconfigLogger,
              "Failed to generate and load values for implicit and "
              "generated configuration parameters.");
    return -1;
  }
  return 0;
}

int valideConfig(ConcordConfiguration& config,
                 const logging::Logger& concGenconfigLogger) {
  if (!config.scopeIsInstantiated("node")) {
    LOG_FATAL(concGenconfigLogger,
              "conc_genconfig failed to determine the number of nodes "
              "for which configuration files should be output.");
    return -1;
  }

  if (config.validateAll(true, false) !=
      ConcordConfiguration::ParameterStatus::VALID) {
    LOG_FATAL(concGenconfigLogger,
              "conc_genconfig failed to verify the all parameters that "
              "will be written to the configuratioin files are valid.");
    return -1;
  }
  if (!hasAllParametersRequiredAtConfigurationGeneration(config)) {
    LOG_FATAL(concGenconfigLogger,
              "Parameters required for configuration files are missing.");
    return -1;
  }
  return 0;
}

int outputConfig(ConcordConfiguration& config,
                 const logging::Logger& concGenconfigLogger, bool clientFlag,
                 const string nodeMapFilename,
                 const variables_map& optionsInput, string& outputPrefix) {
  if (clientFlag) {
    size_t numNodes = config.getValue<uint16_t>("num_of_participant_nodes");
    for (size_t i = 0; i < numNodes; ++i) {
      std::string outputFilename =
          "Participant" + std::to_string(i) + ".config";
      std::ofstream fileOutput(outputFilename);
      YAMLConfigurationOutput yamlOutput(fileOutput);
      try {
        outputParticipantNodeConfiguration(config, yamlOutput, i);
      } catch (std::exception& e) {
        return -1;
      }
    }
  } else {
    size_t numNodes = config.scopeSize("node");
    for (size_t i = 0; i < numNodes; ++i) {
      std::string outputFilename =
          outputPrefix + std::to_string(i + 1) + ".config";
      std::ofstream fileOutput(outputFilename);
      YAMLConfigurationOutput yamlOutput(fileOutput);
      try {
        outputConcordNodeConfiguration(config, yamlOutput, i);
      } catch (std::exception& e) {
        LOG_FATAL(
            concGenconfigLogger,
            "An exception occurred while trying to write configuraiton file " +
                outputFilename + ".");
        LOG_FATAL(concGenconfigLogger,
                  "Exception message: " + std::string(e.what()));
        return -1;
      }
      LOG_INFO(concGenconfigLogger, "Configuration file " + outputFilename +
                                        " (" + std::to_string(i + 1) + " of " +
                                        std::to_string(numNodes) +
                                        ") written.");
    }

    if (optionsInput.count("report-principal-locations")) {
      ofstream nodeMapOutput(nodeMapFilename);
      try {
        outputPrincipalLocationsMappingJSON(config, nodeMapOutput);
      } catch (const exception& e) {
        LOG_FATAL(concGenconfigLogger,
                  "An exception occurred while trying to write principal "
                  "locations mapping. Exception message: " +
                      string(e.what()));
        return -1;
      }
    }
  }
  return 0;
}

int main(int argc, char** argv) {
  // Initialize the logger, as logging may be used by some subprocesses of this
  // utility. Note this configuration generation utility currently is not using
  // a logger configuration file for itself.
  log4cplus::initialize();
  log4cplus::BasicConfigurator loggerConfig;
  loggerConfig.configure();
  logging::Logger concGenconfigLogger =
      logging::getLogger("com.vmware.concord.conc_genconfig");

  std::string inputFilename;
  std::string outputPrefix;
  std::string nodeMapFilename;
  bool clientFlag = false;

  variables_map optionsInput;
  options_description optionsSpec;
  defineOptionsSpec(inputFilename, outputPrefix, nodeMapFilename, clientFlag,
                    optionsSpec);

  store(command_line_parser(argc, argv).options(optionsSpec).run(),
        optionsInput);
  if ((argc < 2) || optionsInput.count("help")) {
    std::cout << "conc_genconfig" << std::endl;
    std::cout << "Usage:" << std::endl;
    std::cout << "    conc_genconfig --configuration-input <PATH_TO_INPUT_FILE>"
              << std::endl;
    std::cout << optionsSpec << std::endl;
    return 0;
  }
  notify(optionsInput);
  LOG_INFO(concGenconfigLogger, "conc_genconfig launched.");

  if (optionsInput.count("configuration-input") < 1) {
    LOG_FATAL(
        concGenconfigLogger,
        "No input file specified. Please use --configuration-input options.");
    return -1;
  }
  std::ifstream fileInput(inputFilename);
  if (!(fileInput.is_open())) {
    LOG_FATAL(concGenconfigLogger, "Could not open specified input file: \"" +
                                       inputFilename + "\".");
    return -1;
  }

  YAMLConfigurationInput yamlInput(fileInput);
  try {
    yamlInput.parseInput();
  } catch (std::exception& e) {
    LOG_FATAL(
        concGenconfigLogger,
        "An exception occurred while trying to parse the input to "
        "conc_genconfig. This likely suggests the requested input file (" +
            inputFilename +
            ") is inexistent, unreadable, malformed, or otherwise unusable.");
    LOG_FATAL(concGenconfigLogger,
              "Exception message: " + std::string(e.what()));
    return -1;
  }
  ConcordConfiguration config;
  configurationSpec(config, clientFlag);
  if (initiateConfigurationParams(yamlInput, config, clientFlag,
                                  concGenconfigLogger) == -1)
    return -1;
  if (valideConfig(config, concGenconfigLogger) == -1) return -1;
  if (outputConfig(config, concGenconfigLogger, clientFlag, nodeMapFilename,
                   optionsInput, outputPrefix) == -1)
    return -1;
  LOG_INFO(concGenconfigLogger, "conc_genconfig completed successfully.");
  return 0;
}
