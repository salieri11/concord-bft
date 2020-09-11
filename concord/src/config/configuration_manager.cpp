// Copyright 2018-2020 VMware, all rights reserved

#include <regex>

#include <cryptopp/dll.h>
#include <boost/algorithm/string.hpp>
#include <nlohmann/json.hpp>

#include "configuration_manager.hpp"

using std::cerr;
using std::endl;
using std::exception;
using std::invalid_argument;
using std::make_shared;
using std::make_unique;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::unordered_set;
using std::vector;

using boost::program_options::command_line_parser;
using boost::program_options::options_description;
using boost::program_options::variables_map;

using logging::Logger;

using nlohmann::json;

using concord::config::ConcordConfiguration;
using concord::config::detectLocalNode;
using concord::config::kConcordNodeConfigurationStateLabel;

YAML::Node yaml_merge(const YAML::Node& a, const YAML::Node& b,
                      std::set<std::string> errorOnConflict,
                      const YAML::Node& parent) {
  if (a.IsMap() && b.IsMap()) {
    auto c = YAML::Node(YAML::NodeType::Map);
    // Take every element from "a" and if no conflict with "b" insert it in
    // result -> "c"
    for (auto it = a.begin(); it != a.end(); it++) {
      if (!b[it->first.as<std::string>()]) {
        // No conflict so just add the key/value pair
        c[it->first] = it->second;
      } else {
        // We have conflict, so we need to merge what will be inserted in "c" at
        // position with same key in both "a" and "b"
        c[it->first] = yaml_merge(it->second, b[it->first.as<std::string>()],
                                  errorOnConflict, it->first);
      }
    }
    // Add elements from "b" that have no conflicts with elements in "a",
    // they have been resolved in the previous for loop
    for (auto it = b.begin(); it != b.end(); it++) {
      if (!c[it->first.as<std::string>()]) {
        c[it->first] = it->second;
      }
    }
    return c;

  } else if (a.IsSequence() && b.IsSequence()) {
    if (a.size() > b.size()) {
      std::stringstream errMsg;
      errMsg << "Cannot shrink a sequence! key=" << parent
             << " a.size=" << a.size() << "b.size=" << b.size();
      throw(std::runtime_error(errMsg.str()));
    }

    // In merging sequences we have Concord configuration specifics, because we
    // store in sequences the information for the current Replica as well as all
    // the public parameters of peer replicas. Basically, in Application
    // configuration we can have default cluster dimensions specified, and the
    // current node in the sequence under the "node" key is identified by having
    // a "current_node: true" element. In Deployment configuration we expect to
    // have in one of the elements of the sequence also "current_node: true",
    // and we need to merge those elements preserving in the result the index of
    // current node in Deployment configuration. If the sequences do not contain
    // "current_node: true" we merge each element from the first sequence with
    // its corresponding element at the same position in the second sequence,
    // resolving conflicts in them by taking with priority the value in the
    // second.

    // helper function to Check in which postion in a sequence current_node is
    auto getCurrentNodePosition = [](const YAML::Node& node) {
      auto count = 0;
      auto currentNodePosition = -1;
      for (auto it = node.begin(); it != node.end(); ++it, count++) {
        auto n = YAML::Node(*it);
        if (n["current_node"] &&
            n["current_node"].as<std::string>() == "true") {
          currentNodePosition = count;
          break;
        }
      }
      return currentNodePosition;
    };

    int currentNodePositionInA = getCurrentNodePosition(a);
    int currentNodePositionInB = getCurrentNodePosition(b);

    auto c = YAML::Node(YAML::NodeType::Sequence);
    if (currentNodePositionInA != -1 && currentNodePositionInB != -1 &&
        (currentNodePositionInB != currentNodePositionInA ||
         a.size() < b.size())) {
      int otherNodePositionInA = (currentNodePositionInA + 1) % a.size();
      int count = 0;
      for (auto it = b.begin(); it != b.end(); it++, count++) {
        if (currentNodePositionInB == count) {
          c[count] = yaml_merge(a[currentNodePositionInA], *it, errorOnConflict,
                                parent);
        } else {
          c[count] =
              yaml_merge(a[otherNodePositionInA], *it, errorOnConflict, parent);
        }
      }
    } else {
      int count = 0;
      auto itA = a.begin();
      auto itB = b.begin();
      for (; itA != a.end() && itB != b.end(); ++itA, ++itB, count++) {
        c[count] = yaml_merge(*itA, *itB, errorOnConflict, parent);
      }
      for (; itB != b.end(); ++itB, ++count) {
        c[count] = YAML::Node(*itB);
      }
    }
    // return merged sequence
    return c;
  } else if (a.IsScalar() && b.IsScalar()) {
    if (errorOnConflict.find(parent.as<std::string>()) !=
        errorOnConflict.end()) {
      // throw error if vallues differ
      if (a.as<std::string>() != b.as<std::string>()) {
        std::stringstream errMsg;
        errMsg << "Found conflicting values for key=" << parent
               << " with values \"" << a << "\" and \"" << b << "\"!";
        throw(std::runtime_error(errMsg.str()));
      }
      return b;
    } else {
      // we take "b" to override "a" in case of conflicts
      return b;
    }
  }
  return b;
}

bool initialize_config(int argc, char** argv, ConcordConfiguration& config_out,
                       variables_map& opts_out,
                       std::unique_ptr<Cryptosystem>& cryptosys_out) {
  // Holds the file name of path of the configuration file for Concordthis is
  // NOT same as logger configuration file. Logger configuration file can be
  // specified as a property in configuration file.
  string configFile;

  string applicationConfigFile;
  string deploymentConfigFile;
  string secretsConfigFile;

  // Program options which are generic for most of the programs:
  // These are not available via configuration files
  // only allowed to be passed on command line
  options_description generic{"Generic Options"};

  // clang-format off
  generic.add_options()
      ("help,h", "Print this help message")
      ("config,c",
       boost::program_options::value<string>(&configFile),
       "Path for entire configuration file. Deprecated, use separate files for app, depl and secrets instead")
      ("application_config,a",
       boost::program_options::value<string>(&applicationConfigFile),
       "Path for Application configuration file")
      ("deployment_config,d",
       boost::program_options::value<string>(&deploymentConfigFile),
       "Path for Deployment configuration file")
      ("secrets_config,s",
       boost::program_options::value<string>(&secretsConfigFile),
       "Path for Secrets configuration file")
      ("debug", "Sleep for 20 seconds to attach debug");
  // clang-format on

  // First we parse command line options and see if --help
  // options was provided. In this case we don't need to
  // go for parsing config file. Otherwise call notify
  // for command line options and move to parsing config file.
  store(command_line_parser(argc, argv).options(generic).run(), opts_out);

  // If cmdline options specified --help then we don't want
  // to do further processing for command line or
  // config file options
  if (opts_out.count("help")) {
    std::cout << "VMware Project Concord" << std::endl;
    std::cout << generic << std::endl;
    return true;
  }

  // call notify after checking "help", so that required
  // parameters are not required to get help (this call throws an
  // exception to exit the program if any parameters are invalid)
  notify(opts_out);

  auto checkConfigFile = [](std::ifstream& fileInput, std::string configName,
                            const std::string& fileName) {
    if (!fileInput.is_open()) {
      cerr << "Concord could not open " << configName
           << " configuration file: " << fileName << endl;
      return false;
    }
    if (fileInput.peek() == EOF) {
      cerr << "Concord " << configName << " configuration file " << fileName
           << " appears to be an empty file." << endl;
      return false;
    }
    return true;
  };

  if (opts_out.count("config")) {
    // Verify configuration file exists.
    std::ifstream fileInput(configFile);
    if (!checkConfigFile(fileInput, "monolithic", configFile)) {
      return false;
    }

    // Parse configuration file.
    concord::config::specifyConfiguration(config_out);
    config_out.setConfigurationStateLabel(kConcordNodeConfigurationStateLabel);
    concord::config::YAMLConfigurationInput input(fileInput);

    try {
      input.parseInput();
    } catch (std::exception& e) {
      std::cerr << "An exception occurred while trying to read the "
                   "configuration file "
                << configFile << ": exception message: " << e.what()
                << std::endl;
    }

    concord::config::loadNodeConfiguration(config_out, input);

  } else if (opts_out.count("application_config") &&
             opts_out.count("deployment_config") &&
             opts_out.count("secrets_config")) {
    // Verify configuration files exist.
    std::ifstream applicationInput(applicationConfigFile);
    std::ifstream deploymentInput(deploymentConfigFile);
    std::ifstream secretsInput(secretsConfigFile);
    if (checkConfigFile(applicationInput, "application",
                        applicationConfigFile) &&
        checkConfigFile(deploymentInput, "deployment", deploymentConfigFile) &&
        checkConfigFile(secretsInput, "secrets", secretsConfigFile)) {
      // Parse configuration file.
      concord::config::specifyConfiguration(config_out);
      config_out.setConfigurationStateLabel(
          kConcordNodeConfigurationStateLabel);

      YAML::Node yamlApplicationConfiguration;
      YAML::Node yamlDeploymentConfiguration;
      YAML::Node yamlSecretsConfiguration;

      auto loadYaml = [](YAML::Node& n, std::ifstream& fileInput,
                         const std::string& configFile) {
        try {
          n.reset(YAML::Load(fileInput));
        } catch (std::exception& e) {
          std::cerr << "An exception occurred while trying to read the "
                       "configuration file "
                    << configFile << ": exception message: " << e.what()
                    << std::endl;
          return false;
        }
        return true;
      };

      if (!(loadYaml(yamlApplicationConfiguration, applicationInput,
                     applicationConfigFile) &&
            loadYaml(yamlDeploymentConfiguration, deploymentInput,
                     deploymentConfigFile) &&
            loadYaml(yamlSecretsConfiguration, secretsInput,
                     secretsConfigFile))) {
        return false;
      }

      YAML::Node yamlMerged;

      yamlMerged = yaml_merge(yamlApplicationConfiguration,
                              yamlDeploymentConfiguration, {"principal_id"});
      yamlMerged =
          yaml_merge(yamlMerged, yamlSecretsConfiguration, {"principal_id"});

      concord::config::YAMLConfigurationInput input(YAML::Clone(yamlMerged));
      concord::config::loadNodeConfiguration(config_out, input);

    } else {
      return false;
    }
  } else {
    cerr << "Insufficient configuration provided!" << std::endl;
    return false;
  }

  bool isReadOnly;
  std::tie(std::ignore, isReadOnly) = detectLocalNode(config_out);

  if (!isReadOnly) {
    concord::config::loadSBFTCryptosystems(config_out);
  }

  concord::config::ConcordPrimaryConfigurationAuxiliaryState* auxState =
      dynamic_cast<concord::config::ConcordPrimaryConfigurationAuxiliaryState*>(
          config_out.getAuxiliaryState());

  cryptosys_out.reset(auxState->optimisticCommitCryptosys.release());
  return true;
}

namespace concord {
namespace config {

// Implementations of member functions for the core configuration library
// classes declared in concord/src/configuration_manager.hpp.

ConfigurationPath::ConfigurationPath()
    : isScope(false), useInstance(false), name(), index(0), subpath() {}

ConfigurationPath::ConfigurationPath(const string& name, bool isScope)
    : isScope(isScope), useInstance(false), name(name), index(0), subpath() {}

ConfigurationPath::ConfigurationPath(const string& name, size_t index)
    : isScope(true), useInstance(true), name(name), index(index), subpath() {}

ConfigurationPath::ConfigurationPath(const ConfigurationPath& other)
    : isScope(other.isScope),
      useInstance(other.useInstance),
      name(other.name),
      index(other.index),
      subpath() {
  if (other.subpath) {
    subpath.reset(new ConfigurationPath(*(other.subpath)));
  }
}

ConfigurationPath::~ConfigurationPath() {}

ConfigurationPath& ConfigurationPath::operator=(
    const ConfigurationPath& other) {
  isScope = other.isScope;
  useInstance = other.useInstance;
  name = other.name;
  index = other.index;
  if (other.subpath) {
    subpath.reset(new ConfigurationPath(*(other.subpath)));
  } else {
    subpath.reset();
  }
  return *this;
}

bool ConfigurationPath::operator==(const ConfigurationPath& other) const {
  bool equal = (isScope == other.isScope) && (name == other.name);
  if (equal && isScope) {
    equal = (useInstance == other.useInstance) &&
            (((bool)subpath) == ((bool)(other.subpath)));
    if (equal && useInstance) {
      equal = (index == other.index);
    }
    if (equal && subpath) {
      equal = (*subpath == *(other.subpath));
    }
  }
  return equal;
}

bool ConfigurationPath::operator!=(const ConfigurationPath& other) const {
  return !(*this == other);
}

string ConfigurationPath::toString() const {
  string str = name;
  if (isScope && useInstance) {
    str += "[" + to_string(index) + "]";
  }
  if (isScope && subpath) {
    str += "/" + subpath->toString();
  }
  return str;
}

bool ConfigurationPath::contains(const ConfigurationPath& other) const {
  if (!(this->isScope)) {
    return false;
  }
  if (!(other.isScope) || (name != other.name) ||
      (useInstance != other.useInstance) ||
      (useInstance && (index != other.index))) {
    return false;
  }
  if (this->subpath) {
    return (other.subpath) && (this->subpath->contains(*(other.subpath)));
  } else {
    return true;
  }
}

ConfigurationPath ConfigurationPath::concatenate(
    const ConfigurationPath& other) const {
  if (!isScope) {
    throw invalid_argument(
        "Attempting to concatenate a configuration path (" + other.toString() +
        ") to a non-scope ConfigurationPath (" + toString() + ").");
  } else {
    ConfigurationPath ret(name, isScope);
    if (isScope && useInstance) {
      ret.useInstance = true;
      ret.index = index;
    }
    if (subpath) {
      ret.subpath = std::unique_ptr<ConfigurationPath>(
          new ConfigurationPath(subpath->concatenate(other)));
    } else {
      ret.subpath =
          std::unique_ptr<ConfigurationPath>(new ConfigurationPath(other));
    }
    return ret;
  }
}

ConfigurationPath ConfigurationPath::getLeaf() const {
  if (!isScope || !subpath) {
    return ConfigurationPath(*this);
  } else {
    return subpath->getLeaf();
  }
}

ConfigurationPath ConfigurationPath::trimLeaf() const {
  ConfigurationPath ret(*this);
  ConfigurationPath* stemEnd = &ret;
  while ((stemEnd->isScope) && (stemEnd->subpath) &&
         (stemEnd->subpath->isScope) && (stemEnd->subpath->subpath)) {
    stemEnd = stemEnd->subpath.get();
  }
  stemEnd->subpath.reset();
  return ret;
}

ConcordConfiguration::ConfigurationScope::ConfigurationScope(
    const ConcordConfiguration::ConfigurationScope& original)
    : instantiated(original.instantiated),
      instances(original.instances),
      description(original.description),
      size(original.size) {
  if (original.instanceTemplate) {
    instanceTemplate.reset(
        new ConcordConfiguration(*(original.instanceTemplate)));
  }
}

ConcordConfiguration::ConfigurationScope&
ConcordConfiguration::ConfigurationScope::operator=(
    const ConcordConfiguration::ConfigurationScope& original) {
  instantiated = original.instantiated;
  instances = original.instances;
  description = original.description;
  size = original.size;
  if (original.instanceTemplate) {
    instanceTemplate.reset(
        new ConcordConfiguration(*(original.instanceTemplate)));
  }
  return *this;
}

ConcordConfiguration::ConfigurationParameter::ConfigurationParameter(
    const ConcordConfiguration::ConfigurationParameter& other)
    : description(other.description),
      hasDefaultValue(other.hasDefaultValue),
      defaultValue(other.defaultValue),
      initialized(other.initialized),
      value(other.value),
      tags(other.tags),
      validator(other.validator),
      generator(other.generator) {}

ConcordConfiguration::ConfigurationParameter&
ConcordConfiguration::ConfigurationParameter::operator=(
    const ConcordConfiguration::ConfigurationParameter& other) {
  description = other.description, hasDefaultValue = other.hasDefaultValue;
  defaultValue = other.defaultValue;
  initialized = other.initialized;
  value = other.value;
  tags = other.tags;
  validator = other.validator;
  generator = other.generator;
  return *this;
}

void ConcordConfiguration::invalidateIterators() {
  for (ConcordConfiguration::Iterator* iterator : iterators) {
    iterator->invalidate();
  }
  iterators.clear();
  if (parentScope) {
    parentScope->invalidateIterators();
  }
}

ConfigurationPath* ConcordConfiguration::getCompletePath(
    const ConfigurationPath& localPath) const {
  if (scopePath) {
    return new ConfigurationPath(scopePath->concatenate(localPath));
  } else {
    return new ConfigurationPath(localPath);
  }
}

string ConcordConfiguration::printCompletePath(
    const ConfigurationPath& localPath) const {
  std::unique_ptr<ConfigurationPath> completePath(getCompletePath(localPath));
  return completePath->toString();
}

string ConcordConfiguration::printCompletePath(
    const string& localParameter) const {
  std::unique_ptr<ConfigurationPath> completePath(
      getCompletePath(ConfigurationPath(localParameter, false)));
  return completePath->toString();
}

void ConcordConfiguration::updateSubscopePaths() {
  for (auto& scope : scopes) {
    ConfigurationPath templatePath(scope.first, true);
    scopes[scope.first].instanceTemplate->scopePath.reset(
        getCompletePath(templatePath));
    scopes[scope.first].instanceTemplate->updateSubscopePaths();
    vector<ConcordConfiguration>& instances = scopes[scope.first].instances;
    for (size_t i = 0; i < instances.size(); ++i) {
      ConfigurationPath instancePath(scope.first, i);
      instances[i].scopePath.reset(getCompletePath(instancePath));
      instances[i].updateSubscopePaths();
    }
  }
}

ConcordConfiguration::ConfigurationParameter&
ConcordConfiguration::getParameter(const string& parameter,
                                   const string& failureMessage) {
  return const_cast<ConfigurationParameter&>(
      (const_cast<const ConcordConfiguration*>(this))
          ->getParameter(parameter, failureMessage));
}

const ConcordConfiguration::ConfigurationParameter&
ConcordConfiguration::getParameter(const string& parameter,
                                   const string& failureMessage) const {
  if (!contains(parameter)) {
    ConfigurationPath path(parameter, false);
    throw ConfigurationResourceNotFoundException(
        failureMessage + printCompletePath(path) + ": parameter not found.");
  }
  return parameters.at(parameter);
}

const ConcordConfiguration& ConcordConfiguration::getRootConfig() const {
  const ConcordConfiguration* rootConfig = this;
  while (rootConfig->parentScope) {
    rootConfig = rootConfig->parentScope;
  }
  return *rootConfig;
}

template <>
bool ConcordConfiguration::interpretAs<short>(string value,
                                              short& output) const {
  int intVal;
  try {
    intVal = std::stoi(value);
  } catch (invalid_argument& e) {
    return false;
  } catch (std::out_of_range& e) {
    return false;
  }
  if ((intVal < SHRT_MIN) || (intVal > SHRT_MAX)) {
    return false;
  }
  output = static_cast<short>(intVal);
  return true;
}

template <>
string ConcordConfiguration::getTypeName<short>() const {
  return "short";
}

template <>
bool ConcordConfiguration::interpretAs<string>(string value,
                                               string& output) const {
  output = value;
  return true;
}

template <>
string ConcordConfiguration::getTypeName<string>() const {
  return "string";
}

template <>
bool ConcordConfiguration::interpretAs<uint16_t>(string value,
                                                 uint16_t& output) const {
  // This check is necessary because stoul/stoull actually have semantics for if
  // their input is preceded with a '-' other than throwing an exception.
  if ((value.length() > 0) && value[0] == '-') {
    return false;
  }

  unsigned long long intVal;
  try {
    intVal = std::stoull(value);
  } catch (invalid_argument& e) {
    return false;
  } catch (std::out_of_range& e) {
    return false;
  }
  if (intVal > UINT16_MAX) {
    return false;
  }
  output = static_cast<uint16_t>(intVal);
  return true;
}

template <>
string ConcordConfiguration::getTypeName<uint16_t>() const {
  return "uint16_t";
}

template <>
bool ConcordConfiguration::interpretAs<uint32_t>(string value,
                                                 uint32_t& output) const {
  // This check is necessary because stoul/stoull actually have semantics for if
  // their input is preceded with a '-' other than throwing an exception.
  if ((value.length() > 0) && value[0] == '-') {
    return false;
  }

  unsigned long long intVal;
  try {
    intVal = std::stoull(value);
  } catch (invalid_argument& e) {
    return false;
  } catch (std::out_of_range& e) {
    return false;
  }
  if (intVal > UINT32_MAX) {
    return false;
  }
  output = static_cast<uint32_t>(intVal);
  return true;
}

template <>
string ConcordConfiguration::getTypeName<uint32_t>() const {
  return "uint32_t";
}

template <>
bool ConcordConfiguration::interpretAs<uint64_t>(string value,
                                                 uint64_t& output) const {
  // This check is necessary because stoul/stoull actually have semantics for if
  // their input is preceded with a '-' other than throwing an exception.
  if ((value.length() > 0) && value[0] == '-') {
    return false;
  }

  unsigned long long intVal;
  try {
    intVal = std::stoull(value);
  } catch (invalid_argument& e) {
    return false;
  } catch (std::out_of_range& e) {
    return false;
  }
  if (intVal > UINT64_MAX) {
    return false;
  }
  output = static_cast<uint64_t>(intVal);
  return true;
}

template <>
string ConcordConfiguration::getTypeName<uint64_t>() const {
  return "uint64_t";
}

template <>
bool ConcordConfiguration::interpretAs<int32_t>(string value,
                                                int32_t& output) const {
  long long intVal;
  try {
    intVal = std::stoll(value);
  } catch (invalid_argument& e) {
    return false;
  } catch (std::out_of_range& e) {
    return false;
  }
  if ((intVal > INT32_MAX) || (intVal < INT32_MIN)) {
    return false;
  }
  output = static_cast<int32_t>(intVal);
  return true;
}

template <>
string ConcordConfiguration::getTypeName<int32_t>() const {
  return "int32_t";
}

static const vector<string> kValidBooleansTrue({"t", "T", "true", "True",
                                                "TRUE"});
static const vector<string> kValidBooleansFalse({"f", "F", "false", "False",
                                                 "FALSE"});

template <>
bool ConcordConfiguration::interpretAs<bool>(string value, bool& output) const {
  if (std::find(kValidBooleansTrue.begin(), kValidBooleansTrue.end(), value) !=
      kValidBooleansTrue.end()) {
    output = true;
    return true;
  } else if (std::find(kValidBooleansFalse.begin(), kValidBooleansFalse.end(),
                       value) != kValidBooleansFalse.end()) {
    output = false;
    return true;
  }

  return false;
}

template <>
string ConcordConfiguration::getTypeName<bool>() const {
  return "bool";
}

void ConcordConfiguration::ConfigurationIterator::updateRetVal() {
  if (currentParam != endParams) {
    retVal.name = (*currentParam).first;
    retVal.isScope = false;
    retVal.subpath.reset();
  } else if (currentScope != endScopes) {
    retVal.name = (*currentScope).first;
    retVal.isScope = true;
    if (usingInstance) {
      retVal.useInstance = true;
      retVal.index = instance;
    } else {
      retVal.useInstance = false;
    }
    if (currentScopeContents && endCurrentScope &&
        (*currentScopeContents != *endCurrentScope)) {
      retVal.subpath.reset(new ConfigurationPath(**currentScopeContents));
    } else {
      retVal.subpath.reset();
    }
  }
}

ConcordConfiguration::ConfigurationIterator::ConfigurationIterator()
    : recursive(false),
      scopes(false),
      parameters(false),
      instances(false),
      templates(false),
      config(nullptr),
      retVal(),
      currentScope(),
      endScopes(),
      usingInstance(false),
      instance(0),
      currentScopeContents(),
      endCurrentScope(),
      currentParam(),
      endParams(),
      invalid(false) {}

ConcordConfiguration::ConfigurationIterator::ConfigurationIterator(
    ConcordConfiguration& configuration, bool recursive, bool scopes,
    bool parameters, bool instances, bool templates, bool end)
    : recursive(recursive),
      scopes(scopes),
      parameters(parameters),
      instances(instances),
      templates(templates),
      config(&configuration),
      retVal(),
      endScopes(configuration.scopes.end()),
      usingInstance(false),
      instance(0),
      currentScopeContents(),
      endCurrentScope(),
      endParams(configuration.parameters.end()),
      invalid(false) {
  if (!end && parameters) {
    currentParam = configuration.parameters.begin();
  } else {
    currentParam = configuration.parameters.end();
  }
  if (!end && (recursive || scopes)) {
    currentScope = configuration.scopes.begin();

    // If the above initializations leave this iterator in a state where it is
    // pointing to a path that it cannot return, then we call ++ to advance it
    // to the first point where it can actually return something (if any).
    if ((currentParam == endParams) && (currentScope != endScopes) &&
        (!scopes || !templates)) {
      ++(*this);
    }
  } else {
    currentScope = configuration.scopes.end();
  }

  updateRetVal();
  configuration.registerIterator(this);
}

ConcordConfiguration::ConfigurationIterator::ConfigurationIterator(
    const ConcordConfiguration::ConfigurationIterator& original)
    : recursive(original.recursive),
      scopes(original.scopes),
      parameters(original.parameters),
      instances(original.instances),
      templates(original.templates),
      config(original.config),
      retVal(original.retVal),
      currentScope(original.currentScope),
      endScopes(original.endScopes),
      usingInstance(original.usingInstance),
      instance(original.instance),
      currentScopeContents(),
      endCurrentScope(),
      currentParam(original.currentParam),
      endParams(original.endParams),
      invalid(original.invalid) {
  if (original.currentScopeContents) {
    currentScopeContents.reset(new ConcordConfiguration::ConfigurationIterator(
        *(original.currentScopeContents)));
  }
  if (original.endCurrentScope) {
    endCurrentScope.reset(new ConcordConfiguration::ConfigurationIterator(
        *(original.endCurrentScope)));
  }

  if (config && !invalid) {
    config->registerIterator(this);
  }
}

ConcordConfiguration::ConfigurationIterator::~ConfigurationIterator() {
  if (config && !invalid) {
    config->deregisterIterator(this);
  }
  currentScopeContents.reset();
  endCurrentScope.reset();
}

ConcordConfiguration::ConfigurationIterator&
ConcordConfiguration::ConfigurationIterator::operator=(
    const ConcordConfiguration::ConfigurationIterator& original) {
  if (config && !invalid) {
    config->deregisterIterator(this);
  }

  recursive = original.recursive;
  scopes = original.scopes;
  parameters = original.parameters;
  instances = original.instances;
  templates = original.templates;
  config = original.config;
  retVal = original.retVal;
  currentScope = original.currentScope;
  endScopes = original.endScopes;
  usingInstance = original.usingInstance;
  instance = original.instance;
  if (original.currentScopeContents) {
    currentScopeContents.reset(new ConcordConfiguration::ConfigurationIterator(
        *(original.currentScopeContents)));
  } else {
    currentScopeContents.reset();
  }
  if (original.endCurrentScope) {
    endCurrentScope.reset(new ConcordConfiguration::ConfigurationIterator(
        *(original.endCurrentScope)));
  } else {
    endCurrentScope.reset();
  }
  currentParam = original.currentParam;
  endParams = original.endParams;
  invalid = original.invalid;

  if (config && !invalid) {
    config->registerIterator(this);
  }

  return *this;
}

bool ConcordConfiguration::ConfigurationIterator::operator==(
    const ConcordConfiguration::ConfigurationIterator& other) const {
  bool ret = (recursive == other.recursive) && (scopes == other.scopes) &&
             (parameters == other.parameters) &&
             (instances == other.instances) && (templates == other.templates) &&
             (config == other.config) && (currentScope == other.currentScope) &&
             (endScopes == other.endScopes) &&
             (currentParam == other.currentParam) &&
             (endParams == other.endParams) && (invalid == other.invalid);

  // Note we ignore scope-specific state if we are done with all the scopes and
  // we ignore instance-specific state if we are not currently using an
  // instance.
  if (ret && (currentScope != endScopes)) {
    ret = (usingInstance == other.usingInstance);
    if (ret && usingInstance) {
      ret = instance == other.instance;
    }
    if (ret) {
      if (!(currentScopeContents && other.currentScopeContents)) {
        ret = currentScopeContents == other.currentScopeContents;
      } else {
        ret = *currentScopeContents == *(other.currentScopeContents);
      }
    }
    if (ret) {
      if (!(endCurrentScope && other.endCurrentScope)) {
        ret = endCurrentScope == other.endCurrentScope;
      } else {
        ret = *endCurrentScope == *(other.endCurrentScope);
      }
    }
  }
  return ret;
}

bool ConcordConfiguration::ConfigurationIterator::operator!=(
    const ConcordConfiguration::ConfigurationIterator& other) const {
  return (!(*this == other));
}

const ConfigurationPath& ConcordConfiguration::ConfigurationIterator::
operator*() const {
  if (invalid) {
    throw InvalidIteratorException(
        "Attempting to use an iterator over a ConcordConfiguration that has "
        "been modified since the iterator's creation.");
  }
  if ((currentScope == endScopes) && (currentParam == endParams)) {
    // This iterator is either empty or pointing to the end of the configuration
    // if this case is reached.
    throw std::out_of_range(
        "Attempting to access value at iterator already at the end of a "
        "ConcordConfiguration.");
  }
  return retVal;
}

ConcordConfiguration::ConfigurationIterator&
ConcordConfiguration::ConfigurationIterator::operator++() {
  if (invalid) {
    throw InvalidIteratorException(
        "Attempting to use an iterator over a ConcordConfiguration that has "
        "been modified since the iterator's creation.");
  }
  if ((currentScope == endScopes) && (currentParam == endParams)) {
    throw std::out_of_range(
        "Attempting to advance an iterator already at the end of a "
        "ConcordConfiguration.");
  }

  bool hasVal = false;

  while (!hasVal &&
         ((currentScope != endScopes) || (currentParam != endParams))) {
    // Case where we have parameters we can return in the top-level scope.
    if (currentParam != endParams) {
      ++currentParam;
      hasVal = (currentParam != endParams) ||
               ((currentScope != endScopes) && (scopes && templates));

    } else if (currentScope != endScopes) {
      // Case where we continue iteration through a sub-scope of the
      // configuration by advancing a sub-iterator.
      if (currentScopeContents && endCurrentScope && recursive &&
          (*currentScopeContents != *endCurrentScope)) {
        ++(*currentScopeContents);
        hasVal = *currentScopeContents != *endCurrentScope;

        // Case where we have completed any non-recursive handling of a scope
        // itself and we procede to begin iterating through members of that
        // scope.
      } else if (recursive && (!currentScopeContents || !endCurrentScope) &&
                 ((templates && !usingInstance) ||
                  (instances && usingInstance))) {
        ConfigurationPath path = ConfigurationPath((*currentScope).first, true);
        if (usingInstance) {
          path.useInstance = true;
          path.index = instance;
        }
        ConcordConfiguration& scope = config->subscope(path);
        currentScopeContents.reset(new ConfigurationIterator(
            scope, recursive, scopes, parameters, instances, templates, false));
        endCurrentScope.reset(new ConfigurationIterator(
            scope, recursive, scopes, parameters, instances, templates, true));
        hasVal = currentScopeContents && endCurrentScope &&
                 (*currentScopeContents != *endCurrentScope);

        // Case where we have completed handling a specific instance of a scope
        // and we advance to the next instance.
      } else if (instances && usingInstance &&
                 (instance < ((*currentScope).second.instances.size() - 1))) {
        ++instance;
        currentScopeContents.reset();
        endCurrentScope.reset();
        hasVal = scopes;

        // Case where we have completed handling all relevant contents of a
        // specific scope and can move to the next scope (if any).
      } else if (!instances || (currentScope->second.instances.size() < 1) ||
                 (usingInstance &&
                  (instance >= (currentScope->second.instances.size() - 1)))) {
        usingInstance = false;
        instance = 0;
        currentScopeContents.reset();
        endCurrentScope.reset();
        ++currentScope;
        usingInstance = false;
        if (currentScope == endScopes) {
          hasVal = (currentParam != endParams);
        } else {
          hasVal = scopes && templates;
        }

        // Case where we have completed any template handling for a specific
        // scope and should begin handling instances of that scope.
      } else if (instances && !usingInstance &&
                 (currentScope->second.instances.size() > 0)) {
        usingInstance = true;
        instance = 0;
        currentScopeContents.reset();
        endCurrentScope.reset();
        hasVal = ((*currentScope).second.instances.size() > instance) && scopes;

        // The following cases should not be reached unless
        // ConfigurationIterator's implementation is buggy.
      } else {
        throw InvalidIteratorException(
            "ConcordConfiguration::ConfigurationIterator is implemented "
            "incorrectly: an iterator could not determine how to advance "
            "itself.");
      }

    } else {
      throw InvalidIteratorException(
          "ConcordConfiguration::ConfigurationIterator is implemented "
          "incorrectly: an iterator could not determine how to advance "
          "itself.");
    }
  }

  updateRetVal();

  return *this;
}

ConcordConfiguration::ConfigurationIterator
ConcordConfiguration::ConfigurationIterator::operator++(int) {
  ConfigurationIterator ret(*this);
  ++(*this);
  return ret;
}

void ConcordConfiguration::ConfigurationIterator::invalidate() {
  invalid = true;
}

void ConcordConfiguration::registerIterator(
    ConcordConfiguration::ConfigurationIterator* iterator) {
  iterators.insert(iterator);
}

void ConcordConfiguration::deregisterIterator(
    ConcordConfiguration::ConfigurationIterator* iterator) {
  iterators.erase(iterator);
}

ConcordConfiguration::ConcordConfiguration()
    : auxiliaryState(),
      configurationState(),
      parentScope(),
      scopePath(),
      scopes(),
      parameters(),
      iterators() {}

ConcordConfiguration::ConcordConfiguration(const ConcordConfiguration& original)
    : auxiliaryState(),
      configurationState(original.configurationState),
      parentScope(original.parentScope),
      scopePath(),
      scopes(original.scopes),
      parameters(original.parameters),
      iterators() {
  if (original.auxiliaryState) {
    auxiliaryState.reset(original.auxiliaryState->clone());
  }
  if (original.scopePath) {
    scopePath.reset(new ConfigurationPath(*(original.scopePath)));
  }
  for (auto& scopeEntry : scopes) {
    scopes[scopeEntry.first].instanceTemplate->parentScope = this;
    for (auto& instance : scopes[scopeEntry.first].instances) {
      instance.parentScope = this;
    }
  }
  confType_ = original.confType_;
}

ConcordConfiguration::~ConcordConfiguration() {
  auxiliaryState.reset();
  invalidateIterators();
  scopes.clear();
  parameters.clear();
  configurationState = "";
}

ConcordConfiguration& ConcordConfiguration::operator=(
    const ConcordConfiguration& original) {
  invalidateIterators();

  configurationState = original.configurationState;
  parentScope = original.parentScope;
  if (original.auxiliaryState) {
    auxiliaryState.reset(original.auxiliaryState->clone());
  } else {
    auxiliaryState.reset();
  }
  if (original.scopePath) {
    scopePath.reset(new ConfigurationPath(*(original.scopePath)));
  } else {
    scopePath.reset();
  }
  parameters = original.parameters;
  scopes = original.scopes;

  for (auto& scopeEntry : scopes) {
    scopes[scopeEntry.first].instanceTemplate->parentScope = this;
    for (auto& instance : scopes[scopeEntry.first].instances) {
      instance.parentScope = this;
    }
  }
  return *this;
}

void ConcordConfiguration::clear() {
  configurationState = "";
  auxiliaryState.reset();
  invalidateIterators();
  scopes.clear();
  parameters.clear();
}

void ConcordConfiguration::setAuxiliaryState(
    ConfigurationAuxiliaryState* auxState) {
  auxiliaryState.reset(auxState);
}

ConfigurationAuxiliaryState* ConcordConfiguration::getAuxiliaryState() {
  return auxiliaryState.get();
}

const ConfigurationAuxiliaryState* ConcordConfiguration::getAuxiliaryState()
    const {
  return auxiliaryState.get();
}

void ConcordConfiguration::setConfigurationStateLabel(const string& state) {
  configurationState = state;
}

string ConcordConfiguration::getConfigurationStateLabel() const {
  return configurationState;
}

void ConcordConfiguration::declareScope(const string& scope,
                                        const string& description,
                                        shared_ptr<ScopeSizer> size) {
  ConfigurationPath requestedScope(scope, true);
  if (scope.size() < 1) {
    throw invalid_argument(
        "Unable to create configuration scope: the empty string is not a valid "
        "name for a configuration scope.");
  }
  assert(kYAMLScopeTemplateSuffix.length() > 0);
  if ((scope.length() >= kYAMLScopeTemplateSuffix.length()) &&
      ((scope.substr(scope.length() - kYAMLScopeTemplateSuffix.length())) ==
       kYAMLScopeTemplateSuffix)) {
    throw invalid_argument("Cannot declare scope " + scope +
                           ": to facilitate configuration serialization, "
                           "scope names ending in \"" +
                           kYAMLScopeTemplateSuffix + "\" are disallowed.");
  }
  if (containsScope(scope)) {
    throw ConfigurationRedefinitionException(
        "Unable to create configuration scope " +
        printCompletePath(requestedScope) + ": scope already exists.");
  }
  if (contains(scope)) {
    throw ConfigurationRedefinitionException(
        "Unable to create configuration scope " +
        printCompletePath(requestedScope) + ": identifier " + scope +
        " is already used for a parameter.");
  }
  if (!size) {
    throw invalid_argument("Unable to create configuration scope " +
                           printCompletePath(requestedScope) +
                           ": provided scope sizer object is null.");
  }
  invalidateIterators();
  scopes[scope] = ConfigurationScope();
  scopes[scope].instanceTemplate.reset(new ConcordConfiguration());
  scopes[scope].instanceTemplate->confType_ = confType_;
  scopes[scope].instantiated = false;
  scopes[scope].description = description;
  scopes[scope].size = size;

  scopes[scope].instanceTemplate->parentScope = this;
  ConfigurationPath relativeScopePath(scope, true);
  ConfigurationPath* path;
  if (scopePath) {
    path = new ConfigurationPath(scopePath->concatenate(relativeScopePath));
  } else {
    path = new ConfigurationPath(relativeScopePath);
  }
  scopes[scope].instanceTemplate->scopePath.reset(path);
}

string ConcordConfiguration::getScopeDescription(const string& scope) const {
  if (!containsScope(scope)) {
    ConfigurationPath path(scope, true);
    throw ConfigurationResourceNotFoundException(
        "Cannot get description for scope " + printCompletePath(path) +
        ": scope does not exist.");
  }
  return scopes.at(scope).description;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::instantiateScope(
    const string& scope) {
  ConfigurationPath relativePath(scope, true);
  if (!containsScope(scope)) {
    throw ConfigurationResourceNotFoundException(
        "Unable to instantiate configuration scope " +
        printCompletePath(relativePath) + ": scope does not exist.");
  }
  ConfigurationScope& scopeEntry = scopes[scope];
  if (!(scopeEntry.size)) {
    throw invalid_argument("Unable to instantiate configuration scope " +
                           printCompletePath(relativePath) +
                           ": scope does not have a size function.");
  }
  std::unique_ptr<ConfigurationPath> fullPath(getCompletePath(relativePath));
  size_t scopeSize{};
  ParameterStatus result =
      scopeEntry.size->sizeScope(getRootConfig(), *fullPath, scopeSize);
  if (result != ParameterStatus::VALID) {
    return result;
  }
  invalidateIterators();
  scopeEntry.instantiated = true;
  scopeEntry.instances.clear();
  for (size_t i = 0; i < scopeSize; ++i) {
    scopeEntry.instances.push_back(
        ConcordConfiguration(*(scopeEntry.instanceTemplate)));
    ConfigurationPath instancePath(scope, i);
    scopeEntry.instances[i].scopePath.reset(getCompletePath(instancePath));
    scopeEntry.instances[i].updateSubscopePaths();
  }
  scopeEntry.instantiated = true;
  return result;
}

ConcordConfiguration& ConcordConfiguration::subscope(const string& scope) {
  // This cast avoids duplicating code between the const and non-const versions
  // of this function.
  return const_cast<ConcordConfiguration&>(
      (const_cast<const ConcordConfiguration*>(this))->subscope(scope));
}

const ConcordConfiguration& ConcordConfiguration::subscope(
    const string& scope) const {
  if (!containsScope(scope)) {
    ConfigurationPath path(scope, true);
    throw ConfigurationResourceNotFoundException("Could not find scope " +
                                                 printCompletePath(path) + ".");
  }
  return *(scopes.at(scope).instanceTemplate);
}

ConcordConfiguration& ConcordConfiguration::subscope(const string& scope,
                                                     size_t index) {
  // This cast avoids duplicating code between the const and non-const versions
  // of this function.
  return const_cast<ConcordConfiguration&>(
      (const_cast<const ConcordConfiguration*>(this))->subscope(scope, index));
}

const ConcordConfiguration& ConcordConfiguration::subscope(const string& scope,
                                                           size_t index) const {
  if ((scopes.count(scope) < 1) || (!(scopes.at(scope).instantiated)) ||
      (index >= scopes.at(scope).instances.size())) {
    ConfigurationPath path(scope, index);
    throw ConfigurationResourceNotFoundException("Could not find scope " +
                                                 printCompletePath(path) + ".");
  }
  return scopes.at(scope).instances[index];
}

ConcordConfiguration& ConcordConfiguration::subscope(
    const ConfigurationPath& path) {
  // This cast avoids duplicating code between the const and non-const versions
  // of this function.
  return const_cast<ConcordConfiguration&>(
      (const_cast<const ConcordConfiguration*>(this))->subscope(path));
}

const ConcordConfiguration& ConcordConfiguration::subscope(
    const ConfigurationPath& path) const {
  if (!path.isScope || (scopes.count(path.name) < 1) ||
      (path.useInstance &&
       ((!(scopes.at(path.name).instantiated)) ||
        (path.index >= scopes.at(path.name).instances.size())))) {
    throw ConfigurationResourceNotFoundException("Could not find scope " +
                                                 printCompletePath(path) + ".");
  }

  const ConcordConfiguration* subscope =
      scopes.at(path.name).instanceTemplate.get();
  if (path.useInstance) {
    subscope = &(scopes.at(path.name).instances[path.index]);
  }
  if (path.subpath) {
    return subscope->subscope(*(path.subpath));
  } else {
    return *subscope;
  }
}

bool ConcordConfiguration::containsScope(const string& name) const {
  return (scopes.count(name) > 0);
}

bool ConcordConfiguration::containsScope(const ConfigurationPath& path) const {
  if (!path.isScope || (scopes.count(path.name) < 1) ||
      (path.useInstance &&
       ((!(scopes.at(path.name).instantiated)) ||
        (path.index >= scopes.at(path.name).instances.size())))) {
    return false;
  }
  if (path.subpath) {
    ConcordConfiguration& subscope = *(scopes.at(path.name).instanceTemplate);
    if (path.useInstance) {
      subscope = scopes.at(path.name).instances[path.index];
    }
    return subscope.containsScope(*(path.subpath));
  } else {
    return true;
  }
}

bool ConcordConfiguration::scopeIsInstantiated(const string& name) const {
  return containsScope(name) && scopes.at(name).instantiated;
}

size_t ConcordConfiguration::scopeSize(const string& scope) const {
  if (!scopeIsInstantiated(scope)) {
    ConfigurationPath path(scope, true);
    throw ConfigurationResourceNotFoundException("Cannot get size of scope " +
                                                 printCompletePath(path) +
                                                 ": scope does not exist.");
  }
  return scopes.at(scope).instances.size();
}

void ConcordConfiguration::declareParameter(const string& name,
                                            const string& description) {
  if (name.size() < 1) {
    throw invalid_argument(
        "Cannot declare parameter: the empty string is not a valid name for a "
        "configuration parameter.");
  }
  assert(kYAMLScopeTemplateSuffix.length() > 0);
  if ((name.length() >= kYAMLScopeTemplateSuffix.length()) &&
      ((name.substr(name.length() - kYAMLScopeTemplateSuffix.length())) ==
       kYAMLScopeTemplateSuffix)) {
    throw invalid_argument("Cannot declare parameter " + name +
                           ": to facilitate configuration serialization, "
                           "parameter names ending in \"" +
                           kYAMLScopeTemplateSuffix + "\" are disallowed.");
  }
  if (contains(name)) {
    ConfigurationPath path(name, false);
    throw ConfigurationRedefinitionException("Cannot declare parameter " +
                                             printCompletePath(path) +
                                             ": parameter already exists.");
  }
  if (containsScope(name)) {
    ConfigurationPath path(name, false);
    throw ConfigurationRedefinitionException(
        "Cannot declare parameter " + printCompletePath(path) +
        ": identifier is already used for a scope.");
  }

  invalidateIterators();
  parameters[name] = ConfigurationParameter();
  ConfigurationParameter& parameter = parameters[name];
  parameter.description = description;
  parameter.hasDefaultValue = false;
  parameter.defaultValue = "";
  parameter.initialized = false;
  parameter.value = "";
  parameter.tags = std::unordered_set<string>();
  parameter.validator = shared_ptr<ParameterValidator>(nullptr);
  parameter.generator = shared_ptr<ParameterGenerator>(nullptr);
}

void ConcordConfiguration::declareParameter(const string& name,
                                            const string& description,
                                            const string& defaultValue) {
  declareParameter(name, description);
  ConfigurationParameter& parameter = parameters[name];
  parameter.defaultValue = defaultValue;
  parameter.hasDefaultValue = true;
}

void ConcordConfiguration::tagParameter(const string& name,
                                        const vector<string>& tags) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot tag parameter ");
  for (auto&& tag : tags) {
    parameter.tags.emplace(tag);
  }
}

bool ConcordConfiguration::isTagged(const string& name,
                                    const string& tag) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Cannot check tags for parameter ");
  return parameter.tags.count(tag) > 0;
}

string ConcordConfiguration::getDescription(const string& name) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Cannot get description for parameter ");
  return parameter.description;
}

void ConcordConfiguration::addValidator(
    const string& name,
    shared_ptr<ConcordConfiguration::ParameterValidator> validator) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot add validator to parameter ");
  if (!validator) {
    throw invalid_argument("Cannot add validator to parameter " +
                           printCompletePath(ConfigurationPath(name, false)) +
                           ": validator given points to null.");
  }
  parameter.validator = validator;
}

void ConcordConfiguration::addGenerator(
    const string& name,
    shared_ptr<ConcordConfiguration::ParameterGenerator> generator) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot add generator to parameter ");
  if (!generator) {
    throw invalid_argument("Cannot add generator to parameter " +
                           printCompletePath(ConfigurationPath(name, false)) +
                           ": generator given points to null.");
  }
  parameter.generator = generator;
}

bool ConcordConfiguration::contains(const string& name) const {
  return parameters.count(name) > 0;
}

bool ConcordConfiguration::contains(const ConfigurationPath& path) const {
  if (path.isScope && path.subpath) {
    if (scopes.count(path.name) < 1) {
      return false;
    }
    const ConfigurationScope& scope = scopes.at(path.name);
    const ConcordConfiguration* subscope = scope.instanceTemplate.get();
    if (path.useInstance) {
      if (!(scope.instantiated) || (path.index >= scope.instances.size())) {
        return false;
      } else {
        subscope = &(scope.instances[path.index]);
      }
    }
    return subscope->contains(*(path.subpath));
  } else {
    return !(path.isScope) && contains(path.name);
  }
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::loadValue(
    const string& name, const string& value, string* failureMessage,
    bool overwrite) {
  ConfigurationParameter& parameter =
      getParameter(name, "Could not load value for parameter ");
  std::unique_ptr<ConfigurationPath> path(
      getCompletePath(ConfigurationPath(name, false)));
  ParameterStatus status = ParameterStatus::VALID;
  string message;
  if (parameter.validator) {
    status =
        parameter.validator->validate(value, getRootConfig(), *path, message);
  }
  if (failureMessage && (status != ParameterStatus::VALID)) {
    *failureMessage = message;
  }

  if (status != ParameterStatus::INVALID) {
    if (parameter.initialized) {
      if (overwrite) {
        parameter.value = value;
      }
    } else {
      parameter.value = value;
      parameter.initialized = true;
    }
  }

  return status;
}

void ConcordConfiguration::eraseValue(const string& name) {
  ConfigurationParameter& parameter =
      getParameter(name, "Could not erase value for parameter ");
  parameter.value = "";
  parameter.initialized = false;
}

void ConcordConfiguration::eraseAllValues() {
  auto iterator = this->begin(kIterateAllParameters);
  auto end = this->end(kIterateAllParameters);
  while (iterator != end) {
    const ConfigurationPath& path = *iterator;
    ConcordConfiguration* containingScope = this;
    if (path.isScope && path.subpath) {
      containingScope = &(subscope(path.trimLeaf()));
    }
    containingScope->eraseValue(path.getLeaf().name);
    ++iterator;
  }
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::loadDefault(
    const string& name, string* failureMessage, bool overwrite) {
  ConfigurationParameter& parameter =
      getParameter(name, "Could not load default value for parameter ");

  if (!parameter.hasDefaultValue) {
    throw ConfigurationResourceNotFoundException(
        "Could not load default value for parameter " +
        printCompletePath(name) +
        ": this parameter does not have a default value.");
  }
  return loadValue(name, parameter.defaultValue, failureMessage, overwrite);
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::loadAllDefaults(
    bool overwrite, bool includeTemplates) {
  ParameterStatus status = ParameterStatus::VALID;

  IteratorFeatureSelection iteratorFeatures = kIterateAllInstanceParameters;
  if (includeTemplates) {
    iteratorFeatures |= kTraverseTemplates;
  }
  auto iterator = this->begin(iteratorFeatures);
  auto end = this->end(iteratorFeatures);
  while (iterator != end) {
    const ConfigurationPath& path = *iterator;
    ConcordConfiguration* containingScope = this;
    if (path.isScope && path.subpath) {
      containingScope = &(subscope(path.trimLeaf()));
    }
    if (containingScope->parameters[path.getLeaf().name].hasDefaultValue) {
      ParameterStatus loadRes =
          containingScope->loadDefault(path.getLeaf().name, nullptr, overwrite);
      if ((loadRes == ParameterStatus::INVALID) ||
          (status == ParameterStatus::VALID)) {
        status = loadRes;
      }
    }
    ++iterator;
  }
  return status;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::validate(
    const string& name, string* failureMessage) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Could not validate contents of parameter ");
  std::unique_ptr<ConfigurationPath> path(
      getCompletePath(ConfigurationPath(name, false)));
  if (!parameter.initialized) {
    return ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  ParameterStatus status = ParameterStatus::VALID;
  string message;
  if (parameter.validator) {
    status = parameter.validator->validate(parameter.value, getRootConfig(),
                                           *path, message);
  }
  if (failureMessage && (status != ParameterStatus::VALID)) {
    *failureMessage = message;
  }
  return status;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::validateAll(
    bool ignoreUninitializedParameters, bool includeTemplates) {
  ParameterStatus status = ParameterStatus::VALID;

  IteratorFeatureSelection iteratorFeatures = kIterateAllInstanceParameters;
  if (includeTemplates) {
    iteratorFeatures |= kTraverseTemplates;
  }
  auto iterator = this->begin(iteratorFeatures);
  auto end = this->end(iteratorFeatures);
  while (iterator != end) {
    const ConfigurationPath& path = *iterator;
    ConcordConfiguration* containingScope = this;
    if (path.isScope && path.subpath) {
      containingScope = &(subscope(path.trimLeaf()));
    }

    // We don't need to validate values that will not be instantiated.
    if (!confType_.empty() &&
        !containingScope->isTagged(path.getLeaf().name, confType_)) {
      ++iterator;
      continue;
    }
    // We do not give an error message to getParameter in this case because we
    // do not expect getParameter to fail because the iterator over this should
    // not return paths that are not to existing parameters.

    const ConfigurationParameter& parameter =
        containingScope->getParameter(path.getLeaf().name, "");

    if (parameter.initialized) {
      ParameterStatus validateRes =
          containingScope->validate(path.getLeaf().name);
      if ((validateRes == ParameterStatus::INVALID) ||
          (status == ParameterStatus::VALID)) {
        status = validateRes;
      }
    } else if (!ignoreUninitializedParameters) {
      if (status != ParameterStatus::INVALID) {
        status = ParameterStatus::INSUFFICIENT_INFORMATION;
      }
    }
    ++iterator;
  }
  return status;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::generate(
    const string& name, string* failureMessage, bool overwrite) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot generate value for parameter ");
  std::unique_ptr<ConfigurationPath> path(
      getCompletePath(ConfigurationPath(name, false)));

  if (!parameter.generator) {
    throw ConfigurationResourceNotFoundException(
        "Cannot generate value for parameter " + printCompletePath(*path) +
        ": no generator object has been specified for this parameter.");
  }
  string generatedValue;
  ParameterStatus status =
      parameter.generator->generate(getRootConfig(), *path, generatedValue);
  if (status == ParameterStatus::VALID) {
    string message;
    if (parameter.validator) {
      status = parameter.validator->validate(generatedValue, getRootConfig(),
                                             *path, message);
    }
    if (failureMessage && (status != ParameterStatus::VALID)) {
      *failureMessage = message;
    }
    if ((status != ParameterStatus::INVALID) &&
        (!parameter.initialized || overwrite)) {
      parameter.value = generatedValue;
      parameter.initialized = true;
    }
  }
  return status;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::generateAll(
    bool overwrite, bool includeTemplates) {
  ParameterStatus status = ParameterStatus::VALID;

  IteratorFeatureSelection iteratorFeatures = kIterateAllInstanceParameters;
  if (includeTemplates) {
    iteratorFeatures |= kTraverseTemplates;
  }
  auto iterator = this->begin(iteratorFeatures);
  auto end = this->end(iteratorFeatures);
  while (iterator != end) {
    const ConfigurationPath& path = *iterator;
    ConcordConfiguration* containingScope = this;
    if (path.isScope && path.subpath) {
      containingScope = &(subscope(path.trimLeaf()));
    }

    // Note we do not provide an error message here to getParameter because
    // getParameter should not fail to find this parameter, given this request
    // is based on a path we got by iterating through this ConcordConfiguration.
    ConfigurationParameter& parameter =
        containingScope->getParameter(path.getLeaf().name, "");

    if (parameter.generator) {
      ParameterStatus generateRes =
          containingScope->generate(path.getLeaf().name, nullptr, overwrite);
      if ((generateRes == ParameterStatus::INVALID) ||
          (status == ParameterStatus::VALID)) {
        status = generateRes;
      }
    }
    ++iterator;
  }
  return status;
}

ConcordConfiguration::Iterator ConcordConfiguration::begin(
    ConcordConfiguration::IteratorFeatureSelection features) {
  ConfigurationIterator it(
      *this, (features & kTraverseRecursively), (features & kTraverseScopes),
      (features & kTraverseParameters), (features & kTraverseInstances),
      (features & kTraverseTemplates), false);
  return it;
}

ConcordConfiguration::Iterator ConcordConfiguration::end(
    ConcordConfiguration::IteratorFeatureSelection features) {
  ConfigurationIterator it(
      *this, (features & kTraverseRecursively), (features & kTraverseScopes),
      (features & kTraverseParameters), (features & kTraverseInstances),
      (features & kTraverseTemplates), true);
  return it;
}

ParameterSelection::ParameterSelectionIterator::ParameterSelectionIterator()
    : selection(nullptr),
      unfilteredIterator(),
      endUnfilteredIterator(),
      invalid(false) {}

ParameterSelection::ParameterSelectionIterator::ParameterSelectionIterator(
    ParameterSelection* selection, bool end)
    : selection(selection), invalid(false) {
  if (selection) {
    endUnfilteredIterator =
        selection->config->end(ConcordConfiguration::kIterateAllParameters);
    if (end) {
      unfilteredIterator =
          selection->config->end(ConcordConfiguration::kIterateAllParameters);
    } else {
      unfilteredIterator =
          selection->config->begin(ConcordConfiguration::kIterateAllParameters);
    }

    // Advance from the first value to the first value this iterator should
    // actually return if the first value the unfiltered iterator has is not
    // actually in the selection.
    if ((unfilteredIterator != endUnfilteredIterator) &&
        (!(selection->contains(*unfilteredIterator)))) {
      ++(*this);
    }

    selection->registerIterator(this);
  }
}

ParameterSelection::ParameterSelectionIterator::ParameterSelectionIterator(
    const ParameterSelection::ParameterSelectionIterator& original)
    : selection(original.selection),
      unfilteredIterator(original.unfilteredIterator),
      endUnfilteredIterator(original.endUnfilteredIterator),
      invalid(original.invalid) {
  if (selection && !invalid) {
    selection->registerIterator(this);
  }
}

ParameterSelection::ParameterSelectionIterator::~ParameterSelectionIterator() {
  if (selection && !invalid) {
    selection->deregisterIterator(this);
  }
}

ParameterSelection::ParameterSelectionIterator&
ParameterSelection::ParameterSelectionIterator::operator=(
    const ParameterSelection::ParameterSelectionIterator& original) {
  if (selection && !invalid) {
    selection->deregisterIterator(this);
  }
  selection = original.selection;
  unfilteredIterator = original.unfilteredIterator;
  endUnfilteredIterator = original.unfilteredIterator;
  invalid = original.invalid;
  if (selection && !invalid) {
    selection->registerIterator(this);
  }
  return *this;
}

bool ParameterSelection::ParameterSelectionIterator::operator==(
    const ParameterSelection::ParameterSelectionIterator& other) const {
  return (selection == other.selection) &&
         (unfilteredIterator == other.unfilteredIterator) &&
         (endUnfilteredIterator == other.endUnfilteredIterator) &&
         (invalid == other.invalid);
}

bool ParameterSelection::ParameterSelectionIterator::operator!=(
    const ParameterSelection::ParameterSelectionIterator& other) const {
  return !((*this) == other);
}

const ConfigurationPath& ParameterSelection::ParameterSelectionIterator::
operator*() const {
  if (invalid) {
    throw InvalidIteratorException(
        "Attempting to use an iterator over a ParameterSelection that has been "
        "modified since the iterator's creation.");
  }
  if (!selection || (unfilteredIterator == endUnfilteredIterator)) {
    throw std::out_of_range(
        "Attempting to get the value at an iterator already at the end of a "
        "ParameterSelection.");
  }
  return *unfilteredIterator;
}

ParameterSelection::ParameterSelectionIterator&
ParameterSelection::ParameterSelectionIterator::operator++() {
  if (invalid) {
    throw InvalidIteratorException(
        "Attempting to use an iterator over a ParameterSelection that has been "
        "modified since the iterator's creation.");
  }
  if (!selection || (unfilteredIterator == endUnfilteredIterator)) {
    throw std::out_of_range(
        "Attempting to advance an iterator already at the end of a "
        "ParameterSelection.");
  }
  ++unfilteredIterator;
  while ((unfilteredIterator != endUnfilteredIterator) &&
         (!(selection->contains(*unfilteredIterator)))) {
    ++unfilteredIterator;
  }
  return *this;
}

ParameterSelection::ParameterSelectionIterator
ParameterSelection::ParameterSelectionIterator::operator++(int) {
  ParameterSelectionIterator ret(*this);
  ++(*this);
  return ret;
}

void ParameterSelection::ParameterSelectionIterator::invalidate() {
  invalid = true;
}

void ParameterSelection::registerIterator(
    ParameterSelection::ParameterSelectionIterator* iterator) {
  iterators.insert(iterator);
}

void ParameterSelection::deregisterIterator(
    ParameterSelection::ParameterSelectionIterator* iterator) {
  iterators.erase(iterator);
}

void ParameterSelection::invalidateIterators() {
  for (ParameterSelection::Iterator* iterator : iterators) {
    iterator->invalidate();
  }
  iterators.clear();
}

ParameterSelection::ParameterSelection(ConcordConfiguration& config,
                                       shared_ptr<ParameterSelector> selector)
    : config(&config), selector(selector), iterators() {
  if (!selector) {
    throw invalid_argument(
        "Attempting to construct a ParameterSelection with a null parameter "
        "selction function.");
  }
}

ParameterSelection::ParameterSelection(const ParameterSelection& original)
    : config(original.config), selector(original.selector) {}

ParameterSelection::~ParameterSelection() { invalidateIterators(); }

bool ParameterSelection::contains(const ConfigurationPath& parameter) const {
  return (config->contains(parameter)) &&
         (selector->includeParameter(*config, parameter));
}

ParameterSelection::Iterator ParameterSelection::begin() {
  return ParameterSelectionIterator(this, false);
}

ParameterSelection::Iterator ParameterSelection::end() {
  return ParameterSelectionIterator(this, true);
}

void YAMLConfigurationInput::loadParameter(ConcordConfiguration& config,
                                           const ConfigurationPath& path,
                                           const YAML::Node& obj,
                                           Logger* errorOut, bool overwrite) {
  // Note cases in this function where we return without either writing a value
  // to the configuration or making a recursive call indicate we have concluded
  // that the parameter indicated by path is not given in the input.
  if (!obj.IsMap()) {
    return;
  }

  if (path.isScope && path.subpath) {
    YAML::Node subObj;
    if (path.useInstance) {
      if (!obj[path.name]) {
        return;
      }
      subObj.reset(obj[path.name]);
      if (!subObj.IsSequence() || (path.index >= subObj.size())) {
        return;
      }
      subObj.reset(subObj[path.index]);
    } else {
      string templateName = path.name + kYAMLScopeTemplateSuffix;
      if (!obj[templateName]) {
        return;
      }
      subObj.reset(obj[templateName]);
    }
    ConfigurationPath subscope(path);
    subscope.subpath.reset();
    loadParameter(config.subscope(subscope), *(path.subpath), subObj, errorOut,
                  overwrite);

  } else {
    if (!obj[path.name] || !obj[path.name].IsScalar()) {
      return;
    }

    string failureMessage;
    ConcordConfiguration::ParameterStatus status = config.loadValue(
        path.name, obj[path.name].Scalar(), &failureMessage, overwrite);
    if (status == ConcordConfiguration::ParameterStatus::INVALID) {
      if (errorOut) {
        LOG_ERROR((*errorOut), "Cannot load value for parameter " + path.name +
                                   ": " + failureMessage);
      }
      throw InvalidConfigurationInputException(
          "Rejected input value for parameter " + path.name + ": " +
          failureMessage);
    }
  }
}

YAMLConfigurationInput::YAMLConfigurationInput(std::istream& input)
    : input(&input), yaml(), success(false) {}

YAMLConfigurationInput::YAMLConfigurationInput(const YAML::Node& y)
    : input(nullptr), yaml(y), success(true) {}

void YAMLConfigurationInput::parseInput() {
  if (input) {
    yaml.reset(YAML::Load(*input));
    success = true;
  }
}

YAMLConfigurationInput::~YAMLConfigurationInput() {}

void YAMLConfigurationOutput::addParameterToYAML(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    YAML::Node& yaml) {
  // Note this helper function expects that it has already been validated or
  // otherwise guaranteed that path is a valid path to a declared parameter in
  // config and yaml is an associative array.
  if (!config.contains(path) || !config.hasValue<string>(path) ||
      !yaml.IsMap()) {
    return;
  }

  if (path.isScope && path.subpath) {
    YAML::Node subscope;
    string pathName;
    if (path.useInstance) {
      pathName = path.name;
      if (!yaml[pathName]) {
        yaml[pathName] = YAML::Node(YAML::NodeType::Sequence);
      }
      subscope.reset(yaml[pathName]);
      assert(subscope.IsSequence());
      while (path.index >= subscope.size()) {
        subscope.push_back(YAML::Node(YAML::NodeType::Map));
      }
      subscope.reset(subscope[path.index]);
    } else {
      pathName = path.name + kYAMLScopeTemplateSuffix;
      if (!yaml[pathName]) {
        yaml[pathName] = YAML::Node(YAML::NodeType::Map);
      }
      subscope.reset(yaml[pathName]);
      assert(subscope.IsMap());
    }
    ConfigurationPath subscopePath(path);
    subscopePath.subpath.reset();
    addParameterToYAML(config.subscope(subscopePath), *(path.subpath),
                       subscope);
  } else {
    // Add leafs only for the specific conf type.
    if (!config.confType_.empty() &&
        !config.isTagged(path.getLeaf().name, config.confType_)) {
      return;
    }
    yaml[path.name] = config.getValue<string>(path.name);
  }
}

YAMLConfigurationOutput::YAMLConfigurationOutput(std::ostream& output)
    : output(&output), yaml() {}

YAMLConfigurationOutput::~YAMLConfigurationOutput() {}

ConcordPrimaryConfigurationAuxiliaryState::
    ConcordPrimaryConfigurationAuxiliaryState()
    : slowCommitCryptosys(), commitCryptosys(), optimisticCommitCryptosys() {}

ConcordPrimaryConfigurationAuxiliaryState::
    ~ConcordPrimaryConfigurationAuxiliaryState() {
  slowCommitCryptosys.reset();
  commitCryptosys.reset();
  optimisticCommitCryptosys.reset();
};

ConfigurationAuxiliaryState*
ConcordPrimaryConfigurationAuxiliaryState::clone() {
  ConcordPrimaryConfigurationAuxiliaryState* copy =
      new ConcordPrimaryConfigurationAuxiliaryState();
  if (slowCommitCryptosys) {
    copy->slowCommitCryptosys.reset(new Cryptosystem(*slowCommitCryptosys));
  }
  if (commitCryptosys) {
    copy->commitCryptosys.reset(new Cryptosystem(*commitCryptosys));
  }
  if (optimisticCommitCryptosys) {
    copy->optimisticCommitCryptosys.reset(
        new Cryptosystem(*optimisticCommitCryptosys));
  }
  return copy;
}

// generateRSAKeyPair implementation, which itself just uses an implementation
// for RSA key generation from CryptoPP.
std::pair<string, string> generateRSAKeyPair(
    CryptoPP::RandomPool& randomnessSource) {
  std::pair<string, string> keyPair;

  CryptoPP::RSAES<CryptoPP::OAEP<CryptoPP::SHA256>>::Decryptor privateKey(
      randomnessSource, kRSAKeyLength);
  CryptoPP::HexEncoder privateEncoder(new CryptoPP::StringSink(keyPair.first));
  privateKey.AccessMaterial().Save(privateEncoder);
  privateEncoder.MessageEnd();

  CryptoPP::RSAES<CryptoPP::OAEP<CryptoPP::SHA256>>::Encryptor publicKey(
      privateKey);
  CryptoPP::HexEncoder publicEncoder(new CryptoPP::StringSink(keyPair.second));
  publicKey.AccessMaterial().Save(publicEncoder);
  publicEncoder.MessageEnd();

  return keyPair;
}

// Helper functions to the validation, generation, and sizing functions to
// follow.

static ConcordConfiguration::ParameterStatus validateNumberOfPrincipalsInBounds(
    uint16_t fVal, uint16_t cVal, uint16_t clientProxiesPerReplica,
    string& failureMessage) {
  // Conditions which should have been validated before this function was
  // called.
#ifdef ALLOW_0_FAULT_TOLERANCE
  assert((fVal >= 0) && (clientProxiesPerReplica > 0));
#else
  assert((fVal > 0) && (clientProxiesPerReplica > 0));
#endif
  // The principals consist of (3F + 2C + 1) SBFT replicas plus
  // client_proxies_per_replica client proxies for each of these replicas.
  uint64_t numPrincipals = 3 * ((uint64_t)fVal) + 2 * ((uint64_t)cVal) + 1;
  numPrincipals =
      numPrincipals + ((uint64_t)clientProxiesPerReplica) * numPrincipals;

  if (numPrincipals > UINT16_MAX) {
    failureMessage =
        "Invalid combination of values for f_val (" + to_string(fVal) +
        "), c_val (" + to_string(cVal) + "), and client_proxies_per_replica (" +
        to_string(clientProxiesPerReplica) +
        "): these parameters imply too many (" + to_string(numPrincipals) +
        ") SBFT principals; a maximum of " + to_string(UINT16_MAX) +
        " principals are currently supported.";
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static std::pair<string, string> parseCryptosystemSelection(string selection) {
  std::pair<string, string> parseRes({"", ""});
  boost::trim(selection);
  size_t spaceLoc = selection.find_first_of(" ");
  parseRes.first = selection.substr(0, spaceLoc);
  if (spaceLoc < selection.length()) {
    parseRes.second = selection.substr(spaceLoc);
  }
  boost::trim(parseRes.first);
  boost::trim(parseRes.second);
  return parseRes;
}

NodesSizer::~NodesSizer() {}

ConcordConfiguration::ParameterStatus NodesSizer::sizeScope(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t& output) {
  if (!(config.hasValue<uint16_t>("f_val") &&
        config.hasValue<uint16_t>("c_val"))) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (!((config.validate("f_val") ==
         ConcordConfiguration::ParameterStatus::VALID) &&
        (config.validate("c_val") ==
         ConcordConfiguration::ParameterStatus::VALID))) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }

  uint16_t f = config.getValue<uint16_t>("f_val");
  uint16_t c = config.getValue<uint16_t>("c_val");
  size_t numNodes = 3 * (size_t)f + 2 * (size_t)c + 1;
  if (numNodes > (size_t)UINT16_MAX) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  output = numNodes;
  return ConcordConfiguration::ParameterStatus::VALID;
}

ReplicasSizer::~ReplicasSizer() {}

size_t getCommitterNodesCount(const ConcordConfiguration& config,
                              const std::string expceptMsg) {
  size_t nodeSize;

  ConfigurationPath p;  // dummy path - this is not used in sizeNodes()
  NodesSizer s;
  if (s.sizeScope(config, p, nodeSize) !=
      ConcordConfiguration::ParameterStatus::VALID) {
    throw InvalidConfigurationInputException(expceptMsg);
  }

  return nodeSize;
}

RoNodesSizer::~RoNodesSizer() {}

ConcordConfiguration::ParameterStatus RoNodesSizer::sizeScope(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t& output) {
  if (!(config.hasValue<uint16_t>("num_ro_replicas"))) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (!((config.validate("num_ro_replicas") ==
         ConcordConfiguration::ParameterStatus::VALID))) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }

  output = config.getValue<uint16_t>("num_ro_replicas");

  return ConcordConfiguration::ParameterStatus::VALID;
}

ConcordConfiguration::ParameterStatus ReplicasSizer::sizeScope(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t& output) {
  output = 1;
  return ConcordConfiguration::ParameterStatus::VALID;
}

class ClientProxiesSizer : public ConcordConfiguration::ScopeSizer {
 public:
  virtual ~ClientProxiesSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    if (!config.hasValue<uint16_t>("client_proxies_per_replica")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    if (config.validate("client_proxies_per_replica") !=
        ConcordConfiguration::ParameterStatus::VALID) {
      return ConcordConfiguration::ParameterStatus::INVALID;
    }

    output = config.getValue<uint16_t>("client_proxies_per_replica");
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class BooleanValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~BooleanValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (std::find(kValidBooleansTrue.begin(), kValidBooleansTrue.end(),
                  value) != kValidBooleansTrue.end() ||
        std::find(kValidBooleansFalse.begin(), kValidBooleansFalse.end(),
                  value) != kValidBooleansFalse.end()) {
      return ConcordConfiguration::ParameterStatus::VALID;
    }

    failureMessage = "Invalid value for parameter " + path.toString() + ": \"" +
                     value +
                     "\". A boolean (e.g. \"true\" or \"false\") is required.";
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
};

class IntValidator : public ConcordConfiguration::ParameterValidator {
 private:
  long long lowerBound;
  long long upperBound;

 public:
  IntValidator(long long lowerBound, long long upperBound)
      : lowerBound(lowerBound), upperBound(upperBound) {}
  virtual ~IntValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    long long intVal;
    try {
      intVal = std::stoll(value);
    } catch (invalid_argument& e) {
      failureMessage = "Invalid value for parameter " + path.toString() +
                       ": \"" + value + "\". An integer is required.";
      return ConcordConfiguration::ParameterStatus::INVALID;
    } catch (std::out_of_range& e) {
      failureMessage = "Invalid value for parameter " + path.toString() +
                       ": \"" + value + "\". An integer in the range (" +
                       to_string(lowerBound) + ", " + to_string(upperBound) +
                       "), inclusive, is required.";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    if ((intVal < lowerBound) || (intVal > upperBound)) {
      failureMessage = "Invalid value for parameter " + path.toString() +
                       ": \"" + value + "\". An integer in the range (" +
                       to_string(lowerBound) + ", " + to_string(upperBound) +
                       "), inclusive, is required.";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

UIntValidator::UIntValidator(unsigned long long lowerBound,
                             unsigned long long upperBound)
    : lowerBound(lowerBound), upperBound(upperBound) {}

UIntValidator::~UIntValidator() {}

ConcordConfiguration::ParameterStatus UIntValidator::validate(
    const string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, string& failureMessage) {
  unsigned long long intVal;
  try {
    intVal = std::stoull(value);
  } catch (invalid_argument& e) {
    failureMessage = "Invalid value for parameter " + path.toString() + ": \"" +
                     value + "\". An integer is required.";
    return ConcordConfiguration::ParameterStatus::INVALID;
  } catch (std::out_of_range& e) {
    failureMessage = "Invalid value for parameter " + path.toString() + ": \"" +
                     value + "\". An integer in the range (" +
                     to_string(lowerBound) + ", " + to_string(upperBound) +
                     "), inclusive, is required.";
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  if ((intVal < lowerBound) || (intVal > upperBound)) {
    failureMessage = "Invalid value for parameter " + path.toString() + ": \"" +
                     value + "\". An integer in the range (" +
                     to_string(lowerBound) + ", " + to_string(upperBound) +
                     "), inclusive, is required.";
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

class ClientProxiesPerReplicaValidator
    : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~ClientProxiesPerReplicaValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(1, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    } else {
      if (!config.hasValue<uint16_t>("f_val") ||
          !config.hasValue<uint16_t>("c_val")) {
        failureMessage =
            "Cannot fully validate client_proxies_per_replica: value is in "
            "range, but f_val and c_val must both also be known to veriy that "
            "the total number of SBFT principals needed is in range.";
        return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
      }
      uint16_t clientProxiesPerReplica = (uint16_t)(std::stoull(value));
      return validateNumberOfPrincipalsInBounds(
          config.getValue<uint16_t>("f_val"),
          config.getValue<uint16_t>("c_val"), clientProxiesPerReplica,
          failureMessage);
    }
  }
};

class PublicKeyValidator : public ConcordConfiguration::ParameterValidator {
 private:
  const unique_ptr<Cryptosystem>& cryptosystem;

 public:
  PublicKeyValidator(const unique_ptr<Cryptosystem>& cryptosystem)
      : cryptosystem(cryptosystem) {}
  virtual ~PublicKeyValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (!cryptosystem) {
      failureMessage = "Cannot assess validity of threshold public key " +
                       path.toString() +
                       ": corresponding cryptosystem is not initialized.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }

    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ThresholdPublicKeyLoader
    : public ConcordConfiguration::ParameterGenerator {
 private:
  const unique_ptr<Cryptosystem>& cryptosystem;

 public:
  ThresholdPublicKeyLoader(const unique_ptr<Cryptosystem>& cryptosystem)
      : cryptosystem(cryptosystem) {}
  virtual ~ThresholdPublicKeyLoader() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!cryptosystem) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    try {
      output = cryptosystem->getSystemPublicKey();
      return ConcordConfiguration::ParameterStatus::VALID;
    } catch (const exception& e) {
      LOG_FATAL(THRESHSIGN_LOG, e.what());
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
  }
};

class CValValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~CValValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(0, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    } else {
      if (!config.hasValue<uint16_t>("f_val") ||
          !config.hasValue<uint16_t>("client_proxies_per_replica")) {
        failureMessage =
            "Cannot fully validate c_val: value is in range, but f_val and "
            "client_proxies_per_replica must both also be known to veriy that "
            "the total number of SBFT principals needed is in range.";
        return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
      }
      uint16_t cVal = (uint16_t)(std::stoull(value));
      return validateNumberOfPrincipalsInBounds(
          config.getValue<uint16_t>("f_val"), cVal,
          config.getValue<uint16_t>("client_proxies_per_replica"),
          failureMessage);
    }
  }
};

class FValValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~FValValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(
#ifdef ALLOW_0_FAULT_TOLERANCE
        0,
#else
        1,
#endif
        UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      failureMessage =
          "Cannot fully validate f_val: value is in range, but c_val and "
          "client_proxies_per_replica must both also be known to veriy that "
          "the total number of SBFT principals needed is in range.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t fVal = (uint16_t)(std::stoull(value));
    return validateNumberOfPrincipalsInBounds(
        fVal, config.getValue<uint16_t>("c_val"),
        config.getValue<uint16_t>("client_proxies_per_replica"),
        failureMessage);
  }
};

class NumClientProxiesValidator
    : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~NumClientProxiesValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(1, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      failureMessage =
          "Cannot validate num_client_proxies: values for f_val, c_val and "
          "client_proxies_per_replica are required to determine expected value "
          "of num_client_proxies.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t expectedNumClientProxies =
        config.getValue<uint16_t>("client_proxies_per_replica") *
        (3 * config.getValue<uint16_t>("f_val") +
         2 * config.getValue<uint16_t>("c_val") + 1);
    if ((uint16_t)(std::stoull(value)) != expectedNumClientProxies) {
      failureMessage =
          "Invalid valud for num_client_proxies: " + value +
          "; num_client_proxies must be equal to client_proxies_per_replica * "
          "(3 * f_val + 2 * c_val + 1).";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class NumClientProxiesCalculator
    : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~NumClientProxiesCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    output = to_string(config.getValue<uint16_t>("client_proxies_per_replica") *
                       (3 * config.getValue<uint16_t>("f_val") +
                        2 * config.getValue<uint16_t>("c_val") + 1));
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class NumPrincipalsValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~NumPrincipalsValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(1, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      failureMessage =
          "Cannot validate num_principals: values for f_val, c_val and "
          "client_proxies_per_replica are required to determine expected value "
          "of num_principals.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t expectedNumPrincipals =
        (config.getValue<uint16_t>("client_proxies_per_replica") + 1) *
        (3 * config.getValue<uint16_t>("f_val") +
         2 * config.getValue<uint16_t>("c_val") + 1);
    if ((uint16_t)(std::stoull(value)) != expectedNumPrincipals) {
      failureMessage =
          "Invalid value for num_principals: " + value +
          "; num_principals must be equal to (1 + client_proxies_per_replica) "
          "* (3 * f_val + 2 * c_val + 1).";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class NumPrincipalsCalculator
    : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~NumPrincipalsCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    output = to_string(
        (1 + config.getValue<uint16_t>("client_proxies_per_replica")) *
        (3 * config.getValue<uint16_t>("f_val") +
         2 * config.getValue<uint16_t>("c_val") + 1));
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class NumReplicasValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~NumReplicasValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(1, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val")) {
      failureMessage =
          "Cannot validate num_replicas: values for f_val and c_val are "
          "required to determine expected value of num_replicas.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t expectedNumReplicas = 3 * config.getValue<uint16_t>("f_val") +
                                   2 * config.getValue<uint16_t>("c_val") + 1;
    if ((uint16_t)(std::stoull(value)) != expectedNumReplicas) {
      failureMessage =
          "Invalid value for num_replicas: " + value +
          "; num_replicas must be equal to 3 * f_val + 2 * c_val + 1.";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class NumReplicasCalculator : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~NumReplicasCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    output = to_string(3 * config.getValue<uint16_t>("f_val") +
                       2 * config.getValue<uint16_t>("c_val") + 1);
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class PositiveReplicaIntValidator
    : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~PositiveReplicaIntValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    // Note we can only truly validate that this parameter fits in an int on an
    // actual Concord replica, and not in conc_genconfig, as int's size may vary
    // between different builds and machines; in conc_genconfig this function
    // will enforce as loose a bound as possible because it does not
    // necessarilly know how big an int will be on the nodes being configured.
    auto limits = make_unique<UIntValidator>(1, ULLONG_MAX);
    if (config.getConfigurationStateLabel() ==
        kConcordNodeConfigurationStateLabel) {
      limits = make_unique<UIntValidator>(1, INT_MAX);
    }

    return limits->validate(value, config, path, failureMessage);
  }
};

class DatabaseImplementationValidator
    : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~DatabaseImplementationValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (!((value == "memory") || (value == "rocksdb"))) {
      failureMessage =
          "Unrecognized database implementation: \"" + value + "\".";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class StorageTypeValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~StorageTypeValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (value != "basic" && value != "merkle") {
      failureMessage = "Unrecognized storage type: \"" + value + "\".";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class PortNumberValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~PortNumberValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(0, UINT16_MAX);
    return boundsCheck.validate(value, config, path, failureMessage);
  }
};

class PrivateKeyValidator : public ConcordConfiguration::ParameterValidator {
 private:
  const unique_ptr<Cryptosystem>& cryptosystem;

 public:
  PrivateKeyValidator(const unique_ptr<Cryptosystem>& cryptosystem)
      : cryptosystem(cryptosystem) {}
  virtual ~PrivateKeyValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (!cryptosystem) {
      failureMessage = "Cannot assess validity of threshold private key " +
                       path.toString() +
                       ": corresponding cryptosystem is not initialized.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ThresholdPrivateKeyLoader
    : public ConcordConfiguration::ParameterGenerator {
 private:
  const unique_ptr<Cryptosystem>& cryptosystem;

 public:
  ThresholdPrivateKeyLoader(const unique_ptr<Cryptosystem>& cryptosystem)
      : cryptosystem(cryptosystem) {}
  virtual ~ThresholdPrivateKeyLoader() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!cryptosystem) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    try {
      // This generator should only be called with paths of the form:
      //
      // node[i]/replica[0]/..._private_key
      //
      // We infer which replica the key is for from this path. Note the
      // Cryptosystem class considers signers 1-indexed.
      if ((path.name != "node") || !path.isScope || !path.useInstance ||
          (path.index >= UINT16_MAX)) {
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
      uint16_t signer = path.index + 1;

      output = cryptosystem->getPrivateKey(signer);
      return ConcordConfiguration::ParameterStatus::VALID;
    } catch (const exception& e) {
      LOG_FATAL(THRESHSIGN_LOG, e.what());
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
  }
};

class VerificationKeyValidator
    : public ConcordConfiguration::ParameterValidator {
 private:
  const unique_ptr<Cryptosystem>& cryptosystem;

 public:
  VerificationKeyValidator(const unique_ptr<Cryptosystem>& cryptosystem)
      : cryptosystem(cryptosystem) {}
  virtual ~VerificationKeyValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (!cryptosystem) {
      failureMessage = "Cannot assess validity of threshold verification key " +
                       path.toString() +
                       ": corresponding cryptosystem is not initialized.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ThresholdVerificationKeyLoader
    : public ConcordConfiguration::ParameterGenerator {
 private:
  const unique_ptr<Cryptosystem>& cryptosystem;

 public:
  ThresholdVerificationKeyLoader(const unique_ptr<Cryptosystem>& cryptosystem)
      : cryptosystem(cryptosystem) {}
  virtual ~ThresholdVerificationKeyLoader() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!cryptosystem) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    try {
      // This generator should only be called with paths of the form:
      //
      // node[i]/replica[0]/..._verification_key
      //
      // We infer which replica the key is for from this path. Note the
      // Cryptosystem class considers signers 1-indexed.
      if ((path.name != "node") || !path.isScope || !path.useInstance ||
          (path.index >= UINT16_MAX)) {
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
      uint16_t signer = path.index + 1;

      output = (cryptosystem->getSystemVerificationKeys())[signer];
      return ConcordConfiguration::ParameterStatus::VALID;
    } catch (const exception& e) {
      LOG_FATAL(THRESHSIGN_LOG, e.what());
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
  }
};

class PrincipalIdValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~PrincipalIdValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    UIntValidator boundsCheck(0, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }
    uint16_t principalID = (uint16_t)(std::stoull(value));

    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("num_ro_replicas") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      failureMessage =
          "Cannot fully validate Concord-BFT principal ID for " +
          path.toString() +
          ": f_val, c_val, and client_proxies_per_replica are required to "
          "determine bounds for maximum principal ID.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t fVal = config.getValue<uint16_t>("f_val");
    uint16_t cVal = config.getValue<uint16_t>("c_val");
    uint16_t numRoReplicas = config.getValue<uint16_t>("num_ro_replicas");
    uint16_t numReplicas = 3 * fVal + 2 * cVal + 1;
    uint16_t clientProxiesPerReplica =
        config.getValue<uint16_t>("client_proxies_per_replica");
    uint16_t numPrincipals = numReplicas * (1 + clientProxiesPerReplica);

    // The path to a principal Id should be of one of these forms:
    //   node[i]/replica[0]/principal_id
    //   node[i]/client_proxy[j]/principal_id
    assert(path.isScope && path.subpath);

    if (path.name == "ro_node") {
      if (principalID < numReplicas ||
          principalID >= numReplicas + numRoReplicas) {
        failureMessage =
            "Invalid principal ID for " + path.toString() + ": " +
            to_string(principalID) +
            ". Principal IDs for RO replicas must be bigger than num_replicas "
            "and less than num_replicas + num_ro_replicas.";
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
    } else if (path.subpath->name == "replica") {
      if (principalID >= numReplicas) {
        failureMessage =
            "Invalid principal ID for " + path.toString() + ": " +
            to_string(principalID) +
            ". Principal IDs for replicas must be less than num_replicas.";
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
    } else {
      assert(path.subpath->name == "client_proxy");
      if ((principalID < numReplicas + numRoReplicas) ||
          (principalID >= numReplicas + numRoReplicas + numPrincipals)) {
        failureMessage =
            "Invalid principal ID for " + path.toString() + ": " +
            to_string(principalID) +
            ". Principal IDs for client proxies should be in the range "
            "(num_replicas, num_principals - 1), inclusive.";
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
    }

    res = ConcordConfiguration::ParameterStatus::VALID;
    for (size_t i = 0; i < numReplicas; ++i) {
      ConfigurationPath replicaPath("node", (size_t)i);
      replicaPath.subpath.reset(new ConfigurationPath("replica", (size_t)0));
      replicaPath.subpath->subpath.reset(
          new ConfigurationPath("principal_id", false));
      if (!config.hasValue<uint16_t>(replicaPath)) {
        if (replicaPath != path) {
          res = ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
        }
      } else if ((config.getValue<uint16_t>(replicaPath) == principalID) &&
                 (replicaPath != path)) {
        failureMessage = "Invalid principal ID for " + path.toString() + ": " +
                         to_string(principalID) +
                         ". This ID is non-unique; it duplicates the ID for " +
                         replicaPath.toString() + ".";
        return ConcordConfiguration::ParameterStatus::INVALID;
      }

      for (size_t j = 0; j < clientProxiesPerReplica; ++j) {
        ConfigurationPath clientProxyPath("node", (size_t)i);
        clientProxyPath.subpath.reset(new ConfigurationPath("client_proxy", j));
        clientProxyPath.subpath->subpath.reset(
            new ConfigurationPath("principal_id", false));
        if (!config.hasValue<uint16_t>(clientProxyPath)) {
          if (clientProxyPath != path) {
            res =
                ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
          }
        } else if ((config.getValue<uint16_t>(clientProxyPath) ==
                    principalID) &&
                   (clientProxyPath != path)) {
          failureMessage =
              "Invalid principal ID for " + path.toString() + ": " +
              to_string(principalID) +
              ". This ID is non-unique; it duplicates the ID for " +
              clientProxyPath.toString() + ".";
          return ConcordConfiguration::ParameterStatus::INVALID;
        }
      }
    }

    for (size_t i = 0; i < numRoReplicas; ++i) {
      ConfigurationPath roReplicaPath("ro_node", (size_t)i);
      roReplicaPath.subpath.reset(new ConfigurationPath("principal_id", false));
      if (!config.hasValue<uint16_t>(roReplicaPath)) {
        if (roReplicaPath != path) {
          res = ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
        }
      } else if ((config.getValue<uint16_t>(roReplicaPath) == principalID) &&
                 (roReplicaPath != path)) {
        failureMessage = "Invalid principal ID for " + path.toString() + ":" +
                         to_string(principalID) +
                         ". This ID is non-unique; it duplicates the ID for " +
                         roReplicaPath.toString() + ".";
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
    }

    if (res ==
        ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION) {
      failureMessage = "Cannot fully validate principal ID for " +
                       path.toString() +
                       ": Not all other principal IDs are known, but are "
                       "required to check for uniqueness.";
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class PrincipalIdCalculator : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~PrincipalIdCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    // The path to a principal Id should be of one of these forms:
    //   node[i]/replica[0]/principal_id
    //   node[i]/client_proxy[j]/principal_id
    //   ro_node[i]/principa_id
    assert(path.isScope && path.subpath && path.useInstance);

    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("num_ro_replicas")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t numReplicas = 3 * config.getValue<uint16_t>("f_val") +
                           2 * config.getValue<uint16_t>("c_val") + 1;
    uint16_t numRoReplicas = config.getValue<uint16_t>("num_ro_replicas");

    if (path.name == "ro_node") {
      output = to_string(path.index + numReplicas);
    } else if (path.subpath->name == "replica") {
      output = to_string(path.index);
    } else {
      assert((path.subpath->name == "client_proxy") && path.subpath->isScope &&
             path.subpath->useInstance);

      // path.index - index of the replica
      // path.subpath->index - index of the proxy
      output = to_string(path.index + numRoReplicas +
                         numReplicas * (1 + path.subpath->index));
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class TimeSourceIdCalculator : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~TimeSourceIdCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    output = "time-source" + to_string(path.index);
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

const size_t kRSAPublicKeyHexadecimalLength = 584;
// Note we do not have a correpsonding kRSAPrivateKeyHexadecimalLength constant
// because the hexadecimal length of RSA private keys actually seems to vary a
// little in the current serialization of them.

class RSAPrivateKeyValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~RSAPrivateKeyValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (!(std::regex_match(value, std::regex("[0-9A-Fa-f]+")))) {
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class RSAPrivateKeyLoader : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~RSAPrivateKeyLoader() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    // The path to an RSA private key should be of the form:
    //   node[i]/replica[0]/private_key
    // We infer what replica the key is for from the path.
    assert(path.isScope && path.useInstance && path.subpath &&
           (path.subpath->name == "replica" || path.name == "ro_node"));
    const ConcordPrimaryConfigurationAuxiliaryState* auxState =
        dynamic_cast<const ConcordPrimaryConfigurationAuxiliaryState*>(
            config.getAuxiliaryState());
    assert(auxState);

    size_t nodeIndex = 0;  // path.index;
    if (path.subpath->name == "replica") {
      nodeIndex = path.index;
    } else {
      assert(path.name == "ro_node");
      size_t numNodes = getCommitterNodesCount(
          config, "Can't get node count for private key generation");
      nodeIndex = numNodes + path.index;
    }

    if (nodeIndex >= auxState->replicaRSAKeys.size()) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    output = auxState->replicaRSAKeys[nodeIndex].first;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class RSAPublicKeyValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~RSAPublicKeyValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (!((value.length() == kRSAPublicKeyHexadecimalLength) &&
          std::regex_match(value, std::regex("[0-9A-Fa-f]+")))) {
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class RSAPublicKeyLoader : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~RSAPublicKeyLoader() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    // The path to an RSA public key should be of this form:
    //   node[i]/replica[0]/public_key
    // We infer what replica the key is for from the path.
    assert(path.isScope && path.useInstance && path.subpath &&
           (path.subpath->name == "replica"));
    const ConcordPrimaryConfigurationAuxiliaryState* auxState =
        dynamic_cast<const ConcordPrimaryConfigurationAuxiliaryState*>(
            config.getAuxiliaryState());
    assert(auxState);

    size_t nodeIndex = path.index;

    if (nodeIndex >= auxState->replicaRSAKeys.size()) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    output = auxState->replicaRSAKeys[nodeIndex].second;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

// We generally do not validate hosts as we want to allow use of either
// hostnames or IP addresses in the configuration files in various contexts
// without the configuration generation utility concerning itself about this;
// however, one constraint we do want to validate is that, when a node reads its
// configuration file, node-local hosts are set to loopback if the
// "use_loopback_for_local_hosts" option is enabled.
class PrincipalHostValidator : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~PrincipalHostValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    // Enforcing that loopback is expected for node-local hosts does not make
    // sense unless we have a specific node's configuration and this principal
    // host belongs to a particular node.
    if ((config.getConfigurationStateLabel() !=
         kConcordNodeConfigurationStateLabel) ||
        !path.isScope || !path.useInstance || (path.name != "node")) {
      return ConcordConfiguration::ParameterStatus::VALID;
    }

    // We make a copy of the configuration to use for determining which node is
    // local because this process may require iteration of the configuration,
    // and, under the implementations at the time of this writing, a const
    // ConcordConfiguration cannot be iterated.
    ConcordConfiguration nonConstConfig = config;

    if (config.hasValue<bool>("use_loopback_for_local_hosts") &&
        config.getValue<bool>("use_loopback_for_local_hosts") &&
        (value != "127.0.0.1")) {
      // Note that, depending on the order order in which parameters are loaded,
      // we may not yet be able to tell which node is local in this
      // configuration at the time this validator is called when first loading
      // hosts into the config. To handle this case, we return
      // INSUFFICIENT_INFORMATION if detectLocalNode throws an exception.
      size_t local_node_index;
      try {
        std::tie(local_node_index, std::ignore) =
            detectLocalNode(nonConstConfig);
      } catch (const ConfigurationResourceNotFoundException& e) {
        return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
      }
      if (path.index == local_node_index) {
        failureMessage =
            "Invalid host address for " + path.toString() + ": " + value +
            "; the value 127.0.0.1 (i.e. loopback) is expected for "
            "this host since it is local to this node and "
            "use_loopback_for_local_hosts is enabled.";
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

const unordered_set<string> timeVerificationOptions({"rsa-time-signing",
                                                     "bft-client-proxy-id",
                                                     "none"});

class EnumeratedOptionValidator
    : public ConcordConfiguration::ParameterValidator {
 private:
  const unordered_set<string>& options;

 public:
  EnumeratedOptionValidator(const unordered_set<string>& options)
      : options(options) {}
  virtual ~EnumeratedOptionValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, string& failureMessage) override {
    if (options.count(value) < 1) {
      failureMessage = "Unrecognized or unsupported value for " +
                       path.toString() + ": \"" + value +
                       "\". Recognized values include: ";
      bool was_first = true;
      for (const auto& option : options) {
        if (!was_first) {
          failureMessage += ", ";
        }
        failureMessage += "\"" + option + "\"";
        was_first = false;
      }
      failureMessage += ".";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

// Implementation of specifyConfiguration and other utility functions that
// encode knowledge about the current configuration.

// This function is intended to serve as a single source of truth for all places
// that need to know the current Concord configuration format. If you need to
// add new configuration parameters or otherwise change the format of our
// configuration files, please add to or modify the code in this function.
void specifyConfiguration(ConcordConfiguration& config) {
  config.clear();

  // Auxiliary State initialization
  ConcordPrimaryConfigurationAuxiliaryState* auxState =
      new ConcordPrimaryConfigurationAuxiliaryState();
  config.setAuxiliaryState(auxState);

  // Scope declarations
  config.declareScope(
      "node",
      "Concord nodes, the nodes that form the distributed system that "
      "maintains a blockchain in Concord. Each node runs in its own process, "
      "and, in a production deployment, each node should be on a different "
      "machine. Ideally, the nodes should also be split up into different "
      "fault domains.",
      make_shared<NodesSizer>());
  ConcordConfiguration& node = config.subscope("node");

  config.declareScope("ro_node",
                      "Concord nodes that are syncing state without "
                      "participating in the consensus.",
                      make_shared<RoNodesSizer>());
  ConcordConfiguration& ro_node = config.subscope("ro_node");

  node.declareScope(
      "replica",
      "SBFT replicas, which serve as the core replicas for Byzantine fault "
      "tolerant consensus in a Concord deployment. At the time of this "
      "writing, there generally should be no more than one SBFT replica per "
      "Concord node, and ideally the SBFT replicas should be split up into "
      "separate fault domains.",
      make_shared<ReplicasSizer>());
  ConcordConfiguration& replica = node.subscope("replica");

  node.declareScope(
      "client_proxy",
      "SBFT client proxies; these client proxies serve to connect the SBFT "
      "replicas to whatever may be accessing and using the blockchain that the "
      "replicas maintain. The client proxies communicate with the SBFT "
      "replicas to forward requests incoming to the Concord system and to "
      "fetch information from the blockchain to be served externally, but the "
      "client proxies are not \"voting\" participants in establishing "
      "consensus.",
      make_shared<ClientProxiesSizer>());
  ConcordConfiguration& clientProxy = node.subscope("client_proxy");

  vector<string> privateGeneratedTags(
      {"config_generation_time", "generated", "private"});
  vector<string> publicGeneratedTags(
      {"config_generation_time", "generated", "public"});
  vector<string> publicInputTags({"config_generation_time", "input", "public"});
  vector<string> principalHostTags(
      {"config_generation_time", "could_be_loopback", "input", "public"});
  vector<string> defaultableByUtilityTags(
      {"config_generation_time", "defaultable", "public"});
  vector<string> privateInputTags({"input", "private"});
  vector<string> defaultableByReplicaTags({"defaultable", "private"});
  vector<string> privateOptionalTags({"optional", "private"});
  vector<string> publicOptionalTags({"optional", "public"});
  vector<string> publicDefaultableTags({"defaultable", "public"});

  // Global optional config values
  vector<string> optionalTags({"optional"});

  // Types of configuration to output
  vector<string> applicationTag({"application", "optionalInput", "all"});
  vector<string> deploymentTag({"deployment", "all"});
  vector<string> secretsTag({"secrets", "all"});

  // Parameter declarations
  config.declareParameter("client_proxies_per_replica",
                          "The number of SBFT client proxies to create on each "
                          "Concord node with each SBFT replica.");
  config.tagParameter("client_proxies_per_replica", publicInputTags);
  config.tagParameter("client_proxies_per_replica", deploymentTag);
  config.addValidator("client_proxies_per_replica",
                      make_shared<ClientProxiesPerReplicaValidator>());

  config.declareParameter(
      "commit_cryptosys",
      "Type of cryptosystem to use to commit transactions in the SBFT general "
      "path (threshold 3F + C + 1). This parameter should consist of two "
      "space-separated strings, the first of which names the cryptosystem type "
      "and the second of which is a type-specific parameter or subtype "
      "selection (for example, the second string might be an eliptic curve "
      "type if an elliptic curve cryptosystem is selected).",
      "threshold-bls BN-P254");
  config.tagParameter("commit_cryptosys", defaultableByUtilityTags);
  config.tagParameter("commit_cryptosys", secretsTag);

  config.declareParameter(
      "commit_public_key",
      "Public key for the general path commit cryptosystem.");
  config.tagParameter("commit_public_key", publicGeneratedTags);
  config.tagParameter("commit_public_key", secretsTag);
  config.addValidator("commit_public_key", make_shared<PublicKeyValidator>(
                                               auxState->commitCryptosys));
  config.addGenerator(
      "commit_public_key",
      make_shared<ThresholdPublicKeyLoader>(auxState->commitCryptosys));

  config.declareParameter(
      "concord-bft_communication_buffer_length",
      "Size of buffers to be used for messages exchanged with and within "
      "Concord-BFT. Note that the capacity of these buffers may limit things "
      "like the maximum sizes of transactions or replies to requests that can "
      "be handled.",
      "64000");
  config.tagParameter("concord-bft_communication_buffer_length",
                      defaultableByUtilityTags);
  config.tagParameter("concord-bft_communication_buffer_length",
                      applicationTag);
  config.addValidator(
      "concord-bft_communication_buffer_length",
      make_shared<UIntValidator>(kMinConcordBFTCommunicationBufferSize,
                                 kMaxConcordBFTCommunicationBufferSize));

  config.declareParameter(
      "num_of_external_clients",
      "Represents the number of external clients in the system. This value "
      "should be a sum of the number of clients in the client pools at all "
      "participant nodes.",
      "15");
  config.tagParameter("num_of_external_clients", defaultableByUtilityTags);
  config.tagParameter("num_of_external_clients", applicationTag);
  config.tagParameter("num_of_external_clients", deploymentTag);

  // TODO: The following parameters should be completely optional because
  // its default values are within concord-bft
  config.declareParameter("concord-bft_max_external_message_size",
                          "Maximum external message size");
  config.tagParameter("concord-bft_max_external_message_size", optionalTags);
  config.tagParameter("concord-bft_max_external_message_size", applicationTag);
  config.addValidator("concord-bft_max_external_message_size",
                      make_shared<UIntValidator>(0, UINT32_MAX));

  config.declareParameter("concord-bft_max_reply_message_size",
                          "Maximum reply message size");
  config.tagParameter("concord-bft_max_reply_message_size", optionalTags);
  config.tagParameter("concord-bft_max_reply_message_size", applicationTag);
  config.addValidator("concord-bft_max_reply_message_size",
                      make_shared<UIntValidator>(0, UINT32_MAX));

  config.declareParameter("concord-bft_max_num_of_reserved_pages",
                          "Maximum number of reserved pages");
  config.tagParameter("concord-bft_max_num_of_reserved_pages", optionalTags);
  config.tagParameter("concord-bft_max_num_of_reserved_pages", applicationTag);
  config.addValidator("concord-bft_max_num_of_reserved_pages",
                      make_shared<UIntValidator>(0, UINT32_MAX));

  config.declareParameter("concord-bft_size_of_reserved_page",
                          "Size of a reserved page");
  config.tagParameter("concord-bft_size_of_reserved_page", optionalTags);
  config.tagParameter("concord-bft_size_of_reserved_page", applicationTag);
  config.addValidator("concord-bft_size_of_reserved_page",
                      make_shared<UIntValidator>(0, UINT32_MAX));

  config.declareParameter("concurrency_level",
                          "Number of consensus operations that Concord-BFT may "
                          "execute in parallel.",
                          "3");
  config.tagParameter("concurrency_level", defaultableByUtilityTags);
  config.tagParameter("concurrency_level", applicationTag);
  config.addValidator("concurrency_level",
                      make_shared<UIntValidator>(1, UINT16_MAX));

  config.declareParameter(
      "c_val",
      "C parameter to the SBFT algorithm, that is, the number of slow, "
      "crashed, or otherwise non-responsive replicas that can be tolerated "
      "before having to fall back on a slow path for consensus.");
  config.tagParameter("c_val", publicInputTags);
  config.tagParameter("c_val", deploymentTag);
  config.addValidator("c_val", make_shared<CValValidator>());

  config.declareParameter(
      "f_val",
      "F parameter to the SBFT algorithm, that is, the number of "
      "Byzantine-faulty replicas that can be tolerated in the system before "
      "safety guarantees are lost.");
  config.tagParameter("f_val", publicInputTags);
  config.tagParameter("f_val", deploymentTag);
  config.addValidator("f_val", make_shared<FValValidator>());

  config.declareParameter(
      "gas_limit",
      "Ethereum gas limit to enforce on all transactions; this prevents "
      "transactions that either fail to terminate or take excessively long to "
      "do so from burdening the system.",
      "10000000");
  config.tagParameter("gas_limit", defaultableByUtilityTags);
  config.tagParameter("gas_limit", applicationTag);
  config.addValidator("gas_limit", make_shared<UIntValidator>(1, UINT64_MAX));

  config.declareParameter(
      "num_client_proxies",
      "Total number of Concord-BFT client proxies in this deployment.");
  config.tagParameter("num_client_proxies", publicGeneratedTags);
  config.tagParameter("num_client_proxies", deploymentTag);
  config.addValidator("num_client_proxies",
                      make_shared<NumClientProxiesValidator>());
  config.addGenerator("num_client_proxies",
                      make_shared<NumClientProxiesCalculator>());

  config.declareParameter("num_principals",
                          "Combined total number of replicas and Concord-BFT "
                          "client proxies in this deployment.");
  config.tagParameter("num_principals", publicGeneratedTags);
  config.tagParameter("num_principals", deploymentTag);
  config.addValidator("num_principals", make_shared<NumPrincipalsValidator>());
  config.addGenerator("num_principals", make_shared<NumPrincipalsCalculator>());

  config.declareParameter(
      "num_ro_replicas",
      "Total number of Concord read-only replicas in this deployment.", "0");
  config.tagParameter("num_ro_replicas", defaultableByUtilityTags);
  config.tagParameter("num_ro_replicas", publicInputTags);
  config.tagParameter("num_ro_replicas", deploymentTag);
  config.addValidator("num_ro_replicas",
                      make_shared<UIntValidator>(0, UINT64_MAX));

  config.declareParameter(
      "num_replicas", "Total number of Concord replicas in this deployment.");
  config.tagParameter("num_replicas", publicGeneratedTags);
  config.tagParameter("num_replicas", deploymentTag);
  config.addValidator("num_replicas", make_shared<NumReplicasValidator>());
  config.addGenerator("num_replicas", make_shared<NumReplicasCalculator>());

  config.declareParameter(
      "optimistic_commit_cryptosys",
      "Type of cryptosystem to use to commit transactions in the SBFT "
      "optimistic fast path (threshold 3F + 2C + 1). This parameter should "
      "consist of two space-separated strings, the first of which names the "
      "cryptosystem type and the second of which is a type-specific parameter "
      "or subtype selection (for example, the second string might be an "
      "elliptic curve type if an elliptic curve cryptosystem is selected).",
      "multisig-bls BN-P254");
  config.tagParameter("optimistic_commit_cryptosys", defaultableByUtilityTags);
  config.tagParameter("optimistic_commit_cryptosys", secretsTag);

  config.declareParameter(
      "optimistic_commit_public_key",
      "Public key for the optimistic fast path commit cryptosystem.");
  config.tagParameter("optimistic_commit_public_key", publicGeneratedTags);
  config.addValidator(
      "optimistic_commit_public_key",
      make_shared<PublicKeyValidator>(auxState->optimisticCommitCryptosys));
  config.tagParameter("optimistic_commit_public_key", secretsTag);
  config.addGenerator("optimistic_commit_public_key",
                      make_shared<ThresholdPublicKeyLoader>(
                          auxState->optimisticCommitCryptosys));

  config.declareParameter(
      "pruning_enabled",
      "A flag to indicate if pruning is enabled for the replica. If set to "
      "false, LatestPrunableBlockRequest will return 0 as a latest block("
      "indicating no blocks can be pruned) and PruneRequest will return an "
      "error. If not specified, a value of false is assumed.");
  config.tagParameter("pruning_enabled", publicOptionalTags);
  config.tagParameter("pruning_enabled", applicationTag);
  config.addValidator("pruning_enabled", make_shared<BooleanValidator>());

  config.declareParameter(
      "key_exchange_on_start",
      "A flag to indicate if key exchange on start is required.", "false");
  config.tagParameter("key_exchange_on_start", publicDefaultableTags);
  config.tagParameter("key_exchange_on_start", applicationTag);
  config.addValidator("key_exchange_on_start", make_shared<BooleanValidator>());

  config.declareParameter(
      "pruning_num_blocks_to_keep",
      "Minimum number of blocks to always keep in storage when pruning. If not "
      "specified, a value of 0 is assumed. If pruning_duration_to_keep_minutes "
      "is specified too, the more conservative pruning range will be used (the "
      "one that prunes less blocks).");
  config.tagParameter("pruning_num_blocks_to_keep", publicOptionalTags);
  config.tagParameter("pruning_num_blocks_to_keep", applicationTag);
  config.addValidator("pruning_num_blocks_to_keep",
                      make_shared<UIntValidator>(0, UINT64_MAX));

  config.declareParameter(
      "pruning_duration_to_keep_minutes",
      "Time range (in minutes) from now to the past that determines which "
      "blocks to keep and which are older than (now - "
      "pruning_duration_to_keep_minutes) and can, therefore, be pruned. If not "
      "specified, a value of 0 is assumed. If pruning_num_blocks_to_keep is "
      "specified too, the more conservative pruning range will be used (the "
      "one that prunes less blocks). This option requires the time service to "
      "be enabled.");
  config.tagParameter("pruning_duration_to_keep_minutes", publicOptionalTags);
  config.tagParameter("pruning_duration_to_keep_minutes", applicationTag);
  config.addValidator("pruning_duration_to_keep_minutes",
                      make_shared<UIntValidator>(0, UINT32_MAX));

  config.declareParameter(
      "pruning_operator_public_key",
      "Public key for the privileged operator authorized to issue pruning "
      "commands (pruning commands not including a valid signature made with "
      "the private key corresponding to this public key will be refused). This "
      "parameter is required if pruning is enabled, and is ignored otherwise.");
  config.tagParameter("pruning_operator_public_key", publicOptionalTags);
  config.tagParameter("pruning_operator_public_key", secretsTag);

  config.declareParameter(
      "slow_commit_cryptosys",
      "Type of cryptosystem to use to commit transactions in the SBFT slow "
      "path (threshold 2F + C + 1). This parameter should consist of two "
      "space-separated strings, the first of which names the cryptosystem type "
      "and the second of which is a type-specific parameter or subtype "
      "selectioin (for example, the second string might be an elliptic curve "
      "type if an elliptic curve cryptosystem is selected).",
      "threshold-bls BN-P254");
  config.tagParameter("slow_commit_cryptosys", defaultableByUtilityTags);
  config.tagParameter("slow_commit_cryptosys", secretsTag);

  config.declareParameter("slow_commit_public_key",
                          "Public key for the slow path commit cryptosystem.");
  config.tagParameter("slow_commit_public_key", publicGeneratedTags);
  config.tagParameter("slow_commit_public_key", secretsTag);
  config.addValidator(
      "slow_commit_public_key",
      make_shared<PublicKeyValidator>(auxState->slowCommitCryptosys));
  config.addGenerator(
      "slow_commit_public_key",
      make_shared<ThresholdPublicKeyLoader>(auxState->slowCommitCryptosys));

  config.declareParameter(
      "status_time_interval",
      "Time interval, measured in milliseconds, at which each Concord replica "
      "should send its status to the others.",
      "3000");
  config.tagParameter("status_time_interval", defaultableByUtilityTags);
  config.tagParameter("status_time_interval", applicationTag);
  config.addValidator("status_time_interval",
                      make_shared<UIntValidator>(1, UINT16_MAX));

  config.declareParameter(
      "use_loopback_for_local_hosts",
      "If this parameter is set to true, Concord will expect to use the "
      "loopback IP (i.e. 127.0.0.1) for all host addresses used for internal "
      "Concord communication. Specifically, if this parameter is set to true, "
      "the configuration generation utility will replace hosts used in "
      "internal Concord communication with \"127.0.0.1\" in the configuration "
      "file belonging to the node on which that host is located; furthermore, "
      "when Concord nodes load their configuration, they will expect every "
      "host on their own node to have this IP address, and will reject their "
      "configuration otherwise. Note this parameter should not be set to true "
      "in deployments which do not guarantee that all hosts contained in a "
      "single Concord node are on the same machine such that they can reach "
      "each other via the loopback IP.",
      "false");
  config.tagParameter("use_loopback_for_local_hosts", defaultableByUtilityTags);
  config.tagParameter("use_loopback_for_local_hosts", applicationTag);
  config.addValidator("use_loopback_for_local_hosts",
                      make_shared<BooleanValidator>());

  config.declareParameter(
      "view_change_timeout",
      "Timeout, measured in milliseconds, after which Concord-BFT will attempt "
      "an SBFT view change if not enough replicas are responding.",
      "20000");
  config.tagParameter("view_change_timeout", applicationTag);
  config.tagParameter("view_change_timeout", defaultableByUtilityTags);
  config.addValidator("view_change_timeout",
                      make_shared<UIntValidator>(1, UINT16_MAX));

  config.declareParameter(
      "FEATURE_time_service",
      "Enable the Time Service, and switch Ethereum to using it.", "false");
  config.tagParameter("FEATURE_time_service", applicationTag);
  config.tagParameter("FEATURE_time_service", publicDefaultableTags);
  config.addValidator("FEATURE_time_service", make_shared<BooleanValidator>());

  config.declareParameter(
      "time_verification",
      "What mechanism to use, if any, to verify received time samples "
      "allegedly from configured time sources are legitimate and not forgeries "
      "by a malicious or otherwise Byzantine-faulty party impersonating a time "
      "source. Currently supported time verification methods are: "
      "\"rsa-time-signing\", \"bft-client-proxy-id\", and \"none\". If "
      "\"rsa-time-signing\" is selected, each time source will sign its time "
      "updates with its replica's RSA private key to prove the sample's "
      "legitimacy; these signatures will be transmitted and recorded with each "
      "time sample. If \"bft-client-proxy-id\" is selected, the time contract "
      "will scrutinize the Concord-BFT client proxy ID submitting each time "
      "update and check it matches the node for the claimed time source "
      "(Concord-BFT should guarantee it is intractable to impersonate client "
      "proxies to it without that proxy's private key). If \"none\" is "
      "selected, no verification of received time samples will be used (this "
      "is NOT recommended for production deployments).",
      "none");
  config.tagParameter("time_verification", publicDefaultableTags);
  config.tagParameter("time_verification", applicationTag);
  config.addValidator(
      "time_verification",
      make_shared<EnumeratedOptionValidator>(timeVerificationOptions));

  config.declareParameter(
      "eth_enable",
      "Enable Ethereum support. At the moment, DAML/Eth/HLF/TEE/Perf "
      "support are mutually exclusive.",
      "true");
  config.tagParameter("eth_enable", publicDefaultableTags);
  config.tagParameter("eth_enable", deploymentTag);
  config.addValidator("eth_enable", make_shared<BooleanValidator>());

  config.declareParameter(
      "daml_enable",
      "Enable DAML support. At the moment, DAML/Eth/HLF/TEE "
      "support are mutually exclusive.",
      "false");
  config.tagParameter("daml_enable", publicDefaultableTags);
  config.tagParameter("daml_enable", deploymentTag);
  config.addValidator("daml_enable", make_shared<BooleanValidator>());

  config.declareParameter("pre_execute_all_requests",
                          "Enable pre-execution for all requests", "false");
  config.tagParameter("pre_execute_all_requests", publicDefaultableTags);
  config.tagParameter("pre_execute_all_requests", applicationTag);
  config.addValidator("pre_execute_all_requests",
                      make_shared<BooleanValidator>());

  node.declareParameter("daml_service_addr",
                        "IP address and port (<IP>:<PORT>) on which Concord's "
                        "DAML service can be reached.",
                        "0.0.0.0:50051");
  node.tagParameter("daml_service_addr", defaultableByReplicaTags);
  node.tagParameter("daml_service_addr", deploymentTag);

  node.declareParameter("daml_execution_engine_addr",
                        "IP address and port (<IP>:<PORT>) to reach DAMLe. "
                        "Concord is a client to DAML's execution engine.",
                        "0.0.0.0:55000");
  node.tagParameter("daml_execution_engine_addr", defaultableByReplicaTags);
  node.tagParameter("daml_execution_engine_addr", deploymentTag);

  // If the worker pool is exhausted then the gRPC server will return
  // RESOURCE_EXHAUSTED.
  node.declareParameter("daml_service_threads",
                        "Number of threads to be used by the gRPC server.",
                        "32");
  node.tagParameter("daml_service_threads", defaultableByReplicaTags);
  node.tagParameter("daml_service_threads", applicationTag);
  node.addValidator("daml_service_threads",
                    make_shared<UIntValidator>(0, UINT16_MAX));

  node.declareParameter("FEATURE_daml_pipelined_commits",
                        "Enable support for pipelined commits, i.e., "
                        "interleaving read/writes by the submission validator.",
                        "true");
  node.tagParameter("FEATURE_daml_pipelined_commits", publicOptionalTags);
  node.tagParameter("FEATURE_daml_pipelined_commits", applicationTag);
  node.addValidator("FEATURE_daml_pipelined_commits",
                    make_shared<BooleanValidator>());

  // Test Execution Engine (TEE) Parameters
  config.declareParameter(
      "tee_enable",
      "Enable Test Execution Engine support. At the moment, "
      "DAML/Eth/HLF/TEE/Perf support are mutually exclusive.",
      "false");
  config.tagParameter("tee_enable", publicDefaultableTags);
  config.tagParameter("tee_enable", deploymentTag);
  config.addValidator("tee_enable", make_shared<BooleanValidator>());

  config.declareParameter(
      "create_tee_genesis_block",
      "Whether or not to create a genesis block on tee blockchain. This "
      "parameter is for test environments only. E.g. run linearizability "
      "tests against tee blockchain.",
      "true");
  config.tagParameter("create_tee_genesis_block", publicDefaultableTags);
  config.tagParameter("create_tee_genesis_block", deploymentTag);
  config.addValidator("create_tee_genesis_block",
                      make_shared<BooleanValidator>());

  node.declareParameter("tee_service_addr",
                        "IP address and port (<IP>:<PORT>) on which Concord's "
                        "TEE service can be reached.",
                        "0.0.0.0:50051");
  node.tagParameter("tee_service_addr", defaultableByReplicaTags);
  node.tagParameter("tee_service_addr", deploymentTag);

  node.declareParameter("tee_service_threads",
                        "Number of threads to be used by the TEE gRPC"
                        "server.",
                        "32");
  node.tagParameter("tee_service_threads", defaultableByReplicaTags);
  node.tagParameter("tee_service_threads", applicationTag);
  node.addValidator("tee_service_threads",
                    make_shared<UIntValidator>(0, UINT16_MAX));

  // Performance handler parameters
  config.declareParameter(
      "perf_enable",
      "Enable Performance Execution Engine support. At the moment, "
      "DAML/Eth/HLF/TEE/Perf support are mutually exclusive.",
      "false");
  config.tagParameter("perf_enable", publicDefaultableTags);
  config.tagParameter("perf_enable", deploymentTag);
  config.addValidator("perf_enable", make_shared<BooleanValidator>());

  node.declareParameter("perf_service_addr",
                        "IP address and port (<IP>:<PORT>) on which Concord's "
                        "Perforamance service can be reached.",
                        "0.0.0.0:50051");
  node.tagParameter("perf_service_addr", defaultableByReplicaTags);
  node.tagParameter("perf_service_addr", deploymentTag);

  node.declareParameter("perf_service_threads",
                        "Number of threads to be used by the Performance gRPC"
                        "server.",
                        "32");
  node.tagParameter("perf_service_threads", defaultableByReplicaTags);
  node.tagParameter("perf_service_threads", applicationTag);
  node.addValidator("perf_service_threads",
                    make_shared<UIntValidator>(0, UINT16_MAX));

  node.declareParameter(
      "bft_client_timeout_ms",
      "How long to wait for a command execution response, in milliseconds.",
      to_string(UINT32_MAX));
  node.tagParameter("bft_client_timeout_ms", defaultableByReplicaTags);
  node.tagParameter("bft_client_timeout_ms", applicationTag);
  node.addValidator("bft_client_timeout_ms",
                    make_shared<UIntValidator>(0, UINT64_MAX));

  node.declareParameter("api_worker_pool_size",
                        "Number of threads to create to handle TCP connections "
                        "to this node's external API.",
                        "3");
  node.tagParameter("api_worker_pool_size", defaultableByReplicaTags);
  node.tagParameter("api_worker_pool_size", applicationTag);
  node.addValidator("api_worker_pool_size",
                    make_shared<PositiveReplicaIntValidator>());

  auto blockchain_db_impl_param = [&](ConcordConfiguration& c) {
    c.declareParameter("blockchain_db_impl",
                       "Database implementation to be used by this replica to "
                       "persist blockchain state.",
                       "rocksdb");
    c.tagParameter("blockchain_db_impl", defaultableByReplicaTags);
    c.tagParameter("blockchain_db_impl", applicationTag);
    c.addValidator("blockchain_db_impl",
                   make_shared<DatabaseImplementationValidator>());
  };
  blockchain_db_impl_param(node);
  blockchain_db_impl_param(ro_node);

  auto blockchain_db_path_param = [&](ConcordConfiguration& c) {
    c.declareParameter(
        "blockchain_db_path",
        "Path to storage to use to persist blockchain data for this replica "
        "using the database implementation specified by blockchain_db_impl.",
        "rocksdbdata");
    c.tagParameter("blockchain_db_path", applicationTag);
    c.tagParameter("blockchain_db_path", defaultableByReplicaTags);
  };
  blockchain_db_path_param(node);
  blockchain_db_path_param(ro_node);

  node.declareParameter(
      "blockchain_storage_type",
      "The mechanism for storing blockchain data on top of the key/value "
      "store. Possible values are: \"merkle\" and \"basic\"",
      "merkle");
  node.tagParameter("blockchain_storage_type", publicOptionalTags);
  node.tagParameter("blockchain_storage_type", applicationTag);
  node.addValidator("blockchain_storage_type",
                    make_shared<StorageTypeValidator>());

  ro_node.declareParameter("s3-bucket-name",
                           "S3 bucket name used by the object store.");
  ro_node.tagParameter("s3-bucket-name", publicInputTags);
  ro_node.tagParameter("s3-bucket-name", deploymentTag);

  ro_node.declareParameter("s3-access-key", "Access key for the S3 service.");
  ro_node.tagParameter("s3-access-key", privateInputTags);
  ro_node.tagParameter("s3-access-key", deploymentTag);

  ro_node.declareParameter("s3-secret-key", "Secret key for the S3 service.");
  ro_node.tagParameter("s3-secret-key", publicInputTags);
  ro_node.tagParameter("s3-secret-key", deploymentTag);

  ro_node.declareParameter("s3-protocol",
                           "Protocol used to access the S3 service.");
  ro_node.tagParameter("s3-protocol", publicInputTags);
  ro_node.tagParameter("s3-protocol", deploymentTag);

  ro_node.declareParameter("s3-url", "URL for the S3 service.");
  ro_node.tagParameter("s3-url", publicInputTags);
  ro_node.tagParameter("s3-url", deploymentTag);

  node.declareParameter(
      "concord-bft_enable_debug_statistics",
      "If set to true, Concord-BFT will periodically log debug statistics for "
      "this Concord node, such as throughput metrics and number of messages "
      "sent/received.",
      "false");
  node.tagParameter("concord-bft_enable_debug_statistics",
                    defaultableByReplicaTags);
  node.tagParameter("concord-bft_enable_debug_statistics", applicationTag);
  node.addValidator("concord-bft_enable_debug_statistics",
                    make_shared<BooleanValidator>());

  node.declareParameter(
      "genesis_block",
      "Path, in the node's local filesystem, to a JSON file containing the "
      "genesis block data for this blockchain.");
  node.tagParameter("genesis_block", privateOptionalTags);
  node.tagParameter("genesis_block", applicationTag);

  auto logger_config_param = [&](ConcordConfiguration& c) {
    c.declareParameter("logger_config",
                       "Path, in this node's local filesystem to a "
                       "configuration for Log4CPlus, "
                       "the logging framework Concord uses.",
                       "/concord/resources/log4cplus.properties");
    c.tagParameter("logger_config", defaultableByReplicaTags);
    c.tagParameter("logger_config", applicationTag);
  };
  logger_config_param(node);
  logger_config_param(ro_node);

  auto current_node_param = [&](ConcordConfiguration& c) {
    c.declareParameter("current_node",
                       "hint to the current node in the configuration file.",
                       "true");
    c.tagParameter("current_node", defaultableByReplicaTags);
    c.tagParameter("current_node", deploymentTag);
    c.tagParameter("current_node", secretsTag);
    c.tagParameter("current_node", applicationTag);
  };
  current_node_param(node);
  current_node_param(ro_node);

  auto logger_reconfig_time_param = [&](ConcordConfiguration& c) {
    c.declareParameter(
        "logger_reconfig_time",
        "Interval, measured in milliseconds, with which this replica should "
        "check the file specified by logger_config for changes in requested "
        "logging behavior.",
        "60000");
    c.tagParameter("logger_reconfig_time", defaultableByReplicaTags);
    c.tagParameter("logger_reconfig_time", applicationTag);
    c.addValidator("logger_reconfig_time",
                   make_shared<PositiveReplicaIntValidator>());
  };
  logger_reconfig_time_param(node);
  logger_reconfig_time_param(ro_node);

  node.declareParameter(
      "jaeger_agent",
      "Host:Port of the jaeger-agent process to receive traces "
      "(127.0.0.1:6831 by default).");
  node.tagParameter("jaeger_agent", privateOptionalTags);
  node.tagParameter("jaeger_agent", deploymentTag);

  node.declareParameter("prometheus_port",
                        "Port of prometheus client to publish metrics on "
                        "(9891 by default).");
  node.tagParameter("prometheus_port", privateOptionalTags);
  node.tagParameter("prometheus_port", deploymentTag);

  node.declareParameter(
      "enable_histograms_or_summaries",
      "Indicates whether to enable concord to collect statistics in histograms "
      "or summaries as this may impose performance costs on the system",
      "true");
  node.tagParameter("enable_histograms_or_summaries", publicDefaultableTags);
  node.tagParameter("enable_histograms_or_summaries", deploymentTag);

  node.declareParameter("dump_metrics_interval_sec",
                        "Time interval for dumping concord metrics to log "
                        "(600 seconds by default).");
  node.tagParameter("dump_metrics_interval_sec", privateOptionalTags);
  node.tagParameter("dump_metrics_interval_sec", applicationTag);

  config.declareParameter(
      "preexecution_enabled",
      "A flag to indicate if pre-execution feature is enabled for the replica.",
      "false");
  config.tagParameter("preexecution_enabled", publicDefaultableTags);
  config.tagParameter("preexecution_enabled", applicationTag);
  config.addValidator("preexecution_enabled", make_shared<BooleanValidator>());

  config.declareParameter("preexec_requests_status_check_period_millisec",
                          "Time interval for a periodic detection of timed out "
                          "pre-execution requests "
                          "(1000 milliseconds by default).",
                          "1000");
  config.tagParameter("preexec_requests_status_check_period_millisec",
                      publicDefaultableTags);
  config.tagParameter("preexec_requests_status_check_period_millisec",
                      applicationTag);
  config.addValidator("preexec_requests_status_check_period_millisec",
                      make_shared<UIntValidator>(1, UINT64_MAX));

  config.declareParameter("preexec_concurrency_level",
                          "A number of threads to be used to "
                          "parallelize pre-execution requests (0 by default).",
                          "0");
  config.tagParameter("preexec_concurrency_level", publicDefaultableTags);
  config.tagParameter("preexec_concurrency_level", applicationTag);
  config.addValidator("preexec_concurrency_level",
                      make_shared<UIntValidator>(0, UINT16_MAX));

  auto service_host_param = [&](ConcordConfiguration& c) {
    c.declareParameter("service_host",
                       "Public IP address or hostname on which this replica's "
                       "external API service can be reached.");
    c.tagParameter("service_host", privateInputTags);
    c.tagParameter("service_host", deploymentTag);
  };
  service_host_param(node);
  service_host_param(ro_node);

  auto service_port_param = [&](ConcordConfiguration& c) {
    c.declareParameter(
        "service_port",
        "Port on which this replica's external API service can be reached.");
    c.tagParameter("service_port", privateInputTags);
    c.tagParameter("service_port", deploymentTag);
    c.addValidator("service_port", make_shared<PortNumberValidator>());
  };
  service_port_param(node);
  service_port_param(ro_node);

  auto bft_metrics_udp_port_param = [&](ConcordConfiguration& c) {
    c.declareParameter("bft_metrics_udp_port",
                       "Port for reading BFT metrics (JSON payload) via UDP.");
    c.tagParameter("bft_metrics_udp_port", privateOptionalTags);
    c.tagParameter("bft_metrics_udp_port", deploymentTag);
    c.addValidator("bft_metrics_udp_port", make_shared<PortNumberValidator>());
  };
  bft_metrics_udp_port_param(node);
  bft_metrics_udp_port_param(ro_node);

  node.declareParameter(
      "transaction_list_max_count",
      "Maximum number of transactions to allow this replica to return to "
      "queries to its public API service requesting lists of transactions.",
      "10");
  node.tagParameter("transaction_list_max_count", defaultableByReplicaTags);
  node.tagParameter("transaction_list_max_count", applicationTag);
  node.addValidator("transaction_list_max_count",
                    make_shared<PositiveReplicaIntValidator>());

  node.declareParameter(
      "time_source_id",
      "The source name `time-sourceX` is based on the node index."
      "Ignored unless FEATURE_time_service is \"true\".");
  node.tagParameter("time_source_id", publicGeneratedTags);
  node.tagParameter("time_source_id", deploymentTag);
  node.addGenerator("time_source_id", make_shared<TimeSourceIdCalculator>());

  node.declareParameter(
      "time_pusher_period_ms",
      "How often a node should guarantee that its time is published, in "
      "milliseconds. Ignored unless FEATURE_time_service is \"true\", and "
      "time_source_id is given.");
  node.tagParameter("time_pusher_period_ms", publicOptionalTags);
  node.tagParameter("time_pusher_period_ms", applicationTag);
  node.addValidator("time_pusher_period_ms",
                    make_shared<IntValidator>(INT32_MIN, INT32_MAX));

  replica.declareParameter("commit_private_key",
                           "Private key for this replica under the general "
                           "case commit cryptosystem.");
  replica.tagParameter("commit_private_key", privateGeneratedTags);
  replica.tagParameter("commit_private_key", secretsTag);
  replica.addValidator("commit_private_key", make_shared<PrivateKeyValidator>(
                                                 auxState->commitCryptosys));
  replica.addGenerator(
      "commit_private_key",
      make_shared<ThresholdPrivateKeyLoader>(auxState->commitCryptosys));

  replica.declareParameter(
      "commit_verification_key",
      "Public verification key for this replica's signature under the general "
      "case commit cryptosystem.");
  replica.tagParameter("commit_verification_key", publicGeneratedTags);
  replica.tagParameter("commit_verification_key", secretsTag);
  replica.addValidator(
      "commit_verification_key",
      make_shared<VerificationKeyValidator>(auxState->commitCryptosys));
  replica.addGenerator(
      "commit_verification_key",
      make_shared<ThresholdVerificationKeyLoader>(auxState->commitCryptosys));

  replica.declareParameter("optimistic_commit_private_key",
                           "Private key for this replica under the optimistic "
                           "fast path commit cryptosystem.");
  replica.tagParameter("optimistic_commit_private_key", privateGeneratedTags);
  replica.tagParameter("optimistic_commit_private_key", secretsTag);
  replica.addValidator(
      "optimistic_commit_private_key",
      make_shared<PrivateKeyValidator>(auxState->optimisticCommitCryptosys));
  replica.addGenerator("optimistic_commit_private_key",
                       make_shared<ThresholdPrivateKeyLoader>(
                           auxState->optimisticCommitCryptosys));

  replica.declareParameter(
      "optimistic_commit_verification_key",
      "Public verification key for this replica's signature under the "
      "optimistic fast path commit cryptosystem.");
  replica.tagParameter("optimistic_commit_verification_key",
                       publicGeneratedTags);
  replica.tagParameter("optimistic_commit_verification_key", secretsTag);
  replica.addValidator("optimistic_commit_verification_key",
                       make_shared<VerificationKeyValidator>(
                           auxState->optimisticCommitCryptosys));
  replica.addGenerator("optimistic_commit_verification_key",
                       make_shared<ThresholdVerificationKeyLoader>(
                           auxState->optimisticCommitCryptosys));

  auto principal_id_param = [&](ConcordConfiguration& c) {
    c.declareParameter(
        "principal_id",
        "Unique ID number for this Concord-BFT replica. Concord-BFT considers "
        "replicas and client proxies to be principals, each of which must have "
        "a "
        "unique ID.");
    c.tagParameter("principal_id", publicGeneratedTags);
    c.tagParameter("principal_id", deploymentTag);
    c.tagParameter("principal_id", secretsTag);
    c.addValidator("principal_id", make_shared<PrincipalIdValidator>());
    c.addGenerator("principal_id", make_shared<PrincipalIdCalculator>());
  };
  principal_id_param(replica);
  principal_id_param(ro_node);

  auto private_key_param = [&](ConcordConfiguration& c) {
    c.declareParameter("private_key",
                       "RSA private key for this replica to use for "
                       "general communication.");
    c.tagParameter("private_key", privateGeneratedTags);
    c.tagParameter("private_key", secretsTag);
    c.addValidator("private_key", make_shared<RSAPrivateKeyValidator>());
    c.addGenerator("private_key", make_shared<RSAPrivateKeyLoader>());
  };
  private_key_param(replica);
  private_key_param(ro_node);

  replica.declareParameter(
      "public_key",
      "RSA public key corresponding to this replica's RSA private key.");
  replica.tagParameter("public_key", publicGeneratedTags);
  replica.tagParameter("public_key", secretsTag);
  replica.addValidator("public_key", make_shared<RSAPublicKeyValidator>());
  replica.addGenerator("public_key", make_shared<RSAPublicKeyLoader>());

  replica.declareParameter(
      "replica_host",
      "Public IP address or host name with which other replicas can reach this "
      "one for consensus communication.");
  replica.tagParameter("replica_host", principalHostTags);
  replica.tagParameter("replica_host", deploymentTag);
  replica.addValidator("replica_host", make_shared<PrincipalHostValidator>());

  replica.declareParameter("replica_port",
                           "Port number on which other replicas can reach this "
                           "one for consensus communication.");
  replica.tagParameter("replica_port", publicInputTags);
  replica.tagParameter("replica_port", deploymentTag);
  replica.addValidator("replica_port", make_shared<PortNumberValidator>());

  replica.declareParameter(
      "slow_commit_private_key",
      "Private key for this replica under the slow path commit cryptosystem.");
  replica.tagParameter("slow_commit_private_key", privateGeneratedTags);
  replica.tagParameter("slow_commit_private_key", secretsTag);
  replica.addValidator(
      "slow_commit_private_key",
      make_shared<PrivateKeyValidator>(auxState->slowCommitCryptosys));
  replica.addGenerator(
      "slow_commit_private_key",
      make_shared<ThresholdPrivateKeyLoader>(auxState->slowCommitCryptosys));

  replica.declareParameter(
      "slow_commit_verification_key",
      "Public verification key for this replica's signature under the slow "
      "path commit cryptosystem.");
  replica.tagParameter("slow_commit_verification_key", publicGeneratedTags);
  replica.tagParameter("slow_commit_verification_key", secretsTag);

  replica.addValidator(
      "slow_commit_verification_key",
      make_shared<VerificationKeyValidator>(auxState->slowCommitCryptosys));
  replica.addGenerator("slow_commit_verification_key",
                       make_shared<ThresholdVerificationKeyLoader>(
                           auxState->slowCommitCryptosys));

  clientProxy.declareParameter("client_host",
                               "Public IP address or host name with which this "
                               "client proxy can be reached.");
  clientProxy.tagParameter("client_host", principalHostTags);
  clientProxy.tagParameter("client_host", deploymentTag);
  clientProxy.addValidator("client_host",
                           make_shared<PrincipalHostValidator>());

  clientProxy.declareParameter(
      "client_port", "Port on which this client proxy can be reached.");
  clientProxy.tagParameter("client_port", publicInputTags);
  clientProxy.tagParameter("client_port", deploymentTag);
  clientProxy.addValidator("client_port", make_shared<PortNumberValidator>());

  clientProxy.declareParameter(
      "principal_id",
      "Unique ID number for client proxy. Concord-BFT considers both replicas "
      "and client proxies to be principals, and it requires all principals "
      "have a unique ID.");
  clientProxy.tagParameter("principal_id", publicGeneratedTags);
  clientProxy.tagParameter("principal_id", deploymentTag);
  clientProxy.tagParameter("principal_id", secretsTag);
  clientProxy.addValidator("principal_id", make_shared<PrincipalIdValidator>());
  clientProxy.addGenerator("principal_id",
                           make_shared<PrincipalIdCalculator>());

  // Configuration of HLF
  config.declareParameter(
      "hlf_enable",
      "Enable HLF support. At the moment, DAML/Eth/HLF/TEE/Perf "
      "support are mutually exclusive.",
      "false");
  config.tagParameter("hlf_enable", publicDefaultableTags);
  config.tagParameter("hlf_enable", deploymentTag);
  config.addValidator("hlf_enable", make_shared<BooleanValidator>());

  node.declareParameter("hlf_peer_command_tool_path",
                        "Location of peer command tool.", "/concord/peer");
  node.tagParameter("hlf_peer_command_tool_path", defaultableByReplicaTags);
  node.tagParameter("hlf_peer_command_tool_path", applicationTag);

  node.declareParameter("hlf_peer_command_tool_config_path",
                        "Config file for peer command tool.", "/concord");
  node.tagParameter("hlf_peer_command_tool_config_path",
                    defaultableByReplicaTags);
  node.tagParameter("hlf_peer_command_tool_config_path", applicationTag);

  node.declareParameter("hlf_peer_msp_dir_path",
                        "Location of Membership Service Provider directory.",
                        "/concord/crypto-config/peerOrganizations/"
                        "org1.example.com/users/Admin@org1.example.com/msp");
  node.tagParameter("hlf_peer_msp_dir_path", defaultableByReplicaTags);
  node.tagParameter("hlf_peer_msp_dir_path", applicationTag);

  node.declareParameter("hlf_peer_msp_id",
                        "MSP ID used to communicate with HLF peer.", "Org1MSP");
  node.tagParameter("hlf_peer_msp_id", defaultableByReplicaTags);
  node.tagParameter("hlf_peer_msp_id", deploymentTag);

  node.declareParameter("hlf_peer_address",
                        "Public IP address of HLF peer to communicate with "
                        "(chaincode life cycle managment).",
                        "peer1.org1.example.com:7051");
  node.tagParameter("hlf_peer_address", defaultableByReplicaTags);
  node.tagParameter("hlf_peer_address", deploymentTag);

  node.declareParameter("hlf_orderer_address",
                        "Public IP address of HLF orderer to communicate with "
                        "(channel management).",
                        "orderer1.example.com:7050");
  node.tagParameter("hlf_orderer_address", defaultableByReplicaTags);
  node.tagParameter("hlf_orderer_address", deploymentTag);

  node.declareParameter("hlf_kv_service_address",
                        "Address of Concord to provide KV service"
                        "GoLang Peer connects to concord.",
                        "0.0.0.0:50052");
  node.tagParameter("hlf_kv_service_address", defaultableByReplicaTags);
  node.tagParameter("hlf_kv_service_address", deploymentTag);

  node.declareParameter("hlf_chaincode_service_address",
                        "IP address and port (<IP>:<PORT>) on which Concord's "
                        "HLF chaincode service can be reached.",
                        "0.0.0.0:50051");
  node.tagParameter("hlf_chaincode_service_address", defaultableByReplicaTags);
  node.tagParameter("hlf_chaincode_service_address", deploymentTag);

  node.declareParameter("hlf_chaincode_path",
                        "Directory to store temporary chaincode file, "
                        "this must be the $GOPATH/src for chaincode in golang",
                        "/concord/src");
  node.tagParameter("hlf_chaincode_path", defaultableByReplicaTags);
  node.tagParameter("hlf_chaincode_path", applicationTag);

  // TLS
  config.declareParameter("tls_cipher_suite_list",
                          "TLS cipher suite list to use");
  config.tagParameter("tls_cipher_suite_list", publicInputTags);
  config.tagParameter("tls_cipher_suite_list", secretsTag);
  config.declareParameter("tls_certificates_folder_path",
                          "TLS certificates root folder path");
  config.tagParameter("tls_certificates_folder_path", publicInputTags);
  config.tagParameter("tls_certificates_folder_path", applicationTag);
  config.declareParameter("signing_key_path", "Signing key root folder path");
  config.tagParameter("signing_key_path", publicOptionalTags);
  config.tagParameter("signing_key_path", applicationTag);

  config.declareParameter("comm_to_use", "Default communication module");
  config.tagParameter("comm_to_use", publicInputTags);
  config.tagParameter("comm_to_use", applicationTag);
}

void loadClusterSizeParameters(YAMLConfigurationInput& input,
                               ConcordConfiguration& config, bool isClient) {
  Logger logger = Logger::getInstance("concord.configuration");

  ConfigurationPath fValPath("f_val", false);
  ConfigurationPath cValPath("c_val", false);
  ConfigurationPath roReplicasValPath("num_ro_replicas", false);
  vector<ConfigurationPath> requiredParameters(
      {fValPath, cValPath, roReplicasValPath});
  if (isClient) {
    ConfigurationPath participant_nodes("num_of_participant_nodes", false);
    requiredParameters.push_back(std::move(participant_nodes));
    ConfigurationPath externalClients("clients_per_participant_node", false);
    requiredParameters.push_back(std::move(externalClients));
  } else {
    ConfigurationPath clientProxiesPerReplicaPath("client_proxies_per_replica",
                                                  false);
    requiredParameters.push_back(std::move(clientProxiesPerReplicaPath));
  }
  input.loadConfiguration(config, requiredParameters.begin(),
                          requiredParameters.end(), &logger, true);

  bool missingValue = false;
  for (auto&& parameter : requiredParameters) {
    if (!config.hasValue<uint16_t>(parameter)) {
      missingValue = true;
      LOG_ERROR(logger,
                "Value not found for required cluster sizing parameter: " +
                    parameter.toString());
    }
  }
  if (missingValue) {
    throw ConfigurationResourceNotFoundException(
        "Cannot load cluster size parameters: missing required cluster size "
        "parameter.");
  }
}

// ParameterSelectors used by instantiateTemplatedConfiguration to select
// subsets of parameter paths in order to correctly load template contents
// before instantiating templates.
class StrictlyTemplatedParametersSelector
    : public ParameterSelection::ParameterSelector {
 public:
  virtual ~StrictlyTemplatedParametersSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    if (!path.isScope || path.useInstance) {
      return false;
    }
    const ConfigurationPath* pathStep = &path;
    while (pathStep->isScope && pathStep->subpath) {
      if (pathStep->useInstance) {
        return false;
      }
      pathStep = pathStep->subpath.get();
    }
    return true;
  }
};

class TemplatedInstancedParametersSelector
    : public ParameterSelection::ParameterSelector {
 public:
  virtual ~TemplatedInstancedParametersSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    return (path.isScope && !path.useInstance && path.subpath &&
            path.subpath->isScope && path.subpath->useInstance &&
            path.subpath->subpath && !(path.subpath->subpath->isScope));
  }
};

class InstancedTemplatedParametersSelector
    : public ParameterSelection::ParameterSelector {
 public:
  virtual ~InstancedTemplatedParametersSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    return (path.isScope && path.useInstance && path.subpath &&
            path.subpath->isScope && !(path.subpath->useInstance) &&
            path.subpath->subpath && !(path.subpath->subpath->isScope));
  }
};

class InstancedInstancedParametersSelector
    : public ParameterSelection::ParameterSelector {
 public:
  virtual ~InstancedInstancedParametersSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    return (path.isScope && path.useInstance && path.subpath &&
            path.subpath->isScope && path.subpath->useInstance &&
            path.subpath->subpath && !(path.subpath->subpath->isScope));
  }
};

void instantiateTemplatedConfiguration(YAMLConfigurationInput& input,
                                       ConcordConfiguration& config) {
  Logger logger = Logger::getInstance("concord.configuration");

  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    throw ConfigurationResourceNotFoundException(
        "Cannot instantiate scopes for Concord configuration: required cluster "
        "size parameters are not loaded.");
  }

  assert(config.containsScope("node"));
  ConcordConfiguration& node = config.subscope("node");

  assert(node.containsScope("replica"));
  assert(node.containsScope("client_proxy"));

  // Note this function is complicated by the fact that it handles loading the
  // contents of templates before instantiating them as well as the fact that
  // the input could contain parameters in a mixed state of being template or
  // instance parameters (for example, the input could give a value of the port
  // number for the first client proxy on each node).

  // First, load any parameters in purely templated scopes, then instantiate the
  // scopes within the node template.
  ParameterSelection selection(
      config, make_shared<StrictlyTemplatedParametersSelector>());
  input.loadConfiguration(config, selection.begin(), selection.end(), &logger,
                          true);
  node.instantiateScope("replica");
  node.instantiateScope("client_proxy");

  // Next, load values for parameters in instances of scopes within the node
  // template. After that node can be instantiated.
  selection = ParameterSelection(
      config, make_shared<TemplatedInstancedParametersSelector>());
  input.loadConfiguration(config, selection.begin(), selection.end(), &logger,
                          true);
  config.instantiateScope("node");
  config.instantiateScope("ro_node");

  // Now, we load values to parameters in scope templates within node instances.
  selection = ParameterSelection(
      config, make_shared<InstancedTemplatedParametersSelector>());
  input.loadConfiguration(config, selection.begin(), selection.end(), &logger,
                          true);

  // Finally, to enforce the policy that explicit instanced parameter
  // specifications override values from their templates, we traverse the set of
  // parameters that are contained in errrinstances of scopes within node
  // instances, and write to them any values their node instance's template has
  // for the same parameter.
  selection = ParameterSelection(
      config, make_shared<InstancedInstancedParametersSelector>());
  for (auto iterator = selection.begin(); iterator != selection.end();
       ++iterator) {
    ConfigurationPath instancePath = *iterator;
    ConfigurationPath templatePath(instancePath);
    templatePath.subpath->useInstance = false;
    ConfigurationPath containingScopeOfInstancePath(instancePath);
    containingScopeOfInstancePath.subpath->subpath.reset();

    if (config.hasValue<string>(templatePath)) {
      string value = config.getValue<string>(templatePath);
      ConcordConfiguration& subscope =
          config.subscope(containingScopeOfInstancePath);
      string failureMessage;
      if (subscope.loadValue(instancePath.subpath->subpath->name, value,
                             &failureMessage, true) ==
          ConcordConfiguration::ParameterStatus::INVALID) {
        LOG_ERROR(logger, "Cannot load value " + value + " to parameter " +
                              instancePath.toString() + ": " + failureMessage);
      }
    }
  }
}

void instantiateClientTemplatedConfiguration(YAMLConfigurationInput& input,
                                             ConcordConfiguration& config) {
  Logger logger = Logger::getInstance("concord.configuration");

  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("num_of_participant_nodes")) {
    throw ConfigurationResourceNotFoundException(
        "Cannot instantiate scopes for Concord configuration: required cluster "
        "size parameters are not loaded.");
  }

  assert(config.containsScope("node"));
  ConcordConfiguration& node = config.subscope("node");
  ConcordConfiguration& part_nodes = config.subscope("participant_nodes");
  ConcordConfiguration& part_node = part_nodes.subscope("participant_node");
  ConcordConfiguration& clients = part_node.subscope("external_clients");
  assert(node.containsScope("replica"));
  assert(part_nodes.containsScope("participant_node"));

  // Note this function is complicated by the fact that it handles loading the
  // contents of templates before instantiating them as well as the fact that
  // the input could contain parameters in a mixed state of being template or
  // instance parameters (for example, the input could give a value of the port
  // number for the first client proxy on each node).

  // First, load any parameters in purely templated scopes, then instantiate the
  // scopes within the node template.
  ParameterSelection selection(
      config, make_shared<StrictlyTemplatedParametersSelector>());
  input.loadConfiguration(config, selection.begin(), selection.end(), &logger,
                          true);
  clients.instantiateScope("client");
  node.instantiateScope("replica");

  // Next, load values for parameters in instances of scopes within the node
  // template. After that node can be instantiated.
  selection = ParameterSelection(
      config, make_shared<TemplatedInstancedParametersSelector>());
  input.loadConfiguration(config, selection.begin(), selection.end(), &logger,
                          true);
  part_node.instantiateScope("external_clients");
  part_nodes.instantiateScope("participant_node");
  config.instantiateScope("node");
  config.instantiateScope("participant_nodes");

  // Now, we load values to parameters in scope templates within node instances.
  selection = ParameterSelection(
      config, make_shared<InstancedTemplatedParametersSelector>());
  input.loadConfiguration(config, selection.begin(), selection.end(), &logger,
                          true);

  // Finally, to enforce the policy that explicit instanced parameter
  // specifications override values from their templates, we traverse the set of
  // parameters that are contained in errrinstances of scopes within node
  // instances, and write to them any values their node instance's template has
  // for the same parameter.
  selection = ParameterSelection(
      config, make_shared<InstancedInstancedParametersSelector>());
  for (auto iterator = selection.begin(); iterator != selection.end();
       ++iterator) {
    ConfigurationPath instancePath = *iterator;
    ConfigurationPath templatePath(instancePath);
    templatePath.subpath->useInstance = false;
    ConfigurationPath containingScopeOfInstancePath(instancePath);
    containingScopeOfInstancePath.subpath->subpath.reset();

    if (config.hasValue<string>(templatePath)) {
      string value = config.getValue<string>(templatePath);
      ConcordConfiguration& subscope =
          config.subscope(containingScopeOfInstancePath);
      string failureMessage;
      if (subscope.loadValue(instancePath.subpath->subpath->name, value,
                             &failureMessage, true) ==
          ConcordConfiguration::ParameterStatus::INVALID) {
        LOG_ERROR(logger, "Cannot load value " + value + " to parameter " +
                              instancePath.toString() + ": " + failureMessage);
      }
    }
  }
}

// Helper function used in error reporting by a number of configuration-loading
// functions that can possibly throw exceptions reporting multiple missing
// parameters at once; it may be helpful to list them all in the exception
// message instead of or in addition to logging each missing parameter
// individually as it can be possible for log statements to get loast from the
// output if they do not complete and their message does not get flushed to the
// output stream(s) before an exception following them triggers the program to
// exit.
static string getErrorMessageListingParameters(
    const string& base_error_message,
    const vector<string>& parameters_missing) {
  string error_message = base_error_message;
  for (size_t i = 0; i < parameters_missing.size(); ++i) {
    error_message += parameters_missing[i];
    if (i < (parameters_missing.size() - 1)) {
      error_message += ", ";
    } else {
      error_message += ".";
    }
  }
  return error_message;
}

// Parameter selector object used by loadConfigurationInputParameters.
class InputParametersSelector : public ParameterSelection::ParameterSelector {
 public:
  virtual ~InputParametersSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    assert(config.contains(path));
    const ConcordConfiguration* containingScope = &config;
    if (path.isScope) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }
    string name = path.getLeaf().name;

    return containingScope->isTagged(name, "input") ||
           containingScope->isTagged(name, "defaultable") ||
           containingScope->isTagged(name, "optional");
  }
};

void loadConfigurationInputParameters(YAMLConfigurationInput& input,
                                      ConcordConfiguration& config) {
  Logger logger = Logger::getInstance("concord.configuration");

  ParameterSelection inputParameterSelection(
      config, make_shared<InputParametersSelector>());
  input.loadConfiguration(config, inputParameterSelection.begin(),
                          inputParameterSelection.end(), &logger, true);

  bool missingParameter = false;
  vector<string> parameters_missing;
  for (auto iterator =
           config.begin(ConcordConfiguration::kIterateAllInstanceParameters);
       iterator !=
       config.end(ConcordConfiguration::kIterateAllInstanceParameters);
       ++iterator) {
    ConfigurationPath path = *iterator;
    const ConcordConfiguration* containingScope = &config;
    if (path.isScope) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }
    string name = path.getLeaf().name;
    if (containingScope->isTagged(name, "input") &&
        !containingScope->isTagged(name, "optionalInput") &&
        containingScope->isTagged(name, config.confType_) &&
        !config.hasValue<string>(path)) {
      missingParameter = true;
      parameters_missing.push_back(path.toString());
      LOG_ERROR(logger,
                "Configuration input is missing value for required input "
                "parameter: " +
                    path.toString());
    }
  }
  if (missingParameter) {
    throw ConfigurationResourceNotFoundException(
        getErrorMessageListingParameters(
            "Configuration input is missing required input parameter(s): ",
            parameters_missing));
  }
}

void generateConfigurationKeys(ConcordConfiguration& config) {
  Logger logger = Logger::getInstance("concord.configuration");

  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica") ||
      !config.hasValue<uint16_t>("num_ro_replicas")) {
    throw ConfigurationResourceNotFoundException(
        "Cannot generate keys for Concord cluster: required cluster size "
        "parameters are not loaded.");
  }
  if (!config.hasValue<string>("slow_commit_cryptosys") ||
      !config.hasValue<string>("commit_cryptosys") ||
      !config.hasValue<string>("optimistic_commit_cryptosys")) {
    throw ConfigurationResourceNotFoundException(
        "Cannot generate keys for Concord cluster: required cryptosystem "
        "selections have not been loaded.");
  }

  // Although the validators for these cryptosystem selections should have been
  // run when the values were loaded for them, we check that the validators
  // accept the values again here in case any of them were previously unable to
  // fully validate a cryptosystem selection because cryptosystem selections
  // were loaded before cluster size parameters.
  if ((config.validate("slow_commit_cryptosys") !=
       ConcordConfiguration::ParameterStatus::VALID) ||
      (config.validate("commit_cryptosys") !=
       ConcordConfiguration::ParameterStatus::VALID) ||
      (config.validate("optimistic_commit_cryptosys") !=
       ConcordConfiguration::ParameterStatus::VALID)) {
    throw ConfigurationResourceNotFoundException(
        "Cannot generate keys for Concord cluster: a cryptosystem selection is "
        "not valid.");
  }
  uint16_t fVal = config.getValue<uint16_t>("f_val");
  uint16_t cVal = config.getValue<uint16_t>("c_val");

  uint16_t numReplicas = 3 * fVal + 2 * cVal + 1;
  uint16_t numRoReplicas = config.getValue<uint16_t>("num_ro_replicas");

  uint16_t numSigners = numReplicas;
  uint16_t slowCommitThreshold = 2 * fVal + cVal + 1;
  uint16_t commitThreshold = 3 * fVal + cVal + 1;
  uint16_t optimisticCommitThreshold = 3 * fVal + 2 * cVal + 1;

  assert(config.getAuxiliaryState());
  ConcordPrimaryConfigurationAuxiliaryState* auxState =
      dynamic_cast<ConcordPrimaryConfigurationAuxiliaryState*>(
          config.getAuxiliaryState());

  std::pair<string, string> slowCommitCryptoSelection =
      parseCryptosystemSelection(
          config.getValue<string>("slow_commit_cryptosys"));
  std::pair<string, string> commitCryptoSelection =
      parseCryptosystemSelection(config.getValue<string>("commit_cryptosys"));
  std::pair<string, string> optimisticCommitCryptoSelection =
      parseCryptosystemSelection(
          config.getValue<string>("optimistic_commit_cryptosys"));

  auxState->slowCommitCryptosys.reset(new Cryptosystem(
      slowCommitCryptoSelection.first, slowCommitCryptoSelection.second,
      numSigners, slowCommitThreshold));
  auxState->commitCryptosys.reset(new Cryptosystem(
      commitCryptoSelection.first, commitCryptoSelection.second, numSigners,
      commitThreshold));
  auxState->optimisticCommitCryptosys.reset(
      new Cryptosystem(optimisticCommitCryptoSelection.first,
                       optimisticCommitCryptoSelection.second, numSigners,
                       optimisticCommitThreshold));

  LOG_INFO(logger,
           "Generating threshold cryptographic keys for slow path commit "
           "cryptosystem...");
  auxState->slowCommitCryptosys->generateNewPseudorandomKeys();
  LOG_INFO(
      logger,
      "Generating threshold cryptographic keys for commit cryptosystem...");
  auxState->commitCryptosys->generateNewPseudorandomKeys();
  LOG_INFO(logger,
           "Generating threshold cryptographic keys for optimistic fast "
           "path commit cryptosystem...");
  auxState->optimisticCommitCryptosys->generateNewPseudorandomKeys();

  auxState->replicaRSAKeys.clear();

  LOG_INFO(logger, "Generating Concord-BFT replica RSA keys...");
  CryptoPP::AutoSeededRandomPool randomPool;
  for (uint16_t i = 0; i < numReplicas + numRoReplicas; ++i) {
    auxState->replicaRSAKeys.push_back(generateRSAKeyPair(randomPool));
  }
}

class ParametersRequiredAtConfigurationGenerationSelector
    : public ParameterSelection::ParameterSelector {
 private:
  const string& type;

 public:
  ParametersRequiredAtConfigurationGenerationSelector(const string& type)
      : type(type) {}
  virtual ~ParametersRequiredAtConfigurationGenerationSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    // The configuration generator does not output tempalted parameters, as it
    // outputs specific values for each instance of a parameter.
    const ConfigurationPath* pathStep = &path;
    while (pathStep->isScope && pathStep->subpath) {
      if (!(pathStep->useInstance)) {
        return false;
      }
      pathStep = pathStep->subpath.get();
    }

    const ConcordConfiguration* containingScope = &config;
    if (path.isScope) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }
    return (containingScope->isTagged(path.getLeaf().name,
                                      "config_generation_time") &&
            containingScope->isTagged(path.getLeaf().name, type));
  }
};

bool hasAllParametersRequiredAtConfigurationGeneration(
    ConcordConfiguration& config) {
  Logger logger = Logger::getInstance("concord.configuration");

  ParameterSelection requiredConfiguration(
      config, make_shared<ParametersRequiredAtConfigurationGenerationSelector>(
                  config.confType_));

  bool hasAllRequired = true;
  for (auto iterator = requiredConfiguration.begin();
       iterator != requiredConfiguration.end(); ++iterator) {
    ConfigurationPath path = *iterator;
    if (!config.hasValue<string>(path)) {
      LOG_ERROR(logger, "Missing value for required configuration parameter: " +
                            path.toString() + ".");
      hasAllRequired = false;
    }
  }
  return hasAllRequired;
}

class HostsToMakeLoopbackSelector
    : public ParameterSelection::ParameterSelector {
 private:
  const size_t node;

 public:
  HostsToMakeLoopbackSelector(size_t node) : node(node) {}
  virtual ~HostsToMakeLoopbackSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    if (!path.isScope || !path.subpath || !path.useInstance ||
        (path.index != node)) {
      return false;
    }

    const ConcordConfiguration& containing_scope =
        config.subscope(path.trimLeaf());
    return containing_scope.isTagged(path.getLeaf().name, "could_be_loopback");
  }
};

class NodeConfigurationSelector : public ParameterSelection::ParameterSelector {
 private:
  const size_t node;
  const std::string nodePathName;

 public:
  NodeConfigurationSelector(size_t node, const std::string nodePathName)
      : node(node), nodePathName(nodePathName) {}
  virtual ~NodeConfigurationSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    // The configuration generator does not output tempalted parameters, as it
    // outputs specific values for each instance of a parameter.
    const ConfigurationPath* pathStep = &path;
    while (pathStep->isScope && pathStep->subpath) {
      if (!(pathStep->useInstance)) {
        return false;
      }
      pathStep = pathStep->subpath.get();
    }

    if (path.isScope && (path.name == nodePathName) && path.useInstance &&
        (path.index == node)) {
      return true;
    }

    const ConcordConfiguration* containingScope = &config;
    if (path.isScope) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }

    return !(containingScope->isTagged(path.getLeaf().name, "private"));
  }
};

void outputConcordNodeConfiguration(const ConcordConfiguration& config,
                                    YAMLConfigurationOutput& output,
                                    size_t node, bool isReadOnly) {
  Logger logger = Logger::getInstance("concord.configuration");
  ConcordConfiguration node_config = config;
  if (config.hasValue<bool>("use_loopback_for_local_hosts") &&
      config.getValue<bool>("use_loopback_for_local_hosts")) {
    ParameterSelection node_local_hosts(
        node_config, make_shared<HostsToMakeLoopbackSelector>(node));
    for (auto& path : node_local_hosts) {
      ConcordConfiguration* containing_scope = &node_config;
      if (path.isScope && path.subpath) {
        containing_scope = &(node_config.subscope(path.trimLeaf()));
      }
      string failure_message;
      if (containing_scope->loadValue(path.getLeaf().name, "127.0.0.1",
                                      &failure_message, true) ==
          ConcordConfiguration::ParameterStatus::INVALID) {
        throw invalid_argument("Failed to load 127.0.0.1 for host " +
                               path.toString() + " for node " +
                               to_string(node) +
                               "\'s configuration: " + failure_message);
      }
    }
  }

  ParameterSelection node_config_params(
      node_config, make_shared<NodeConfigurationSelector>(
                       node, isReadOnly ? "ro_node" : "node"));
  output.outputConfiguration(node_config, node_config_params.begin(),
                             node_config_params.end());
}

class ParticipantNodeConfigurationSelector
    : public ParameterSelection::ParameterSelector {
 private:
  const size_t node;

 public:
  ParticipantNodeConfigurationSelector(size_t node) : node(node) {}
  virtual ~ParticipantNodeConfigurationSelector() override {}
  virtual bool includeParameter(const ConcordConfiguration& config,
                                const ConfigurationPath& path) override {
    // The configuration generator does not output tempalted parameters, as it
    // outputs specific values for each instance of a parameter.
    const ConfigurationPath* pathStep = &path;
    while (pathStep->isScope && pathStep->subpath) {
      if (!(pathStep->useInstance)) {
        return false;
      }
      pathStep = pathStep->subpath.get();
    }

    if (path.isScope && (path.name == "participant_nodes") &&
        path.useInstance && (path.index == node)) {
      return true;
    } else if (path.isScope && (path.name == "participant_nodes") &&
               path.useInstance && (path.index != node))
      return false;

    const ConcordConfiguration* containingScope = &config;
    if (path.isScope) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }

    return !(containingScope->isTagged(path.getLeaf().name, "private"));
  }
};

void outputParticipantNodeConfiguration(const ConcordConfiguration& config,
                                        YAMLConfigurationOutput& output,
                                        size_t nodeId) {
  Logger logger = Logger::getInstance("concord.configuration");
  ConcordConfiguration node_config = config;
  node_config.subscope("participant_nodes", 0) =
      config.subscope("participant_nodes", nodeId);
  nodeId = 0;
  if (config.hasValue<bool>("use_loopback_for_local_hosts") &&
      config.getValue<bool>("use_loopback_for_local_hosts")) {
    ParameterSelection node_local_hosts(
        node_config, make_shared<HostsToMakeLoopbackSelector>(nodeId));
    for (auto& path : node_local_hosts) {
      ConcordConfiguration* containing_scope = &node_config;
      if (path.isScope && path.subpath) {
        containing_scope = &(node_config.subscope(path.trimLeaf()));
      }
      string failure_message;
      if (containing_scope->loadValue(path.getLeaf().name, "127.0.0.1",
                                      &failure_message, true) ==
          ConcordConfiguration::ParameterStatus::INVALID) {
        throw invalid_argument("Failed to load 127.0.0.1 for host " +
                               path.toString() + " for node " +
                               to_string(nodeId) +
                               "\'s configuration: " + failure_message);
      }
    }
  }
  ParameterSelection node_config_params(
      node_config, make_shared<ParticipantNodeConfigurationSelector>(nodeId));
  output.outputConfiguration(node_config, node_config_params.begin(),
                             node_config_params.end());
}

void loadNodeConfiguration(ConcordConfiguration& config,
                           YAMLConfigurationInput& input) {
  Logger logger = Logger::getInstance("concord.configuration");

  loadClusterSizeParameters(input, config);
  instantiateTemplatedConfiguration(input, config);

  // Note there are currently no configuration parameters that cannot be loaded
  // from the node's configuration file (for example, in the future, this could
  // include generated parameters that must be generated on the nodes and are
  // therefore unaccptable as input from the node's configuration files). If
  // this changes in the future, the immediately following
  // YAMLConfigurationInput::loadConfiguration call will need to be adjusted to
  // use a ParameterSelection that excludes such parameters.
  input.loadConfiguration(
      config, config.begin(ConcordConfiguration::kIterateAllInstanceParameters),
      config.end(ConcordConfiguration::kIterateAllInstanceParameters), &logger);

  auto [localNode, isReadOnly] = detectLocalNode(config);
  ParameterSelection nodeConfiguration(
      config, make_shared<NodeConfigurationSelector>(
                  localNode, isReadOnly ? "ro_node" : "node"));
  LOG_INFO(logger, "Detected node "
                       << localNode
                       << (isReadOnly ? " (RO node)" : " (committer node"));

  // Try loading defaults and running generators for optional parameters and
  // parameters that can be implicit in the node configuration, excluding those
  // tagged "config_generation_time" (whose values must be settled on at
  // configuration generation time and therefore cannot be picked here by the
  // booting Concord node).
  for (auto iterator = nodeConfiguration.begin();
       iterator != nodeConfiguration.end(); ++iterator) {
    ConfigurationPath path = *iterator;
    ConcordConfiguration* containingScope = &config;
    if (path.isScope && path.subpath) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }
    string name = path.getLeaf().name;

    if (!(containingScope->hasValue<string>(name)) &&
        !(containingScope->isTagged(name, "config_generation_time"))) {
      if (containingScope->isTagged(name, "defaultable")) {
        containingScope->loadDefault(name);
      } else if (containingScope->isTagged(name, "generated")) {
        string failureMessage;
        if (containingScope->generate(name, &failureMessage) !=
            ConcordConfiguration::ParameterStatus::VALID) {
          LOG_ERROR(logger, "Cannot generate value for " + path.toString() +
                                ": " + failureMessage);
        }
      }
    }
  }

  // Validate that all parameters the node will need have actually been loaded
  // and that none have invalid valus. We currently choose not to reject the
  // configuration if some validators claim insufficient information in case
  // some validators actually account for private information from other nodes
  // in their validation.
  if (config.validateAll(true, false) ==
      ConcordConfiguration::ParameterStatus::INVALID) {
    LOG_ERROR(logger,
              "Node configuration was found to contain some invalid "
              "value(s) on final validation.");
    throw ConfigurationResourceNotFoundException(
        "Node configuration complete validation failed.");
  }

  bool hasAllRequired = true;
  vector<string> parameters_missing;
  for (auto iterator = nodeConfiguration.begin();
       iterator != nodeConfiguration.end(); ++iterator) {
    ConfigurationPath path = *iterator;
    if (!config.hasValue<string>(path)) {
      ConcordConfiguration* containingScope = &config;
      if (path.isScope && path.subpath) {
        containingScope = &(config.subscope(path.trimLeaf()));
      }
      if (!(containingScope->isTagged(path.getLeaf().name, "optional"))) {
        hasAllRequired = false;
        parameters_missing.push_back((*iterator).toString());
        LOG_ERROR(logger,
                  "Concord node configuration is missing a value for a "
                  "required parameter: " +
                      (*iterator).toString());
      }
    }
  }
  if (!hasAllRequired) {
    throw ConfigurationResourceNotFoundException(
        getErrorMessageListingParameters("Node configuration is missing "
                                         "value(s) for required parameter(s): ",
                                         parameters_missing));
  }
}

std::pair<size_t, bool> detectLocalNode(ConcordConfiguration& config) {
  size_t nodeDetected;
  bool roNodeDetected = true;
  bool hasDetectedNode = false;
  ConfigurationPath detectedPath;

  bool hasValueForAnyNodePublicParameter = false;
  bool hasValueForAnyNodeTemplateParameter = false;
  bool hasValueForAnyNonNodeParameter = false;

  for (auto iterator =
           config.begin(ConcordConfiguration::kIterateAllParameters);
       iterator != config.end(ConcordConfiguration::kIterateAllParameters);
       ++iterator) {
    ConfigurationPath path = *iterator;
    if (path.isScope && (path.name == "node" || path.name == "ro_node")) {
      if (path.useInstance) {
        // If the node is in ro_node section -> shift its id so that it's
        // after the regular nodes
        size_t node = path.index;
        bool isReadOnly = path.name == "ro_node";
        ConcordConfiguration* containingScope =
            &(config.subscope(path.trimLeaf()));
        if (containingScope->isTagged(path.getLeaf().name, "private") &&
            config.hasValue<string>(path)) {
          if (hasDetectedNode && (node != nodeDetected)) {
            throw ConfigurationResourceNotFoundException(
                "Cannot determine which node Concord configuration file is "
                "for: found values for private configuration parameters for "
                "multiple nodes. Conflicting private parameters are : " +
                detectedPath.toString() + " and " + path.toString() + ".");
          }
          hasDetectedNode = true;
          nodeDetected = node;
          roNodeDetected = isReadOnly;
          detectedPath = path;
        } else if (config.hasValue<string>(path)) {
          hasValueForAnyNodePublicParameter = true;
        }
      } else {
        if (config.hasValue<string>(path)) {
          hasValueForAnyNodeTemplateParameter = true;
        }
      }
    } else {
      if (config.hasValue<string>(path)) {
        hasValueForAnyNonNodeParameter = true;
      }
    }
  }

  if (!hasDetectedNode) {
    if (hasValueForAnyNodePublicParameter) {
      throw ConfigurationResourceNotFoundException(
          "Cannot determine which node configuration file is for: no values "
          "found for any private configuration parameters in instances of the "
          "node scope.");
    } else if (hasValueForAnyNodeTemplateParameter) {
      throw ConfigurationResourceNotFoundException(
          "Cannot determine which node configuration file is for: no values "
          "found for any parameters in instances of the node scope, though "
          "there are values for parameters in the node template.");
    } else if (hasValueForAnyNonNodeParameter) {
      throw ConfigurationResourceNotFoundException(
          "Cannot determine which node configuration file is for: no values "
          "found for any parameters in the node scope (Is the node scope "
          "missing or malformatted in Concord's configuration file?).");
    } else {
      throw ConfigurationResourceNotFoundException(
          "Cannot determine which node configuration file is for: no values "
          "found for any recognized parameters in Concord's configuration "
          "file. (Has Concord been given the wrong file for its configuration? "
          "Is the configuration file malformatted?)");
    }
  }
  return std::make_pair(nodeDetected, roNodeDetected);
}

void loadSBFTCryptosystems(ConcordConfiguration& config) {
  Logger logger = Logger::getInstance("concord.configuration");

  // Note we do not validate that the cryptosystem selections here are valid as
  // long as they exist, as we expect this has been handled by the parameter
  // validators as the configuration was loaded.
  vector<ConfigurationPath> requiredCryptosystemParameters(
      {ConfigurationPath("f_val", false), ConfigurationPath("c_val", false),
       ConfigurationPath("slow_commit_cryptosys", false),
       ConfigurationPath("commit_cryptosys", false),
       ConfigurationPath("optimistic_commit_cryptosys", false),
       ConfigurationPath("slow_commit_public_key", false),
       ConfigurationPath("commit_public_key", false),
       ConfigurationPath("optimistic_commit_public_key", false)});
  bool hasRequired = true;
  vector<string> parameters_missing;
  for (auto&& path : requiredCryptosystemParameters) {
    if (!config.hasValue<string>(path)) {
      hasRequired = false;
      parameters_missing.push_back(path.toString());
      LOG_ERROR(
          logger,
          "Configuration missing value for required cryptosystem parameter: " +
              path.toString());
    }
  }
  if (!hasRequired) {
    throw ConfigurationResourceNotFoundException(
        getErrorMessageListingParameters(
            "Cannot load SBFT Cryptosystems for given configuration: "
            "configuration is missing value(s) for required crypto "
            "parameter(s): ",
            parameters_missing));
  }

  ConcordPrimaryConfigurationAuxiliaryState* auxState;
  assert(auxState = dynamic_cast<ConcordPrimaryConfigurationAuxiliaryState*>(
             config.getAuxiliaryState()));

  uint16_t fVal = config.getValue<uint16_t>("f_val");
  uint16_t cVal = config.getValue<uint16_t>("c_val");
  uint16_t slowCommitThresh = 2 * fVal + cVal + 1;
  uint16_t commitThresh = 3 * fVal + cVal + 1;
  uint16_t optimisticCommitThresh = 3 * fVal + 2 * cVal + 1;
  uint16_t numSigners = 3 * fVal + 2 * cVal + 1;

  std::pair<string, string> slowCommitCryptoSelection =
      parseCryptosystemSelection(
          config.getValue<string>("slow_commit_cryptosys"));
  std::pair<string, string> commitCryptoSelection =
      parseCryptosystemSelection(config.getValue<string>("commit_cryptosys"));
  std::pair<string, string> optimisticCommitCryptoSelection =
      parseCryptosystemSelection(
          config.getValue<string>("optimistic_commit_cryptosys"));

  auxState->slowCommitCryptosys.reset(new Cryptosystem(
      slowCommitCryptoSelection.first, slowCommitCryptoSelection.second,
      numSigners, slowCommitThresh));
  auxState->commitCryptosys.reset(new Cryptosystem(commitCryptoSelection.first,
                                                   commitCryptoSelection.second,
                                                   numSigners, commitThresh));
  auxState->optimisticCommitCryptosys.reset(
      new Cryptosystem(optimisticCommitCryptoSelection.first,
                       optimisticCommitCryptoSelection.second, numSigners,
                       optimisticCommitThresh));

  // Note these vectors will be given to Cryptosystems, which consider them to
  // be 1-indexed.
  vector<string> slowCommitVerificationKeys(numSigners + 1);
  vector<string> commitVerificationKeys(numSigners + 1);
  vector<string> optimisticCommitVerificationKeys(numSigners + 1);

  assert(config.containsScope("node") && config.scopeIsInstantiated("node") &&
         (config.scopeSize("node") == numSigners));
  for (uint16_t i = 0; i < numSigners; ++i) {
    ConcordConfiguration& nodeConfig = config.subscope("node", i);
    assert(nodeConfig.containsScope("replica") &&
           nodeConfig.scopeIsInstantiated("replica") &&
           (nodeConfig.scopeSize("replica") == 1));
    ConcordConfiguration& replicaConfig = nodeConfig.subscope("replica", 0);
    uint16_t replicaID = replicaConfig.getValue<uint16_t>("principal_id");

    if (!replicaConfig.hasValue<string>("slow_commit_verification_key")) {
      hasRequired = false;
      parameters_missing.push_back("node[" + to_string(i) +
                                   "]/replica[0]/slow_commit_verification_key");
      LOG_ERROR(logger,
                "Configuration missing required threshold verification "
                "key: slow_commit_verification_key for replica " +
                    to_string(i) + ".");
    } else {
      slowCommitVerificationKeys[replicaID + 1] =
          replicaConfig.getValue<string>("slow_commit_verification_key");
    }
    if (!replicaConfig.hasValue<string>("commit_verification_key")) {
      hasRequired = false;
      parameters_missing.push_back("node[" + to_string(i) +
                                   "]/replica[0]/commit_verification_key");
      LOG_ERROR(logger,
                "Configuration missing required threshold verification "
                "key: commit_verification_key for replica " +
                    to_string(i) + ".");
    } else {
      commitVerificationKeys[replicaID + 1] =
          replicaConfig.getValue<string>("commit_verification_key");
    }
    if (!replicaConfig.hasValue<string>("optimistic_commit_verification_key")) {
      hasRequired = false;
      parameters_missing.push_back(
          "node[" + to_string(i) +
          "]/replica[0]/optimistic_commit_verification_key");
      LOG_ERROR(logger,
                "Configuration missing required threshold verification "
                "key: optimistic_commit_verification_key for replica " +
                    to_string(i) + ".");
    } else {
      optimisticCommitVerificationKeys[replicaID + 1] =
          replicaConfig.getValue<string>("optimistic_commit_verification_key");
    }
  }
  if (!hasRequired) {
    throw ConfigurationResourceNotFoundException(
        getErrorMessageListingParameters(
            "Cannot load SBFT Cryptosystems: configuration is missing value(s) "
            "for required parameter(s): ",
            parameters_missing));
  }

  auxState->slowCommitCryptosys->loadKeys(
      config.getValue<string>("slow_commit_public_key"),
      slowCommitVerificationKeys);
  auxState->commitCryptosys->loadKeys(
      config.getValue<string>("commit_public_key"), commitVerificationKeys);
  auxState->optimisticCommitCryptosys->loadKeys(
      config.getValue<string>("optimistic_commit_public_key"),
      optimisticCommitVerificationKeys);

  auto [local_node, isReadOnly] = detectLocalNode(config);
  ConcordConfiguration& localReplicaConfig =
      config.subscope("node", local_node).subscope("replica", 0);
  uint16_t localReplicaID =
      localReplicaConfig.getValue<uint16_t>("principal_id");
  if (!localReplicaConfig.hasValue<string>("slow_commit_private_key")) {
    hasRequired = false;
    parameters_missing.push_back("node[" + to_string(local_node) +
                                 "]/replica[0]/slow_commit_private_key");
    LOG_ERROR(logger,
              "Configuration missing required threshold private key: "
              "slow_commit_private_key for this node.");
  } else {
    auxState->slowCommitCryptosys->loadPrivateKey(
        (localReplicaID + 1),
        localReplicaConfig.getValue<string>("slow_commit_private_key"));
  }
  if (!localReplicaConfig.hasValue<string>("commit_private_key")) {
    hasRequired = false;
    parameters_missing.push_back("node[" + to_string(local_node) +
                                 "]/replica[0]/commit_private_key");
    LOG_ERROR(logger,
              "Configuration missing required threshold private key: "
              "commit_private_key for this node.");
  } else {
    auxState->commitCryptosys->loadPrivateKey(
        (localReplicaID + 1),
        localReplicaConfig.getValue<string>("commit_private_key"));
  }
  if (!localReplicaConfig.hasValue<string>("optimistic_commit_private_key")) {
    hasRequired = false;
    parameters_missing.push_back("node[" + to_string(local_node) +
                                 "]/replica[0]/optimistic_commit_private_key");
    LOG_ERROR(logger,
              "Configuration missing required threshold private key: "
              "optimistic_commit_private_key for this node.");
  } else {
    auxState->optimisticCommitCryptosys->loadPrivateKey(
        (localReplicaID + 1),
        localReplicaConfig.getValue<string>("optimistic_commit_private_key"));
  }
  if (!hasRequired) {
    throw ConfigurationResourceNotFoundException(
        getErrorMessageListingParameters(
            "Cannot load SBFT Cryptosystems: configuration is missing value(s) "
            "for required parameter(s): ",
            parameters_missing));
  }
}

// Note the current implementaion of this function assumes all Concord-BFT
// principal IDs in the configuration are parameters with the name
// "principal_id".
void outputPrincipalLocationsMappingJSON(ConcordConfiguration& config,
                                         ostream& output, bool client_flag) {
  json principal_map;
  string scope_name = "node";
  if (client_flag) scope_name = "participant_nodes";
  if (config.containsScope(scope_name) &&
      config.scopeIsInstantiated(scope_name)) {
    for (size_t i = 0; i < config.scopeSize(scope_name); ++i) {
      string node_id = to_string(i + 1);
      ConcordConfiguration& node = config.subscope(scope_name, i);

      principal_map[node_id] = json::array();
      for (auto iter =
               node.begin(ConcordConfiguration::kIterateAllInstanceParameters);
           iter !=
           node.end(ConcordConfiguration::kIterateAllInstanceParameters);
           ++iter) {
        const ConfigurationPath& path = *iter;
        if ((path.getLeaf().name == "principal_id") &&
            node.hasValue<uint16_t>(path)) {
          principal_map[node_id].emplace_back(node.getValue<uint16_t>(path));
        }
      }
    }
  }

  output << principal_map;
}

class ParticipantsNodesSizer : public ConcordConfiguration::ScopeSizer {
 public:
  virtual ~ParticipantsNodesSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    uint16_t num_participants = 0;
    if (!(config.hasValue<uint16_t>("num_of_participant_nodes"))) {
      num_participants = 1;
    } else
      num_participants = config.getValue<uint16_t>("num_of_participant_nodes");
    size_t numParticipants = (size_t)num_participants;
    output = numParticipants;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class SingleParticipantNodeSizer : public ConcordConfiguration::ScopeSizer {
 public:
  virtual ~SingleParticipantNodeSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    size_t numParticipants = (size_t)1;
    output = numParticipants;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ExternalClientsSizer : public ConcordConfiguration::ScopeSizer {
 public:
  virtual ~ExternalClientsSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    if (!(config.hasValue<uint16_t>("clients_per_participant_node"))) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    if (!((config.validate("clients_per_participant_node") ==
           ConcordConfiguration::ParameterStatus::VALID))) {
      return ConcordConfiguration::ParameterStatus::INVALID;
    }

    uint16_t num_clients_proxies =
        config.getValue<uint16_t>("clients_per_participant_node");
    size_t numExternal = (size_t)num_clients_proxies;
    output = numExternal;
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class TimeoutMillisecondsValidator
    : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~TimeoutMillisecondsValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const std::string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, std::string& failure_message) override {
    UIntValidator boundsCheck(1, UINT16_MAX);
    if (const auto res =
            boundsCheck.validate(value, config, path, failure_message);
        res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }

    if (!config.hasValue<uint16_t>("client_initial_retry_timeout_milli") ||
        !config.hasValue<uint16_t>("client_max_retry_timeout_milli")) {
      failure_message =
          "Cannot validate timeouts milli- some field not initialized";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }

    auto initial =
        config.getValue<uint16_t>("client_initial_retry_timeout_milli");
    auto max = config.getValue<uint16_t>("client_max_retry_timeout_milli");
    auto min = std::stoull(value);
    if (min < 1) return ConcordConfiguration::ParameterStatus::INVALID;
    if (max < 1) return ConcordConfiguration::ParameterStatus::INVALID;
    if (initial < min || initial > max) {
      failure_message =
          "Invalid value , value has to be between min to max retry timeout "
          "milli";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ClientPrincipalIdValidator
    : public ConcordConfiguration::ParameterValidator {
 public:
  virtual ~ClientPrincipalIdValidator() override {}
  virtual ConcordConfiguration::ParameterStatus validate(
      const std::string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, std::string& failureMessage) override {
    UIntValidator boundsCheck(0, UINT16_MAX);
    ConcordConfiguration::ParameterStatus res =
        boundsCheck.validate(value, config, path, failureMessage);
    if (res != ConcordConfiguration::ParameterStatus::VALID) {
      return res;
    }
    uint16_t principalID = (uint16_t)(std::stoull(value));

    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val") ||
        !config.hasValue<uint16_t>("num_ro_replicas")) {
      failureMessage =
          "Cannot fully validate Concord-BFT principal ID for " +
          path.toString() +
          ": f_val, c_val, and client_proxies_per_replica are required to "
          "determine bounds for maximum principal ID.";
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t fVal = config.getValue<uint16_t>("f_val");
    uint16_t cVal = config.getValue<uint16_t>("c_val");
    uint16_t numRoReplicas = config.getValue<uint16_t>("num_ro_replicas");
    uint16_t numReplicas = 3 * fVal + 2 * cVal + 1;

    // The path to a principal Id should be of one of these forms:
    //   node[i]/replica[0]/principal_id
    //   participant_node[i]/external_client[j]/client[0]/principal_id
    assert(path.isScope && path.subpath);
    assert(path.subpath->subpath->subpath->name == "client");

    if ((principalID < numReplicas + numRoReplicas)) {
      failureMessage =
          "Invalid principal ID for " + path.toString() + ": " +
          std::to_string(principalID) +
          ". Principal IDs for client proxies should be in the range "
          "(num_replicas, num_principals - 1), inclusive.";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }

    res = ConcordConfiguration::ParameterStatus::VALID;
    if (res ==
        ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION) {
      failureMessage = "Cannot fully validate principal ID for " +
                       path.toString() +
                       ": Not all other principal IDs are known, but are "
                       "required to check for uniqueness.";
    }
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ClientPrincipalIdCalculator
    : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~ClientPrincipalIdCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      std::string& output) override {
    // The path to a principal Id should be of one of these forms:
    //   node[i]/replica[0]/principal_id
    //   participant_node[i]/external_client[j]/client[0]/principal_id
    assert(path.isScope && path.subpath && path.useInstance);

    if (path.subpath->name == "replica") {
      output = std::to_string(path.index);
    } else {
      assert((path.subpath->subpath->subpath->name == "client") &&
             path.subpath->subpath->subpath->isScope &&
             path.subpath->subpath->subpath->useInstance);

      if (!config.hasValue<uint16_t>("f_val") ||
          !config.hasValue<uint16_t>("c_val") ||
          !config.hasValue<uint16_t>("num_ro_replicas") ||
          !config.hasValue<uint16_t>("client_proxies_per_replica")) {
        return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
      }
      auto numOfInternalClientsPerReplica =
          config.getValue<uint16_t>("client_proxies_per_replica");
      auto numOfReplicas = 3 * config.getValue<uint16_t>("f_val") +
                           2 * config.getValue<uint16_t>("c_val") + 1;
      auto numRoReplicas = config.getValue<uint16_t>("num_ro_replicas");

      uint16_t numOfAllCommitterNodePrincipals =
          numOfReplicas * numOfInternalClientsPerReplica + numOfReplicas;

      uint16_t numParticipants =
          config.getValue<uint16_t>("clients_per_participant_node");

      output = std::to_string(path.index * numParticipants +
                              numOfAllCommitterNodePrincipals + numRoReplicas +
                              (path.subpath->subpath->index));
    }

    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class OperatorPrincipalIdCalculator
    : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~OperatorPrincipalIdCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      std::string& output) override {
    // The path to a principal Id should be of one of these forms:
    //   node[i]/replica[0]/principal_id
    //   participant_node[i]/external_client[j]/client[0]/principal_id
    assert(path.isScope && path.subpath && path.useInstance);

    if (path.subpath->name == "replica") {
      output = std::to_string(path.index);
    } else {
      if (!config.hasValue<uint16_t>("f_val") ||
          !config.hasValue<uint16_t>("c_val") ||
          !config.hasValue<uint16_t>("num_ro_replicas") ||
          !config.hasValue<uint16_t>("client_proxies_per_replica")) {
        return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
      }
      auto numParticipantsNodes =
          config.getValue<uint16_t>("num_of_participant_nodes");
      auto numOfInternalClientsPerReplica =
          config.getValue<uint16_t>("client_proxies_per_replica");
      auto numOfReplicas = 3 * config.getValue<uint16_t>("f_val") +
                           2 * config.getValue<uint16_t>("c_val") + 1;
      uint16_t numOfAllCommitterNodePrincipals =
          numOfReplicas * numOfInternalClientsPerReplica + numOfReplicas;
      auto numRoReplicas = config.getValue<uint16_t>("num_ro_replicas");

      uint16_t numParticipants =
          config.getValue<uint16_t>("clients_per_participant_node");

      output = std::to_string(path.index + numOfAllCommitterNodePrincipals +
                              numRoReplicas +
                              (numParticipantsNodes * numParticipants));
    }

    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

class ClientNumReplicasCalculator
    : public ConcordConfiguration::ParameterGenerator {
 public:
  virtual ~ClientNumReplicasCalculator() override {}
  virtual ConcordConfiguration::ParameterStatus generate(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      string& output) override {
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    output = to_string(3 * config.getValue<uint16_t>("f_val") +
                       2 * config.getValue<uint16_t>("c_val") + 1);
    return ConcordConfiguration::ParameterStatus::VALID;
  }
};

void specifyClientConfiguration(ConcordConfiguration& config) {
  vector<std::string> publicGeneratedTags(
      {"config_generation_time", "generated", "public"});

  vector<std::string> publicInputTags(
      {"config_generation_time", "input", "public"});

  vector<std::string> privateInputTags(
      {"config_generation_time", "input", "private"});

  config.declareParameter(
      "num_of_participant_nodes",
      "Total number of participant nodes in this deployment.");
  config.tagParameter("num_of_participant_nodes", privateInputTags);
  config.declareParameter(
      "clients_per_participant_node",
      "Max number of clients that each participant node can have");
  config.tagParameter("clients_per_participant_node", publicInputTags);
  config.addValidator(
      "clients_per_participant_node",
      make_shared<UIntValidator>(kMinParticipantNodeNumOfClients,
                                 kMaxParticipantNodeNumOfClients));
  config.declareScope("participant_nodes", "Participant nodes scope",
                      make_shared<ParticipantsNodesSizer>());
  auto& participant_nodes = config.subscope("participant_nodes");
  participant_nodes.declareScope("participant_node", "One node",
                                 make_shared<SingleParticipantNodeSizer>());
  auto& participant_node = participant_nodes.subscope("participant_node");
  participant_node.declareParameter(
      "participant_node_host",
      "IP address or host name this participant node can be reached at.");
  participant_node.tagParameter("participant_node_host", publicInputTags);
  participant_node.declareParameter(
      "operator_id", "The ID the operator of this specific participant node");
  participant_node.tagParameter("operator_id", publicGeneratedTags);
  participant_node.addGenerator("operator_id",
                                make_shared<OperatorPrincipalIdCalculator>());
  participant_node.declareParameter(
      "operator_port",
      "Port number this operator participant_node can be reached at.");
  participant_node.tagParameter("operator_port", publicInputTags);
  participant_node.addValidator("operator_port",
                                make_shared<UIntValidator>(0, UINT16_MAX));
  participant_node.declareScope(
      "external_clients",
      "Scope that represent the clients inside this participant node, this "
      "scope holds port number and principal id for each client",
      make_shared<ExternalClientsSizer>());
  auto& external_clients = participant_node.subscope("external_clients");
  external_clients.declareScope("client", "One external client params",
                                make_shared<ReplicasSizer>());
  auto& client = external_clients.subscope("client");
  client.declareParameter(
      "principal_id",
      "Unique ID number for this Concord-BFT client proxy. Concord-BFT "
      "considers "
      "replicas, clients and client proxies to be principals, each of which "
      "must have a unique ID.");
  client.tagParameter("principal_id", publicGeneratedTags);
  client.addValidator("principal_id",
                      make_shared<ClientPrincipalIdValidator>());
  client.addGenerator("principal_id",
                      make_shared<ClientPrincipalIdCalculator>());

  client.declareParameter("client_port",
                          "Port number this client can be reached at.");
  client.tagParameter("client_port", publicInputTags);
  client.addValidator("client_port", make_shared<UIntValidator>(0, UINT16_MAX));
}

void specifyGeneralConfiguration(ConcordConfiguration& config) {
  vector<std::string> publicGeneratedTags(
      {"config_generation_time", "generated", "public"});

  vector<std::string> publicInputTags(
      {"config_generation_time", "input", "public"});

  vector<std::string> defaultableByUtilityTags(
      {"config_generation_time", "defaultable", "public"});

  // Validation of f_val is based on c_val and num_replicas .
  config.declareParameter("f_val", "F parameter to the SBFT algorithm.");
  config.tagParameter("f_val", publicInputTags);
  // Validation of c_val is based on f_val and num_replicas .
  config.declareParameter("c_val", "C parameter to the SBFT algorithm.");
  config.tagParameter("c_val", publicInputTags);
  // Parameter declarations
  config.declareParameter("client_proxies_per_replica",
                          "The number of SBFT client proxies configured on "
                          "each Concord committer node",
                          "4");
  config.tagParameter("client_proxies_per_replica", defaultableByUtilityTags);
  config.addValidator("client_proxies_per_replica",
                      make_shared<ClientProxiesPerReplicaValidator>());
  // Validation is based on f_val and c_val .
  config.declareParameter(
      "num_replicas", "Total number of Concord replicas in this deployment.");
  config.tagParameter("num_replicas", publicGeneratedTags);
  config.addValidator("num_replicas", make_shared<NumReplicasValidator>());
  config.addGenerator("num_replicas",
                      make_shared<ClientNumReplicasCalculator>());
  config.declareParameter(
      "num_ro_replicas",
      "Total number of Concord read-only replicas in this deployment.", "0");
  config.tagParameter("num_ro_replicas", defaultableByUtilityTags);
  config.tagParameter("num_ro_replicas", publicInputTags);
  config.addValidator("num_ro_replicas",
                      make_shared<UIntValidator>(0, UINT64_MAX));
  config.declareParameter("prometheus_port",
                          "Port of prometheus client to publish metrics on");
  config.tagParameter("prometheus_port", publicInputTags);
  //  Validation is done at construction of the client object.
  config.declareParameter("comm_to_use", "Default communication module");
  config.tagParameter("comm_to_use", publicInputTags);
  config.declareParameter("tls_certificates_folder_path",
                          "TLS certificates root folder path.");
  config.tagParameter("tls_certificates_folder_path", publicInputTags);
  config.declareParameter("signing_key_path", "Signing key root folder path",
                          "resources/signing_keys");
  config.tagParameter("signing_key_path", defaultableByUtilityTags);
  config.declareParameter("tls_cipher_suite_list",
                          "TLS cipher suite list to use.");
  config.tagParameter("tls_cipher_suite_list", publicInputTags);
  config.declareParameter(
      "concord-bft_communication_buffer_length",
      "Size of buffers to be used for messages exchanged with and within "
      "Concord-BFT.",
      "64000");
  config.tagParameter("concord-bft_communication_buffer_length",
                      defaultableByUtilityTags);
  config.addValidator(
      "concord-bft_communication_buffer_length",
      make_shared<UIntValidator>(kMinConcordBFTCommunicationBufferSize,
                                 kMaxConcordBFTCommunicationBufferSize));
}

void specifyReplicaConfiguration(ConcordConfiguration& config) {
  vector<std::string> publicGeneratedTags(
      {"config_generation_time", "generated", "public"});

  vector<std::string> publicInputTags(
      {"config_generation_time", "input", "public"});

  vector<std::string> principalHostTags(
      {"config_generation_time", "could_be_loopback", "input", "public"});

  config.declareScope("node",
                      "Concord nodes that form the distributed system that "
                      "maintains a blockchain in Concord.",
                      make_shared<NodesSizer>());

  ConcordConfiguration& node = config.subscope("node");

  node.declareScope(
      "replica",
      "SBFT replicas, which serve as the core replicas for Byzantine fault "
      "tolerant consensus in a Concord deployment.",
      make_shared<ReplicasSizer>());
  ConcordConfiguration& replica = node.subscope("replica");

  replica.declareParameter(
      "principal_id",
      "Unique ID number for this Concord-BFT replica. Concord-BFT considers "
      "replicas, clients and client proxies to be principals, each of which "
      "must have a unique ID.");
  replica.tagParameter("principal_id", publicGeneratedTags);
  // replica.addValidator("principal_id", make_shared<PrincipalIdValidator>());
  replica.addGenerator("principal_id",
                       make_shared<ClientPrincipalIdCalculator>());

  replica.declareParameter(
      "replica_host",
      "IP address or host name this replica can be reached at.");
  replica.tagParameter("replica_host", principalHostTags);
  replica.declareParameter("replica_port",
                           "Port number this replica can be reached at.");
  replica.tagParameter("replica_port", publicInputTags);
  replica.addValidator("replica_port",
                       make_shared<UIntValidator>(0, UINT16_MAX));
}

void specifySimpleClientParams(ConcordConfiguration& config) {
  vector<std::string> defaultableByUtilityTags(
      {"config_generation_time", "defaultable", "public"});

  config.declareParameter("client_initial_retry_timeout_milli",
                          "The starting request retry timeout.", "150");
  config.tagParameter("client_initial_retry_timeout_milli",
                      defaultableByUtilityTags);

  config.declareParameter("client_min_retry_timeout_milli",
                          "The minimum dynamically settable request timeout.",
                          "50");
  config.tagParameter("client_min_retry_timeout_milli",
                      defaultableByUtilityTags);
  config.addValidator("client_min_retry_timeout_milli",
                      make_shared<TimeoutMillisecondsValidator>());

  config.declareParameter("client_max_retry_timeout_milli",
                          "The maximum dynamically settable request timeout.",
                          "1000");
  config.tagParameter("client_max_retry_timeout_milli",
                      defaultableByUtilityTags);

  config.declareParameter("client_number_of_standard_deviations_to_tolerate",
                          "The number of standard deviations within the "
                          "average that we expect reply times to fall into.",
                          "2");
  config.tagParameter("client_number_of_standard_deviations_to_tolerate",
                      defaultableByUtilityTags);
  config.addValidator("client_number_of_standard_deviations_to_tolerate",
                      make_shared<UIntValidator>(0, UINT16_MAX));

  config.declareParameter("client_samples_per_evaluation",
                          "The number of replies we sample before we attempt "
                          "to recalculate the rolling average and variance.",
                          "32");
  config.tagParameter("client_samples_per_evaluation",
                      defaultableByUtilityTags);
  config.addValidator("client_samples_per_evaluation",
                      make_shared<UIntValidator>(0, UINT16_MAX));

  config.declareParameter(
      "client_samples_until_reset",
      "The number of samples before we reset the rolling average and variance "
      "to its empty state.",
      "1000");
  config.tagParameter("client_samples_until_reset", defaultableByUtilityTags);
  config.addValidator("client_samples_until_reset",
                      make_shared<UIntValidator>(0, UINT16_MAX));

  config.declareParameter(
      "client_sends_request_to_all_replicas_first_thresh",
      "The scaling factor at which the request timeout can be increased. A "
      "factor of 2 "
      "means that the upper limit can be doubled on each evaluation period.",
      "2");
  config.tagParameter("client_sends_request_to_all_replicas_first_thresh",
                      defaultableByUtilityTags);
  config.addValidator("client_sends_request_to_all_replicas_first_thresh",
                      make_shared<UIntValidator>(0, UINT16_MAX));

  config.declareParameter(
      "client_sends_request_to_all_replicas_period_thresh",
      "The scaling factor at which the request timeout can be decreased. A "
      "factor of "
      "2 means that the upper limit can be halved on each evaluation period.",
      "2");
  config.tagParameter("client_sends_request_to_all_replicas_period_thresh",
                      defaultableByUtilityTags);
  config.addValidator("client_sends_request_to_all_replicas_period_thresh",
                      make_shared<UIntValidator>(0, UINT16_MAX));

  config.declareParameter("client_periodic_reset_thresh",
                          "The client periodic reset thresh configuration.",
                          "30");
  config.tagParameter("client_periodic_reset_thresh", defaultableByUtilityTags);
  config.addValidator("client_periodic_reset_thresh",
                      make_shared<UIntValidator>(0, UINT16_MAX));
}

void specifyExternalClientConfiguration(config::ConcordConfiguration& config) {
  specifyGeneralConfiguration(config);
  specifyReplicaConfiguration(config);
  specifyClientConfiguration(config);
  specifySimpleClientParams(config);
}
}  // namespace config
}  // namespace concord
