// Copyright 2018-2019 VMware, all rights reserved

#include "configuration_manager.hpp"

using namespace boost::program_options;

using namespace std;

// default IP on which to listen for client connections
static const string default_listen_ip = "0.0.0.0";

// default port on which to listen for client connections
static const short default_listen_port = 5458;

// default location of logging properties file
static const string default_log_props = "./resources/log4cplus.properties";

// default location of configuration file
static const string default_config_file = "./resources/concord.config";

// default period to check for logging properties changes (milliseconds)
static const int default_log_props_time_ms = 60000;  // 60sec

// default implementation of blockchain storage
static const string default_blockchain_db_impl = "memory";

// default size of API worker thread pool
static const int default_api_worker_thread_pool_size = 4;

// default count of maximum transactions returned by transaction list query
static const int default_transaction_list_max_count = 10;

// default gas limit for transaction execution
// this high value is a temporary solution until we will find the way
// to customize docker build to run concord processes with CLI params
static const int64_t default_gas_limit = 10000000;

variables_map initialize_config(int argc, char** argv) {
  // A map to hold key-value pairs of all options
  variables_map options_map;

  // holds the value of configuration file for Concord
  // this is NOT same as logger configuration file. Logger
  // configuration file can be specified by command line options or
  // as a property in configuration file.
  string config_file;

  // Program options which are generic for most of the programs:
  // These are not available via configuration files
  // only allowed to be passed on command line
  options_description generic{"Generic Options"};
  generic.add_options()("help,h", "Print this help message")(
      "config,c",
      value<string>(&config_file)->default_value(default_config_file),
      "Path for configuration file")("debug",
                                     "Sleep for 20 seconds to attach debug");

  // The configuration parameters specific to this program
  // These can be provided in config file as well as on command line
  // If same parameter is provided in config file as well as on
  // command line, then command line value will take preference.
  // Since, we read command line options first all the parameters specified
  // in command line will be populated in varaiables_map first, if same option
  // is present in config file it will be read but won't be stored in
  // variables_map since that 'key' already has some valid 'value' (value
  // provided on cmdline)
  options_description config{"Configuration Options"};
  config.add_options()("ip",
                       value<std::string>()->default_value(default_listen_ip),
                       "IP on which to expose the service")(
      "port,p", value<short>()->default_value(default_listen_port),
      "Port on which to expose the service")(
      "logger_config", value<string>()->default_value(default_log_props),
      "Path to logging properties file")(
      "logger_reconfig_time",
      value<int>()->default_value(default_log_props_time_ms),
      "Interval (in ms) to check for updates to logging properties file")(
      "genesis_block", value<string>(),
      "Absolute path of file which contains genesis block json")(
      "blockchain_db_impl",
      value<string>()->default_value(default_blockchain_db_impl),
      "Name of the DB implementation backing the blockchain. "
      "Legal values: memory, rocksdb")("blockchain_db_path", value<string>(),
                                       "Path to blockchain database storage")
      // TOD(BWF): these are required, but this file needs to be rearranged to
      // make that work
      ("SBFT.public", value<string>(), "Path to SBFT public config file")(
          "SBFT.replica", value<string>(),
          "Path to SBFT private replica config file")(
          "SBFT.client", value<std::vector<string> >()->multitoken(),
          "Path to SBFT private client config file")(
          "api_worker_pool_size",
          value<int>()->default_value(default_api_worker_thread_pool_size),
          "Number of threads to create for handling TCP connections")(
          "transaction_list_max_count",
          value<int>()->default_value(default_transaction_list_max_count),
          "Maximum transactions returned for a transaction list query")(
          "gas_limit", value<uint64_t>()->default_value(default_gas_limit),
          "Maximum gas a transaction may consume");

  options_description all_options;  // description of all options
  all_options.add(generic).add(config);

  // First we parse command line options and see if --help
  // options was provided. In this case we don't need to
  // go for parsing config file. Otherwise call notify
  // for command line options and move to parsing config file
  store(command_line_parser(argc, argv).options(all_options).run(),
        options_map);

  // If cmdline options specified --help then we don't want
  // to do further processing for command line or
  // config file options
  if (options_map.count("help")) {
    std::cout << "VMware Project Concord" << std::endl;
    std::cout << all_options << std::endl;
    return options_map;
  }

  // call notify after checking "help", so that required
  // parameters are not required to get help (this call throws an
  // exception to exit the program if any parameters are invalid)
  notify(options_map);

  // Parse config file and populate map with those parameters
  // provided in config file.
  ifstream ifs(config_file.c_str());
  if (!ifs) {
    cerr << "Can not open config file: " << config_file << "\n"
         << " Going ahead with only command line options\n";
  } else {
    auto parsed = parse_config_file(ifs, config);
    store(parsed, options_map);
    notify(options_map);
  }

  return options_map;
}

namespace concord {
namespace config {

ConfigurationPath::ConfigurationPath()
    : isScope(false), useInstance(false), name(), index(0), subpath() {}

ConfigurationPath::ConfigurationPath(const std::string& name, bool isScope)
    : isScope(isScope), useInstance(false), name(name), index(0), subpath() {}

ConfigurationPath::ConfigurationPath(const std::string& name, size_t index)
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

std::string ConfigurationPath::toString() const {
  std::string str = name;
  if (isScope && useInstance) {
    str += "[" + std::to_string(index) + "]";
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
    throw std::invalid_argument(
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

}  // namespace config
}  // namespace concord

// Hash function for ConfigurationPath
size_t std::hash<concord::config::ConfigurationPath>::operator()(
    const concord::config::ConfigurationPath& path) const {
  std::hash<std::string> stringHash;
  size_t hash = stringHash(path.name);
  if (path.isScope) {
    hash ^= ((size_t)0x01) << (sizeof(size_t) * 4);
    if (path.useInstance) {
      std::hash<size_t> indexHash;
      hash ^= indexHash(path.index);
    }
    if (path.subpath) {
      std::hash<concord::config::ConfigurationPath> pathHash;
      hash ^= pathHash(*(path.subpath)) << 1;
    }
  }
  return hash;
}

namespace concord {
namespace config {

ConcordConfiguration::ConfigurationScope::ConfigurationScope(
    const ConcordConfiguration::ConfigurationScope& original)
    : instantiated(original.instantiated),
      instances(original.instances),
      description(original.description),
      size(original.size),
      sizerState(original.sizerState) {
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
  sizerState = original.sizerState;
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
      validatorState(other.validatorState),
      generator(other.generator),
      generatorState(other.generatorState) {}

ConcordConfiguration::ConfigurationParameter&
ConcordConfiguration::ConfigurationParameter::operator=(
    const ConcordConfiguration::ConfigurationParameter& other) {
  description = other.description, hasDefaultValue = other.hasDefaultValue;
  defaultValue = other.defaultValue;
  initialized = other.initialized;
  value = other.value;
  tags = other.tags;
  validator = other.validator;
  validatorState = other.validatorState;
  generator = other.generator;
  generatorState = other.generatorState;
  return *this;
}

void ConcordConfiguration::invalidateIterators() {
  for (auto iterator : iterators) {
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

std::string ConcordConfiguration::printCompletePath(
    const ConfigurationPath& localPath) const {
  std::unique_ptr<ConfigurationPath> completePath(getCompletePath(localPath));
  return completePath->toString();
}

std::string ConcordConfiguration::printCompletePath(
    const std::string& localParameter) const {
  std::unique_ptr<ConfigurationPath> completePath(
      getCompletePath(ConfigurationPath(localParameter, false)));
  return completePath->toString();
}

void ConcordConfiguration::updateSubscopePaths() {
  for (auto scope : scopes) {
    ConfigurationPath templatePath(scope.first, true);
    scope.second.instanceTemplate->scopePath.reset(
        getCompletePath(templatePath));
    scope.second.instanceTemplate->updateSubscopePaths();

    std::vector<ConcordConfiguration>& instances = scope.second.instances;
    for (size_t i = 0; i < instances.size(); ++i) {
      ConfigurationPath instancePath(scope.first, i);
      instances[i].scopePath.reset(getCompletePath(instancePath));
      instances[i].updateSubscopePaths();
    }
  }
}

ConcordConfiguration::ConfigurationParameter&
ConcordConfiguration::getParameter(const std::string& parameter,
                                   const std::string& failureMessage) {
  return const_cast<ConfigurationParameter&>(
      (const_cast<const ConcordConfiguration*>(this))
          ->getParameter(parameter, failureMessage));
}

const ConcordConfiguration::ConfigurationParameter&
ConcordConfiguration::getParameter(const std::string& parameter,
                                   const std::string& failureMessage) const {
  if (!contains(parameter)) {
    ConfigurationPath path(parameter, false);
    throw ConfigurationResourceNotFoundException(
        failureMessage + printCompletePath(path) + ": parameter not found.");
  }
  return parameters.at(parameter);
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
    : configurationState(),
      parentScope(),
      scopePath(),
      scopes(),
      parameters(),
      iterators() {}

ConcordConfiguration::ConcordConfiguration(const ConcordConfiguration& original)
    : configurationState(original.configurationState),
      parentScope(original.parentScope),
      scopePath(),
      scopes(original.scopes),
      parameters(original.parameters),
      iterators() {
  if (original.scopePath) {
    scopePath.reset(new ConfigurationPath(*(original.scopePath)));
  }
  for (auto scopeEntry : scopes) {
    scopeEntry.second.instanceTemplate->parentScope = this;
    for (auto instance : scopeEntry.second.instances) {
      instance.parentScope = this;
    }
  }
}

ConcordConfiguration::~ConcordConfiguration() {
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
  if (original.scopePath) {
    scopePath.reset(new ConfigurationPath(*(original.scopePath)));
  } else {
    scopePath.reset();
  }
  parameters = original.parameters;
  scopes = original.scopes;

  for (auto scopeEntry : scopes) {
    scopeEntry.second.instanceTemplate->parentScope = this;
    for (auto instance : scopeEntry.second.instances) {
      instance.parentScope = this;
    }
  }
  return *this;
}

void ConcordConfiguration::clear() {
  invalidateIterators();
  scopes.clear();
  parameters.clear();
}

void ConcordConfiguration::setConfigurationState(const std::string& state) {
  configurationState = state;
}

std::string ConcordConfiguration::getConfigurationState() const {
  return configurationState;
}

void ConcordConfiguration::declareScope(const std::string& scope,
                                        const std::string& description,
                                        ScopeSizer size, void* sizerState) {
  ConfigurationPath requestedScope(scope, true);
  if (scope.size() < 1) {
    throw std::invalid_argument(
        "Unable to create configuration scope: the empty string is not a valid "
        "name for a configuration scope.");
  }
  assert(kYAMLScopeTemplateSuffix.length() > 0);
  if ((scope.length() >= kYAMLScopeTemplateSuffix.length()) &&
      ((scope.substr(scope.length() - kYAMLScopeTemplateSuffix.length())) ==
       kYAMLScopeTemplateSuffix)) {
    throw std::invalid_argument("Cannot declare scope " + scope +
                                ": to facilitate configuration serialization, "
                                "scope names ending in \"" +
                                kYAMLScopeTemplateSuffix +
                                "\" are disallowed.");
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
    throw std::invalid_argument("Unable to create configuration scope " +
                                printCompletePath(requestedScope) +
                                ": provided scope sizer function is null.");
  }
  invalidateIterators();
  scopes[scope] = ConfigurationScope();
  scopes[scope].instanceTemplate.reset(new ConcordConfiguration());
  scopes[scope].instantiated = false;
  scopes[scope].description = description;
  scopes[scope].size = size;
  scopes[scope].sizerState = sizerState;

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

std::string ConcordConfiguration::getScopeDescription(
    const std::string& scope) const {
  if (!containsScope(scope)) {
    ConfigurationPath path(scope, true);
    throw ConfigurationResourceNotFoundException(
        "Cannot get description for scope " + printCompletePath(path) +
        ": scope does not exist.");
  }
  return scopes.at(scope).description;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::instantiateScope(
    const std::string& scope) {
  ConfigurationPath relativePath(scope, true);
  if (!containsScope(scope)) {
    throw ConfigurationResourceNotFoundException(
        "Unable to instantiate configuration scope " +
        printCompletePath(relativePath) + ": scope does not exist.");
  }
  ConfigurationScope& scopeEntry = scopes[scope];
  if (!(scopeEntry.size)) {
    throw std::invalid_argument("Unable to instantiate configuration scope " +
                                printCompletePath(relativePath) +
                                ": scope does not have a size function.");
  }
  size_t scopeSize;
  std::unique_ptr<ConfigurationPath> fullPath(getCompletePath(relativePath));
  ParameterStatus result =
      scopeEntry.size(*this, *fullPath, &scopeSize, scopeEntry.sizerState);
  if (result != ParameterStatus::VALID) {
    return result;
  }
  invalidateIterators();
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

ConcordConfiguration& ConcordConfiguration::subscope(const std::string& scope) {
  // This cast avoids duplicating code between the const and non-const versions
  // of this function.
  return const_cast<ConcordConfiguration&>(
      (const_cast<const ConcordConfiguration*>(this))->subscope(scope));
}

const ConcordConfiguration& ConcordConfiguration::subscope(
    const std::string& scope) const {
  if (!containsScope(scope)) {
    ConfigurationPath path(scope, true);
    throw ConfigurationResourceNotFoundException("Could not find scope " +
                                                 printCompletePath(path) + ".");
  }
  return *(scopes.at(scope).instanceTemplate);
}

ConcordConfiguration& ConcordConfiguration::subscope(const std::string& scope,
                                                     size_t index) {
  // This cast avoids duplicating code between the const and non-const versions
  // of this function.
  return const_cast<ConcordConfiguration&>(
      (const_cast<const ConcordConfiguration*>(this))->subscope(scope, index));
}

const ConcordConfiguration& ConcordConfiguration::subscope(
    const std::string& scope, size_t index) const {
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

bool ConcordConfiguration::containsScope(const std::string& name) const {
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

bool ConcordConfiguration::scopeIsInstantiated(const std::string& name) const {
  return containsScope(name) && scopes.at(name).instantiated;
}

size_t ConcordConfiguration::scopeSize(const std::string& scope) const {
  if (!scopeIsInstantiated(scope)) {
    ConfigurationPath path(scope, true);
    throw ConfigurationResourceNotFoundException("Cannot get size of scope " +
                                                 printCompletePath(path) +
                                                 ": scope does not exist.");
  }
  return scopes.at(scope).instances.size();
}

void ConcordConfiguration::declareParameter(const std::string& name,
                                            const std::string& description) {
  if (name.size() < 1) {
    throw std::invalid_argument(
        "Cannot declare parameter: the empty string is not a valid name for a "
        "configuration parameter.");
  }
  assert(kYAMLScopeTemplateSuffix.length() > 0);
  if ((name.length() >= kYAMLScopeTemplateSuffix.length()) &&
      ((name.substr(name.length() - kYAMLScopeTemplateSuffix.length())) ==
       kYAMLScopeTemplateSuffix)) {
    throw std::invalid_argument("Cannot declare parameter " + name +
                                ": to facilitate configuration serialization, "
                                "parameter names ending in \"" +
                                kYAMLScopeTemplateSuffix +
                                "\" are disallowed.");
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
  parameter.tags = std::unordered_set<std::string>();
  parameter.validator = nullptr;
  parameter.validatorState = nullptr;
  parameter.generator = nullptr;
  parameter.generatorState = nullptr;
}

void ConcordConfiguration::declareParameter(const std::string& name,
                                            const std::string& description,
                                            const std::string& defaultValue) {
  declareParameter(name, description);
  ConfigurationParameter& parameter = parameters[name];
  parameter.defaultValue = defaultValue;
  parameter.hasDefaultValue = true;
}

void ConcordConfiguration::tagParameter(const std::string& name,
                                        const std::vector<string>& tags) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot tag parameter ");
  for (auto tag : tags) {
    parameter.tags.emplace(tag);
  }
}

bool ConcordConfiguration::isTagged(const std::string& name,
                                    const std::string& tag) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Cannot check tags for parameter ");
  return parameter.tags.count(tag) > 0;
}

std::string ConcordConfiguration::getDescription(
    const std::string& name) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Cannot get description for parameter ");
  return parameter.description;
}

void ConcordConfiguration::addValidator(const std::string& name,
                                        Validator validator,
                                        void* validatorState) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot add validator to parameter ");
  if (!validator) {
    throw std::invalid_argument(
        "Cannot add validator to parameter " +
        printCompletePath(ConfigurationPath(name, false)) +
        ": validator given points to null.");
  }
  parameter.validator = validator;
  parameter.validatorState = validatorState;
}

void ConcordConfiguration::addGenerator(const std::string& name,
                                        Generator generator,
                                        void* generatorState) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot add generator to parameter ");
  if (!generator) {
    throw std::invalid_argument(
        "Cannot add generator to parameter " +
        printCompletePath(ConfigurationPath(name, false)) +
        ": generator given points to null.");
  }
  parameter.generator = generator;
  parameter.generatorState = generatorState;
}

bool ConcordConfiguration::contains(const std::string& name) const {
  return parameters.count(name) > 0;
}

bool ConcordConfiguration::contains(const ConfigurationPath& path) const {
  if (path.subpath && path.isScope) {
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

bool ConcordConfiguration::hasValue(const std::string& name) const {
  return (contains(name)) && (parameters.at(name).initialized);
}

bool ConcordConfiguration::hasValue(const ConfigurationPath& path) const {
  // Note we do not give an error message for getParameter to use because
  // getParameter should not fail if contains(path) is true.
  if (!contains(path)) {
    return false;
  }
  const ConcordConfiguration* containingScope = this;
  if (path.isScope && path.subpath) {
    containingScope = &(subscope(path.trimLeaf()));
  }
  return containingScope->hasValue(path.getLeaf().name);
}

std::string ConcordConfiguration::getValue(const std::string& name) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Could not get value for parameter ");
  if (!(parameter.initialized)) {
    throw ConfigurationResourceNotFoundException(
        "Could not get value for parameter " +
        printCompletePath(ConfigurationPath(name, false)) +
        ": parameter is uninitialized.");
  }
  return parameter.value;
}

std::string ConcordConfiguration::getValue(
    const ConfigurationPath& path) const {
  if (!contains(path)) {
    throw ConfigurationResourceNotFoundException(
        "Could not get value for parameter " + printCompletePath(path) +
        ": parameter not found.");
  }
  const ConcordConfiguration* containingScope = this;
  if (path.subpath) {
    containingScope = &(subscope(path.trimLeaf()));
  }
  const ConfigurationParameter& parameter = containingScope->getParameter(
      path.getLeaf().name, "Could not get value for parameter ");
  if (!(parameter.initialized)) {
    throw ConfigurationResourceNotFoundException(
        "Could not get value for parameter " + printCompletePath(path) +
        ": parameter is uninitialized.");
  }
  return parameter.value;
}

ConcordConfiguration::ParameterStatus ConcordConfiguration::loadValue(
    const std::string& name, const std::string& value,
    std::string* failureMessage, bool overwrite, std::string* prevValue) {
  ConfigurationParameter& parameter =
      getParameter(name, "Could not load value for parameter ");
  std::unique_ptr<ConfigurationPath> path(
      getCompletePath(ConfigurationPath(name, false)));
  ParameterStatus status = ParameterStatus::VALID;
  std::string message;
  if (parameter.validator) {
    status = parameter.validator(value, *this, *path, &message,
                                 parameter.validatorState);
  }
  if (failureMessage && (status != ParameterStatus::VALID)) {
    *failureMessage = message;
  }

  if (status != ParameterStatus::INVALID) {
    if (parameter.initialized) {
      if (overwrite) {
        if (prevValue) {
          *prevValue = parameter.value;
        }
        parameter.value = value;
      }
    } else {
      parameter.value = value;
      parameter.initialized = true;
    }
  }

  return status;
}

void ConcordConfiguration::eraseValue(const std::string& name,
                                      std::string* prevValue) {
  ConfigurationParameter& parameter =
      getParameter(name, "Could not erase value for parameter ");
  if (prevValue && parameter.initialized) {
    *prevValue = parameter.value;
  }
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
    const std::string& name, std::string* failureMessage, bool overwrite,
    std::string* prevValue) {
  ConfigurationParameter& parameter =
      getParameter(name, "Could not load default value for parameter ");

  if (!parameter.hasDefaultValue) {
    throw ConfigurationResourceNotFoundException(
        "Could not load default value for parameter " +
        printCompletePath(name) +
        ": this parameter does not have a default value.");
  }
  return loadValue(name, parameter.defaultValue, failureMessage, overwrite,
                   prevValue);
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
    const std::string& name, std::string* failureMessage) const {
  const ConfigurationParameter& parameter =
      getParameter(name, "Could not validate contents of parameter ");
  std::unique_ptr<ConfigurationPath> path(
      getCompletePath(ConfigurationPath(name, false)));
  if (!parameter.initialized) {
    return ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  ParameterStatus status = ParameterStatus::VALID;
  std::string message;
  if (parameter.validator) {
    status = parameter.validator(parameter.value, *this, *path, &message,
                                 parameter.validatorState);
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
    const std::string& name, std::string* failureMessage, bool overwrite,
    std::string* prevValue) {
  ConfigurationParameter& parameter =
      getParameter(name, "Cannot generate value for parameter ");
  std::unique_ptr<ConfigurationPath> path(
      getCompletePath(ConfigurationPath(name, false)));

  if (!parameter.generator) {
    throw ConfigurationResourceNotFoundException(
        "Cannot generate value for parameter " + printCompletePath(*path) +
        ": no generator function has been specified for this parameter.");
  }
  std::string generatedValue;
  ParameterStatus status = parameter.generator(*this, *path, &generatedValue,
                                               parameter.generatorState);
  if (status == ParameterStatus::VALID) {
    std::string message;
    if (parameter.validator) {
      status = parameter.validator(generatedValue, *this, *path, &message,
                                   parameter.validatorState);
    }
    if (failureMessage && (status != ParameterStatus::VALID)) {
      *failureMessage = message;
    }
    if ((status != ParameterStatus::INVALID) &&
        (!parameter.initialized || overwrite)) {
      if (prevValue && parameter.initialized) {
        *prevValue = parameter.value;
      }
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
  for (auto iterator : iterators) {
    iterator->invalidate();
  }
  iterators.clear();
}

ParameterSelection::ParameterSelection(ConcordConfiguration& config,
                                       ParameterSelector selector,
                                       void* selectorState)
    : config(&config),
      selector(selector),
      selectorState(selectorState),
      iterators() {
  if (!selector) {
    throw std::invalid_argument(
        "Attempting to construct a ParameterSelection with a null parameter "
        "selction function.");
  }
}

ParameterSelection::ParameterSelection(const ParameterSelection& original)
    : config(original.config),
      selector(original.selector),
      selectorState(original.selectorState) {}

ParameterSelection::~ParameterSelection() { invalidateIterators(); }

bool ParameterSelection::contains(const ConfigurationPath& parameter) const {
  return (config->contains(parameter)) &&
         (selector(*config, parameter, selectorState));
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
                                           std::ostream* errorOut,
                                           bool overwrite) {
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
      std::string templateName = path.name + kYAMLScopeTemplateSuffix;
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

    std::string failureMessage;
    ConcordConfiguration::ParameterStatus status = config.loadValue(
        path.name, obj[path.name].Scalar(), &failureMessage, overwrite);
    if (errorOut &&
        (status == ConcordConfiguration::ParameterStatus::INVALID)) {
      (*errorOut) << "Cannot load value for parameter " << path.name << ": "
                  << failureMessage << std::endl;
    }
  }
}

YAMLConfigurationInput::YAMLConfigurationInput(std::istream& input)
    : input(&input), yaml(), success(false) {}

void YAMLConfigurationInput::parseInput() {
  yaml.reset(YAML::Load(*input));
  success = true;
}

YAMLConfigurationInput::~YAMLConfigurationInput() {}

void YAMLConfigurationOutput::addParameterToYAML(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    YAML::Node& yaml) {
  // Note this helper function expects that it has already been validated or
  // otherwise guaranteed that path is a valid path to a declared parameter in
  // config and yaml is an associative array.
  if (!config.contains(path) || !config.hasValue(path) || !yaml.IsMap()) {
    return;
  }

  if (path.isScope && path.subpath) {
    YAML::Node subscope;
    std::string pathName;
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
    yaml[path.name] = config.getValue(path.name);
  }
}

YAMLConfigurationOutput::YAMLConfigurationOutput(std::ostream& output)
    : output(&output), yaml() {}

YAMLConfigurationOutput::~YAMLConfigurationOutput() {}

// Helper functions to the validation, generation, and sizing functions to
// follow.

static ConcordConfiguration::ParameterStatus validateNumberOfPrincipalsInBounds(
    uint16_t fVal, uint16_t cVal, uint16_t clientProxiesPerReplica,
    std::string* failureMessage) {
  // Conditions which should have been validated before this function was
  // called.
  assert((fVal > 0) && (clientProxiesPerReplica > 0));

  // The principals consist of (3F + 2C + 1) SBFT replicas plus
  // client_proxies_per_replica client proxies for each of these replicas.
  uint64_t numPrincipals = 3 * ((uint64_t)fVal) + 2 * ((uint64_t)cVal) + 1;
  numPrincipals =
      numPrincipals + ((uint64_t)clientProxiesPerReplica) * numPrincipals;

  if (numPrincipals > UINT16_MAX) {
    if (failureMessage) {
      *failureMessage =
          "Invalid combination of values for f_val (" + std::to_string(fVal) +
          "), c_val (" + std::to_string(cVal) +
          "), and client_proxies_per_replica (" +
          std::to_string(clientProxiesPerReplica) +
          "): these parameters imply too many (" +
          std::to_string(numPrincipals) + ") SBFT principals; a maximum of " +
          std::to_string(UINT16_MAX) + " principals are currently supported.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static std::pair<std::string, std::string> parseCryptosystemSelection(
    std::string selection) {
  std::pair<std::string, std::string> parseRes({"", ""});
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

// Declaration and implementation of various functions used by
// specifyConfiguration to specify how to size scopes and validatate and
// generate parameters. Pointers to these functions are given to the
// ConcordConfiguration object specifyConfiguration builds so that the
// configuration system can call them at the appropriate time (for example,
// parameter validators are called automatically when parameters are loaded).

// Computes the total number of Concord nodes. Note we currently assume there is
// one Concord node per SBFT replica, and the SBFT algorithm specifies that the
// cluster size in terms of its F and C parameters is (3F + 2C + 1) replicas.
static ConcordConfiguration::ParameterStatus sizeNodes(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  assert(output);

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
  *output = numNodes;
  return ConcordConfiguration::ParameterStatus::VALID;
}

// Computes the number of SBFT replicas per Concord node. Note that, at the time
// of this writing, we assume there is exactly one SBFT replica per Concord
// node.
static ConcordConfiguration::ParameterStatus sizeReplicas(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  assert(output);

  *output = 1;
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus sizeClientProxies(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  assert(output);

  if (!config.hasValue<uint16_t>("client_proxies_per_replica")) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (config.validate("client_proxies_per_replica") !=
      ConcordConfiguration::ParameterStatus::VALID) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }

  *output = config.getValue<uint16_t>("client_proxies_per_replica");
  return ConcordConfiguration::ParameterStatus::VALID;
}

static const std::pair<unsigned long long, unsigned long long>
    kPositiveIntLimits({1, INT_MAX});
static const std::pair<unsigned long long, unsigned long long>
    kPositiveUInt16Limits({1, UINT16_MAX});
static const std::pair<unsigned long long, unsigned long long>
    kPositiveUInt64Limits({1, UINT64_MAX});
static const std::pair<unsigned long long, unsigned long long>
    kPositiveULongLongLimits({1, ULLONG_MAX});
static const std::pair<unsigned long long, unsigned long long> kUInt16Limits(
    {0, UINT16_MAX});

static ConcordConfiguration::ParameterStatus validateUInt(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  assert(state);
  const std::pair<unsigned long long, unsigned long long>* limits =
      static_cast<std::pair<unsigned long long, unsigned long long>*>(state);
  assert(limits->first <= limits->second);

  unsigned long long intVal;
  try {
    intVal = std::stoull(value);
  } catch (std::invalid_argument& e) {
    if (failureMessage) {
      *failureMessage = "Invalid value for parameter " + path.toString() +
                        ": \"" + value + "\". An integer is required.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  } catch (std::out_of_range& e) {
    if (failureMessage) {
      *failureMessage =
          "Invalid value for parameter " + path.toString() + ": \"" + value +
          "\". An integer in the range (" + std::to_string(limits->first) +
          ", " + std::to_string(limits->second) + "), inclusive, is required.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  if ((intVal < limits->first) || (intVal > limits->second)) {
    if (failureMessage) {
      *failureMessage =
          "Invalid value for parameter " + path.toString() + ": \"" + value +
          "\". An integer in the range (" + std::to_string(limits->first) +
          ", " + std::to_string(limits->second) + "), inclusive, is required.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validateClientProxiesPerReplica(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kPositiveUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  } else {
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val")) {
      if (failureMessage) {
        *failureMessage =
            "Cannot fully validate client_proxies_per_replica: value is in "
            "range, but f_val and c_val must both also be known to veriy that "
            "the total number of SBFT principals needed is in range.";
      }
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t clientProxiesPerReplica = (uint16_t)(std::stoull(value));
    return validateNumberOfPrincipalsInBounds(
        config.getValue<uint16_t>("f_val"), config.getValue<uint16_t>("c_val"),
        clientProxiesPerReplica, failureMessage);
  }
}

static ConcordConfiguration::ParameterStatus validateCryptosys(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  std::pair<std::string, std::string> cryptoSelection =
      parseCryptosystemSelection(value);
  if (!Cryptosystem::isValidCryptosystemSelection(cryptoSelection.first,
                                                  cryptoSelection.second)) {
    if (failureMessage) {
      *failureMessage =
          "Invalid cryptosystem selection for " + path.toString() + ": \"" +
          value +
          "\" is not a recognized and supported selection of cryptosystem.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }

  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val")) {
    if (failureMessage) {
      *failureMessage = "Cannot fully validate cryptosystem selection for " +
                        path.toString() +
                        ": f_val and c_val must be known to determine the "
                        "threshold and total number of signers.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }

  uint16_t fVal = config.getValue<uint16_t>("f_val");
  uint16_t cVal = config.getValue<uint16_t>("c_val");
  uint64_t unvalidatedNumSigners =
      3 * ((uint64_t)fVal) + 2 * ((uint64_t)cVal) + 1;
  if (unvalidatedNumSigners > UINT16_MAX) {
    if (failureMessage) {
      *failureMessage =
          "Cryptosystem selection for " + path.toString() +
          " cannot be valid: f_val(" + std::to_string(fVal) + ") and c_val(" +
          std::to_string(cVal) + ") imply the number of SBFT replicas(" +
          std::to_string(unvalidatedNumSigners) +
          "), and by extension the number of threshold signers for this "
          "cryptosystem, exceeds the currently supported limit of " +
          std::to_string(UINT16_MAX) + ".";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }

  uint16_t numSigners = (uint16_t)unvalidatedNumSigners;
  uint16_t threshold;

  // Compute the threshold, which is different for each of the four
  // cryptosystems we are currently using. The code specific to each
  // cryptosystem here will hopefully be factored out into its own function in a
  // futre round of code cleanup in which we intend to replace
  // validator/generator/scopesizer function pointers with objects.
  std::string sysName = path.getLeaf().name;
  if (sysName == "execution_cryptosys") {
    threshold = fVal + 1;
  } else if (sysName == "slow_commit_cryptosys") {
    threshold = 2 * fVal + cVal + 1;
  } else if (sysName == "commit_cryptosys") {
    threshold = 3 * fVal + cVal + 1;
  } else if (sysName == "optimistic_commit_cryptosys") {
    threshold = 3 * fVal + 2 * cVal + 1;
  } else {
    if (failureMessage) {
      *failureMessage =
          "Cannot determine validity of cryptosystem selection for " +
          path.toString() +
          ": The validator lacks knowledge of the correct threshold for this "
          "cryptosystem.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }

  if (!Cryptosystem::isValidCryptosystemSelection(cryptoSelection.first,
                                                  cryptoSelection.second,
                                                  numSigners, threshold)) {
    if (failureMessage) {
      *failureMessage = "Invalid cryptosystem selection for " +
                        path.toString() + ": cryptosytem selection \"" + value +
                        "\" is not supported with the required threshold(" +
                        std::to_string(threshold) + ") and number of signers(" +
                        std::to_string(numSigners) + ") for this system.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validatePublicKey(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  assert(state);
  std::unique_ptr<Cryptosystem>* cryptosystemPointer =
      static_cast<std::unique_ptr<Cryptosystem>*>(state);

  if (!(*cryptosystemPointer)) {
    if (failureMessage) {
      *failureMessage = "Cannot assess validity of threshold public key " +
                        path.toString() +
                        ": corresponding cryptosystem is not initialized.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (!((*cryptosystemPointer)->isValidPublicKey(value))) {
    if (failureMessage) {
      *failureMessage = "Invalid threshold public key for " + path.toString() +
                        ": \"" + value + "\".";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus getThresholdPublicKey(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  assert(state);
  std::unique_ptr<Cryptosystem>* cryptosystemPointer =
      static_cast<std::unique_ptr<Cryptosystem>*>(state);

  if (!(*cryptosystemPointer)) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  try {
    *output = (*cryptosystemPointer)->getSystemPublicKey();
    return ConcordConfiguration::ParameterStatus::VALID;
  } catch (UninitializedCryptosystemException& e) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
}

static ConcordConfiguration::ParameterStatus validateCVal(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  } else {
    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("client_proxies_per_replica")) {
      if (failureMessage) {
        *failureMessage =
            "Cannot fully validate c_val: value is in range, but f_val and "
            "client_proxies_per_replica must both also be known to veriy that "
            "the total number of SBFT principals needed is in range.";
      }
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t cVal = (uint16_t)(std::stoull(value));
    return validateNumberOfPrincipalsInBounds(
        config.getValue<uint16_t>("f_val"), cVal,
        config.getValue<uint16_t>("client_proxies_per_replica"),
        failureMessage);
  }
}

static ConcordConfiguration::ParameterStatus validateFVal(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kPositiveUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    if (failureMessage) {
      *failureMessage =
          "Cannot fully validate f_val: value is in range, but c_val and "
          "client_proxies_per_replica must both also be known to veriy that "
          "the total number of SBFT principals needed is in range.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  uint16_t fVal = (uint16_t)(std::stoull(value));
  return validateNumberOfPrincipalsInBounds(
      fVal, config.getValue<uint16_t>("c_val"),
      config.getValue<uint16_t>("client_proxies_per_replica"), failureMessage);
}

static ConcordConfiguration::ParameterStatus validateNumClientProxies(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kPositiveUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    if (failureMessage) {
      *failureMessage =
          "Cannot validate num_client_proxies: values for f_val, c_val and "
          "client_proxies_per_replica are required to determine expected value "
          "of num_client_proxies.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  uint16_t expectedNumClientProxies =
      config.getValue<uint16_t>("client_proxies_per_replica") *
      (3 * config.getValue<uint16_t>("f_val") +
       2 * config.getValue<uint16_t>("c_val") + 1);
  if ((uint16_t)(std::stoull(value)) != expectedNumClientProxies) {
    if (failureMessage) {
      *failureMessage =
          "Invalid valud for num_client_proxies: " + value +
          "; num_client_proxies must be equal to client_proxies_per_replica * "
          "(3 * f_val + 2 * c_val + 1).";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus computeNumClientProxies(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  *output =
      std::to_string(config.getValue<uint16_t>("client_proxies_per_replica") *
                     (3 * config.getValue<uint16_t>("f_val") +
                      2 * config.getValue<uint16_t>("c_val") + 1));
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validateNumPrincipals(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kPositiveUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    if (failureMessage) {
      *failureMessage =
          "Cannot validate num_principals: values for f_val, c_val and "
          "client_proxies_per_replica are required to determine expected value "
          "of num_principals.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  uint16_t expectedNumPrincipals =
      (config.getValue<uint16_t>("client_proxies_per_replica") + 1) *
      (3 * config.getValue<uint16_t>("f_val") +
       2 * config.getValue<uint16_t>("c_val") + 1);
  if ((uint16_t)(std::stoull(value)) != expectedNumPrincipals) {
    if (failureMessage) {
      *failureMessage =
          "Invalid value for num_principals: " + value +
          "; num_principals must be equal to (1 + client_proxies_per_replica) "
          "* (3 * f_val + 2 * c_val + 1).";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus computeNumPrincipals(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  *output = std::to_string(
      (1 + config.getValue<uint16_t>("client_proxies_per_replica")) *
      (3 * config.getValue<uint16_t>("f_val") +
       2 * config.getValue<uint16_t>("c_val") + 1));
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validateNumReplicas(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kPositiveUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    if (failureMessage) {
      *failureMessage =
          "Cannot validate num_replicas: values for f_val, c_val and "
          "client_proxies_per_replica are required to determine expected value "
          "of num_replicas.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  uint16_t expectedNumReplicas = 3 * config.getValue<uint16_t>("f_val") +
                                 2 * config.getValue<uint16_t>("c_val") + 1;
  if ((uint16_t)(std::stoull(value)) != expectedNumReplicas) {
    if (failureMessage) {
      *failureMessage =
          "Invalid value for num_replicas: " + value +
          "; num_replicas must be equal to 3 * f_val + 2 * c_val + 1.";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus computeNumReplicas(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  *output = std::to_string(3 * config.getValue<uint16_t>("f_val") +
                           2 * config.getValue<uint16_t>("c_val") + 1);
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validatePositiveReplicaInt(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  const std::pair<unsigned long long, unsigned long long>* limits;
  if (config.getConfigurationStateLabel() == "concord_node") {
    limits = &kPositiveIntLimits;
  } else {
    limits = &kPositiveULongLongLimits;
  }
  return validateUInt(value, config, path, failureMessage,
                      const_cast<void*>(reinterpret_cast<const void*>(limits)));
}

static ConcordConfiguration::ParameterStatus validateDatabaseImplementation(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  if (!((value == "memory") || (value == "rocksdb"))) {
    if (failureMessage) {
      *failureMessage =
          "Unrecognized database implementation: \"" + value + "\".";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validatePortNumber(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  return validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kUInt16Limits)));
}

static ConcordConfiguration::ParameterStatus validatePrivateKey(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  assert(state);
  std::unique_ptr<Cryptosystem>* cryptosystemPointer =
      static_cast<std::unique_ptr<Cryptosystem>*>(state);

  if (!(*cryptosystemPointer)) {
    if (failureMessage) {
      *failureMessage = "Cannot assess validity of threshold private key " +
                        path.toString() +
                        ": corresponding cryptosystem is not initialized.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (!((*cryptosystemPointer)->isValidPrivateKey(value))) {
    if (failureMessage) {
      *failureMessage = "Invalid threshold private key for " + path.toString() +
                        ": \"" + value + "\".";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus getThresholdPrivateKey(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  assert(state);
  std::unique_ptr<Cryptosystem>* cryptosystemPointer =
      static_cast<std::unique_ptr<Cryptosystem>*>(state);

  if (!(*cryptosystemPointer)) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  try {
    // This validator should only be called with paths of the form:
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

    *output = (*cryptosystemPointer)->getPrivateKey(signer);
    return ConcordConfiguration::ParameterStatus::VALID;
  } catch (UninitializedCryptosystemException& e) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
}

static ConcordConfiguration::ParameterStatus validateVerificationKey(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  assert(state);
  std::unique_ptr<Cryptosystem>* cryptosystemPointer =
      static_cast<std::unique_ptr<Cryptosystem>*>(state);

  if (!(*cryptosystemPointer)) {
    if (failureMessage) {
      *failureMessage =
          "Cannot assess validity of threshold verification key " +
          path.toString() + ": corresponding cryptosystem is not initialized.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  if (!((*cryptosystemPointer)->isValidVerificationKey(value))) {
    if (failureMessage) {
      *failureMessage = "Invalid threshold verification key for " +
                        path.toString() + ": \"" + value + "\".";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus getThresholdVerificationKey(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  assert(state);
  std::unique_ptr<Cryptosystem>* cryptosystemPointer =
      static_cast<std::unique_ptr<Cryptosystem>*>(state);

  if (!(*cryptosystemPointer)) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  try {
    // This validator should only be called with paths of the form:
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

    *output = ((*cryptosystemPointer)->getSystemVerificationKeys())[signer];
    return ConcordConfiguration::ParameterStatus::VALID;
  } catch (UninitializedCryptosystemException& e) {
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
}

static ConcordConfiguration::ParameterStatus validatePrincipalId(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  ConcordConfiguration::ParameterStatus res = validateUInt(
      value, config, path, failureMessage,
      const_cast<void*>(reinterpret_cast<const void*>(&kUInt16Limits)));
  if (res != ConcordConfiguration::ParameterStatus::VALID) {
    return res;
  }
  uint16_t principalID = (uint16_t)(std::stoull(value));

  if (!config.hasValue<uint16_t>("f_val") ||
      !config.hasValue<uint16_t>("c_val") ||
      !config.hasValue<uint16_t>("client_proxies_per_replica")) {
    if (failureMessage) {
      *failureMessage =
          "Cannot fully validate Concord-BFT principal ID for " +
          path.toString() +
          ": f_val, c_val, and client_proxies_per_replica are required to "
          "determine bounds for maximum principal ID.";
    }
    return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  }
  uint16_t fVal = config.getValue<uint16_t>("f_val");
  uint16_t cVal = config.getValue<uint16_t>("c_val");
  uint16_t numReplicas = 3 * fVal + 2 * cVal + 1;
  uint16_t clientProxiesPerReplica =
      config.getValue<uint16_t>("client_proxies_per_replica");
  uint16_t numPrincipals = numReplicas * (1 + clientProxiesPerReplica);

  // The path to a principal Id should be of one of these forms:
  //   node[i]/replica[0]/principal_id
  //   node[i]/client_proxy[j]/principal_id
  assert(path.isScope && path.subpath);

  if (path.subpath->name == "replica") {
    if (principalID >= numReplicas) {
      if (failureMessage) {
        *failureMessage =
            "Invalid principal ID for " + path.toString() + ": " +
            std::to_string(principalID) +
            ". Principal IDs for replicas must be less than num_replicas.";
      }
      return ConcordConfiguration::ParameterStatus::INVALID;
    }

  } else {
    assert(path.subpath->name == "client_proxy");

    if ((principalID < numReplicas) || (principalID >= numPrincipals)) {
      if (failureMessage) {
        *failureMessage =
            "Invalid principal ID for " + path.toString() + ": " +
            std::to_string(principalID) +
            ". Principal IDs for client proxies should be in the range "
            "(num_replicas, num_principals - 1), inclusive.";
      }
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
      if (failureMessage) {
        *failureMessage = "Invalid principal ID for " + path.toString() + ": " +
                          std::to_string(principalID) +
                          ". This ID is non-unique; it duplicates the ID for " +
                          replicaPath.toString() + ".";
      }
      return ConcordConfiguration::ParameterStatus::INVALID;
    }

    for (size_t j = 0; j < clientProxiesPerReplica; ++j) {
      ConfigurationPath clientProxyPath("node", (size_t)i);
      clientProxyPath.subpath.reset(new ConfigurationPath("client_proxy", j));
      clientProxyPath.subpath->subpath.reset(
          new ConfigurationPath("principal_id", false));
      if (!config.hasValue<uint16_t>(clientProxyPath)) {
        if (clientProxyPath != path) {
          res = ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
        }
      } else if ((config.getValue<uint16_t>(clientProxyPath) == principalID) &&
                 (clientProxyPath != path)) {
        if (failureMessage) {
          *failureMessage =
              "Invalid principal ID for " + path.toString() + ": " +
              std::to_string(principalID) +
              ". This ID is non-unique; it duplicates the ID for " +
              clientProxyPath.toString() + ".";
        }
        return ConcordConfiguration::ParameterStatus::INVALID;
      }
    }
  }

  if (failureMessage &&
      (res ==
       ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION)) {
    *failureMessage = "Cannot fully validate principal ID for " +
                      path.toString() +
                      ": Not all other principal IDs are known, but are "
                      "required to check for uniqueness.";
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus computePrincipalId(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  // The path to a principal Id should be of one of these forms:
  //   node[i]/replica[0]/principal_id
  //   node[i]/client_proxy[j]/principal_id

  assert(path.isScope && path.subpath && path.useInstance);

  if (path.subpath->name == "replica") {
    *output = std::to_string(path.index);
  } else {
    assert((path.subpath->name == "client_proxy") && path.subpath->isScope &&
           path.subpath->useInstance);

    if (!config.hasValue<uint16_t>("f_val") ||
        !config.hasValue<uint16_t>("c_val")) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    uint16_t numReplicas = 3 * config.getValue<uint16_t>("f_val") +
                           2 * config.getValue<uint16_t>("c_val") + 1;

    *output =
        std::to_string(path.index + numReplicas * (1 + path.subpath->index));
  }

  return ConcordConfiguration::ParameterStatus::VALID;
}

const size_t kRSAPublicKeyHexadecimalLength = 584;
// Note we do not have a correpsonding kRSAPrivateKeyHexadecimalLength constant
// because the hexadecimal length of RSA private keys actually seems to vary a
// little in the current serialization of them.

static ConcordConfiguration::ParameterStatus validateRSAPrivateKey(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  if (!(std::regex_match(value, std::regex("[0-9A-Fa-f]+")))) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus getRSAPrivateKey(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  // The path to an RSA private key should be of one of these forms:
  //   node[i]/replica[0]/private_key
  //   node[i]/client_proxy[j]/private_key
  // We infer what replica or client proxy the key is for from the path.
  assert(path.isScope && path.useInstance && path.subpath);
  const ConcordPrimaryConfigurationAuxiliaryState* auxState =
      dynamic_cast<const ConcordPrimaryConfigurationAuxiliaryState*>(
          config.getAuxiliaryState());
  assert(auxState);

  size_t nodeIndex = path.index;

  if (path.subpath->name == "replica") {
    if (nodeIndex >= auxState->replicaRSAKeys.size()) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    *output = auxState->replicaRSAKeys[nodeIndex].first;
  } else {
    assert((path.subpath->name == "client_proxy") && path.subpath->isScope &&
           path.subpath->useInstance);
    size_t clientProxyIndex = path.subpath->index;

    if ((nodeIndex >= auxState->clientProxyRSAKeys.size()) ||
        (clientProxyIndex >= auxState->clientProxyRSAKeys[nodeIndex].size())) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    *output = auxState->clientProxyRSAKeys[nodeIndex][clientProxyIndex].first;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus validateRSAPublicKey(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  if (!((value.length() == kRSAPublicKeyHexadecimalLength) &&
        std::regex_match(value, std::regex("[0-9A-Fa-f]+")))) {
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus getRSAPublicKey(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  // The path to an RSA public key should be of one of these forms:
  //   node[i]/replica[0]/public_key
  //   node[i]/client_proxy[j]/public_key
  // We infer what replica or client proxy the key is for from the path.
  assert(path.isScope && path.useInstance && path.subpath);
  const ConcordPrimaryConfigurationAuxiliaryState* auxState =
      dynamic_cast<const ConcordPrimaryConfigurationAuxiliaryState*>(
          config.getAuxiliaryState());
  assert(auxState);

  size_t nodeIndex = path.index;

  if (path.subpath->name == "replica") {
    if (nodeIndex >= auxState->replicaRSAKeys.size()) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    *output = auxState->replicaRSAKeys[nodeIndex].second;
  } else {
    assert((path.subpath->name == "client_proxy") && path.subpath->isScope &&
           path.subpath->useInstance);
    size_t clientProxyIndex = path.subpath->index;

    if ((nodeIndex >= auxState->clientProxyRSAKeys.size()) ||
        (clientProxyIndex >= auxState->clientProxyRSAKeys[nodeIndex].size())) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }
    *output = auxState->clientProxyRSAKeys[nodeIndex][clientProxyIndex].second;
  }
  return ConcordConfiguration::ParameterStatus::VALID;
}

}  // namespace config
}  // namespace concord
