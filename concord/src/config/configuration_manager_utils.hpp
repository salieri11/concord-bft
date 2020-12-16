// Copyright 2019-2020 VMware, all rights reserved

// Declarations and definitions of classes (and helper code to those classes)
// used only by code (ex: the specifyConfiguration function) defining the
// current Concord configuration. These classes consist primarily of
// implementations of ConcordConfiguration::ParameterValidator,
// ConcordConfiguration::ParameterGenerator, ConcordConfiguration::ScopeSizer,
// and ParameterSelection::ParameterSelector classes specific to the current
// Concord configuration. These declarations and definitions are written in this
// file rather than directly with the code (ex: the specifyConfiguration
// function) that uses them in the interest of reducing the lengthiness of the
// code files for the Concord configuration system in the interest of improving
// code neavigability.

// Note this file is not intended for inclusion anywhere other than
// configuration_manager.cpp.

#include <regex>

#include <cryptopp/dll.h>
#include <boost/algorithm/string.hpp>

#include "configuration_manager.hpp"

// Though using directives should generally be avoided in header files, we use
// them here for convenience since this file is intended more as a subsection to
// be inserted into configuration_manager.cpp than as a general header file.
using concord::config::ConcordConfiguration;
using concord::config::ConcordPrimaryConfigurationAuxiliaryState;
using concord::config::ConfigurationPath;
using concord::config::ConfigurationResourceNotFoundException;
using concord::config::InvalidConfigurationInputException;
using concord::config::kConcordNodeConfigurationStateLabel;
using concord::config::kRSAKeyLength;
using concord::config::NodesSizer;
using concord::config::ParameterSelection;
using concord::config::UIntValidator;
using std::exception;
using std::invalid_argument;
using std::make_unique;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::unordered_set;
using std::vector;

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

// Some helper functions to the validation, generation, and sizing functions to
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

    // Note that, depending on the order in which parameters are loaded,
    // we may not yet be able to tell which node is local in this
    // configuration at the time this validator is called when first loading
    // hosts into the config. To handle this case, we return
    // INSUFFICIENT_INFORMATION if detectLocalNode throws an exception.
    size_t local_node_index;
    bool is_ro_replica;
    try {
      std::tie(local_node_index, is_ro_replica) =
          detectLocalNode(nonConstConfig);
    } catch (const ConfigurationResourceNotFoundException& e) {
      return ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
    }

    if (config.hasValue<bool>("use_loopback_for_local_hosts") &&
        config.getValue<bool>("use_loopback_for_local_hosts") &&
        (value != "127.0.0.1") && !is_ro_replica) {
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

class OperatorNodeConfigurationSelector
    : public ParameterSelection::ParameterSelector {
 public:
  virtual ~OperatorNodeConfigurationSelector() override {}
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

    if (path.isScope && (path.name == "operator_node")) {
      return true;
    }
    const ConcordConfiguration* containingScope = &config;
    if (path.isScope) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }

    return !(containingScope->isTagged(path.getLeaf().name, "private"));
  }
};

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

class SingleOperatorNodeSizer : public ConcordConfiguration::ScopeSizer {
 public:
  virtual ~SingleOperatorNodeSizer() override {}
  virtual ConcordConfiguration::ParameterStatus sizeScope(
      const ConcordConfiguration& config, const ConfigurationPath& path,
      size_t& output) override {
    output = (size_t)1;
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

class S3ProtocolValidator : public ConcordConfiguration::ParameterValidator {
 public:
  ~S3ProtocolValidator() override {}

  ConcordConfiguration::ParameterStatus validate(
      const std::string& value, const ConcordConfiguration& config,
      const ConfigurationPath& path, std::string& failureMessage) override {
    // convert the value to lowercase - s3 client in concord-bft doesn't care
    // about the case
    std::string s3_proto;
    for (auto& v : value) s3_proto.push_back(std::tolower(v));

    if (s3_proto != "http" && s3_proto != "https") {
      failureMessage =
          "Invalid S3 protocol. Supported values are http and https.";
      return ConcordConfiguration::ParameterStatus::INVALID;
    }

    return ConcordConfiguration::ParameterStatus::VALID;
  }
};