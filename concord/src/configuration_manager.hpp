// Copyright 2018-2019 VMware, all rights reserved
//
// Definitions used by configuration management system.
// The most central component of this system is the ConcordConfiguration class,
// which provides a framework for defining and managing configuration
// information. Also included in this file are:
// - ConfigurationPath: a struct for specifying paths to specific configuration
// parameters within a ConcordConfiguration, accounting for the fact that
// parameters may belong to arbitrarilly nested instantiable "scopes" such as
// per-node or per-replica parameters.
// - ParameterSelection: a utility class provided to facilitate turning an
// arbitrary function for picking ConfigurationParameters into an iterable set.

#ifndef CONFIGURATION_MANAGER_HPP
#define CONFIGURATION_MANAGER_HPP

#include <fstream>
#include <iostream>
#include <unordered_map>
#include <unordered_set>

#include <boost/program_options.hpp>

#include <log4cplus/configurator.h>
#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>
#include <csignal>
#include <fstream>
#include <iostream>

boost::program_options::variables_map initialize_config(int argc, char** argv);

namespace com {
namespace vmware {
namespace concord {

// Exception type for any exceptions thrown by the configuration system that do
// not fit into standard exception types such as std::invalid_argument. More
// specific types of configuration-specific exceptions should be subtypes of
// this exception class and, generally configuration system implementations
// should strongly prefer throwing exceptions of those subtypes of this class
// over constructing exceptions of this class directly.
class ConfigurationException : public std::exception {
 public:
  explicit ConfigurationException(const std::string& what) : message(what) {}
  virtual const char* what() const noexcept override { return message.c_str(); }

 private:
  std::string message;
};

// Exception type possibly thrown by iterators over configuration structures in
// the event an iterator has become invalid. The primary cause of such an
// iterator invalidation is modification of the underlying object that the
// iterator does not handle.
class InvalidIteratorException : public ConfigurationException {
 public:
  explicit InvalidIteratorException(const std::string& what)
      : ConfigurationException(what), message(what) {}
  virtual const char* what() const noexcept override { return message.c_str(); }

 private:
  std::string message;
};

// Exception type thrown by ConcordConfiguration on attempts to make definitions
// within the configuration it manages that conflict with existing definitions,
// for example, declaration of a configuration parameter with a name that is
// already in use.
class ConfigurationRedefinitionException : public ConfigurationException {
 public:
  explicit ConfigurationRedefinitionException(const std::string& what)
      : ConfigurationException(what), message(what) {}
  virtual const char* what() const noexcept override { return message.c_str(); }

 private:
  std::string message;
};

// Exception type thrown by ConcordConfiguration when a request requires or
// references something that does not exist or cannot be found. For example,
// this includes attempts to read the value for a configuration parameter that
// does not currently have a value loaded.
class ConfigurationResourceNotFoundException : public ConfigurationException {
 public:
  explicit ConfigurationResourceNotFoundException(const std::string& what)
      : ConfigurationException(what), message(what) {}
  virtual const char* what() const noexcept override { return message.c_str(); }

 private:
  std::string message;
};

// struct for referring to specific parameters or scopes within a
// ConcordConfiguration. ConcordConfiguration provides for the definition of
// instantiable scopes that may contain configuration parameters or other scopes
// (for example, these scopes can be used to handle per-node or per-replica
// parameters). This struct enables constructing references to parameters (or
// scopes) given that they may be contained in (possibly nested) scopes, and
// that the scopes in which they are contained may be either instances of scopes
// or the templates from which those instances are created. Several utility
// functions for workign with ConfigurationPaths are also defined for this
// struct.
// For example, a ConfigurationPath could refer to the parameter "port" for the
// 2nd replica. Such a path would be constructed from scratch like this:
//
// ConfigurationPath path("replica", (size_t)2);
// path.subpath.reset(new ConfigurationPath("port", false));
//
// This struct provides no synchronization or guarantees of synchronization.
// Behavior of this class is currently undefined in the event the subpath
// pointers are used to create a cycle.
struct ConfigurationPath {
  // Whether this segment of the path is a scope (true indicates a scope, false
  // indicates a parameter).
  bool isScope;

  // If this segment of the path is a scope, whether it is the temmplate for the
  // scope or an instance of the scope (true indicates an intance, false
  // indicates the template; note useInstance should be ignored if (isScope ==
  // false).
  bool useInstance;

  // Name for this segment of the configuration path, either the name of a scope
  // or of a parameter depending on isScope.
  std::string name;

  // If this segment of the path selects a scope instance, index selects which
  // one (note scope indexes are 0-indexed). This field should not be given any
  // meaning unless (isScope && useInstance).
  size_t index;

  // If this segment of the path is not the final segment, subpath points to the
  // rest of the path. If this is the final segment, subpath is null. Note
  // subpath should be ignored if (isScope == false). Note this
  // ConfigurationPath object is considered to own the one pointed to by
  // subpath; that one will be deleted by this path's destructor.
  std::unique_ptr<ConfigurationPath> subpath;

  // Default constructor for ConfigurationPath. This constructor is defined
  // primarily to allow declaring a ConfigurationPath by value without
  // initializing it. Although this constructor will make a path to a variable
  // with the empty string as its name, client code should not rely on the
  // behavior of this constructor.
  ConfigurationPath();

  // Constructs a ConfigurationPath with the given values for name and isScope.
  // If isScope is true, subpath will be initialized to null.
  ConfigurationPath(const std::string& name, bool isScope = false);

  // Constructs a ConfigurationPath to a scope instance with the given name and
  // index (this constructor sets isScope and useInstance to true). subpath will
  // be initialized to null.
  ConfigurationPath(const std::string& name, size_t index);

  // Copy constructor for ConfigurationPath. Note that, since each non-terminal
  // path step is considered to own the next one, this constructor will
  // recursively copy the entire path; subpath will not be alliased between
  // other and the newly constructed instance.
  ConfigurationPath(const ConfigurationPath& other);

  // Destructor for ConcigurationPath. Note subpath will be freed (and this
  // freeing will be recursive if the subpath has multiple segments).
  ~ConfigurationPath();

  // Copy assignment operator for ConfigurationPaths. this will be equal to
  // other after this operator returns. Note any subpath the caller currently
  // has will be freed, and other's subpath (if any) will be recursively copied.
  ConfigurationPath& operator=(const ConfigurationPath& other);

  // Equality and inequality operators for ConfigurationPaths. If isScope is
  // true and subpath exists, the equality check is recursive; it will not
  // merely be a test for alliasing of subpath. Note useInstance will be ignored
  // if isScope is not true, index will be ignored if isScope and useInstance
  // are not both true, and subpath will be ignored if isScope is not true.
  bool operator==(const ConfigurationPath& other) const;
  bool operator!=(const ConfigurationPath& other) const;

  // Generates a string representation of this configuration path. The primary
  // intended use case of this function is in generation of human-readable
  // output such as error messages. The format is:
  //   name
  //     for parameters or scope templates
  //   name[index]
  //     for scope instances
  // If subpath is non-null,
  //   /<STRING REPRESENTATION OF THE SUBPATH>
  // will be appended. For example, the string representation of a path to the
  // "port" parameter for the second replica would be: replica[2]/port
  std::string toString() const;

  // Returns true if and only if (1) this (the caller) is ultimately a path to a
  // scope (i.e. isScope is true for the terminal segment of this
  // ConfigurationPath) and (2) other is within that scope. Note this is
  // effectively equivalent to this being a scope and a prefix of other.
  bool contains(const ConfigurationPath& other) const;

  // Concatenate other to the end of this ConfigurationPath.
  // If this ConfigurationPath ultimately refers to a parameter, this operation
  // is illegal and will throw std::invalid_argument.
  ConfigurationPath concatenate(const ConfigurationPath& other) const;

  // Get the "leaf" of this ConfigurationPath, that is, its terminal segment.
  ConfigurationPath getLeaf() const;

  // Get a ConfigurationPath that is equivalent to this path with the "leaf"
  // (that is, its terminal segment) truncated. This will return an unaltered
  // copy of this path if trimLeaf is called directly on a single-segment path.
  ConfigurationPath trimLeaf() const;
};

}  // namespace concord
}  // namespace vmware
}  // namespace com

namespace std {

// Specialization of std::hash<class T> for ConfigurationPath to facilitate
// storing ConfigurationPaths in hash-based data structures.
template <>
struct hash<com::vmware::concord::ConfigurationPath> {
  size_t operator()(const com::vmware::concord::ConfigurationPath& path) const;
};
}  // namespace std

namespace com {
namespace vmware {
namespace concord {

// The ConcordConfiguration class provides a framework for defining the
// structure and contents of configuration and handles storing and managing that
// configuration. ConcordConfiguration assumes the fundamental unit of
// configuration is a configuration parameter with value representable as a
// string. In addition to allowing definition of configuration parameters,
// ConcordConfiguration also permits the definition of instantiable "scopes"
// which may contain parameters or other scopes (i.e. scopes can be nested,
// theoretically to an arbitrary depth). These scopes can be used to model
// things such as per-replica or per-node parameters. Declaring a scope creates
// a template for its instances; at a later time, the scope can be instantiated
// to create a copy of it for each instance. Scope instances and templates are
// themselves fully-featured ConcordConfiguration objects. One goal in designing
// this class is to enable a common definition of the Concord configuration to
// be shared between each agent involved in the configuration's lifecycle. At
// the time of this writing, that includes a configuration generation utility or
// service and the node consuming the configuration. Other than definition of
// parameters and instantiable scopes, this class currently supports the
// following features:
// - Iteration: The entire contents of a ConcordConfiguration, including all
// parameters, all scopes, or both, can be iterated through as a series of
// ConfigurationPaths.
// - Parameter Tagging: Any parameter can be tagged with any set of arbitrary
// string tags. This is intended to facilitate categorizing parameters.
// - Parameter Validation: Any number of validation functions may be associated
// with any parameter. The validators will be enforced when trying to load a
// parameter, causing a failure if any validator explicitly rejects the
// parameter. Validators may also be re-run at an arbitrary time for any
// parameter to enable support for validators that are sensitive to other
// parameters.
// - Default Values: A default value can be given for any parameter.
// - Parameter Generation: A generation function may be defined for any
// parameter that is automatically generated rather than taken as input.
//
// This class currently provides no synchronization or guarantees of
// synchronization and has not been designed with multiple threads writing to
// configuration concurrently in mind.
class ConcordConfiguration {
 public:
  // Return type for the three function type definitions immediately following.
  enum class ParameterStatus { VALID, INSUFFICIENT_INFORMATION, INVALID };

  // Type for parameter validator functions.
  // A validator should return VALID if it finds the parameter's value valid,
  // INVALID if it confirms the value is definitevely invalid, and
  // INSUFFICIENT_INFORMATION if it lacks the information to definitively
  // confirm the parameter's validity. When a ConcordConfiguration calls a
  // parameter validator, it gives the following arguments:
  //   - value: The value to assess the validity of.
  //   - config: The ConcordConfiguration calling this function.
  //   - path: The path to the parameter within this configuration for which
  //   this value is to be validated.
  //   - failureMessage: If the validator does not find value to be valid, it
  //   may choose to write back a message to failureMessage reporting the reason
  //   it did not find value to be valid.
  //   - state: An arbitrary pointer provided at the time this validator was
  //   added to the configuration. It is anticipated this will be used if this
  //   validator requires additional state.
  typedef ParameterStatus (*Validator)(const std::string& value,
                                       const ConcordConfiguration& config,
                                       const ConfigurationPath& path,
                                       std::string* failureMessage,
                                       void* state);

  // Type for parameter generation functions.
  // A parameter generator should return VALID if it was able to successfully
  // generate a valid value, INSUFFICIENT_INFORMATION if it information needed
  // by the generator (such as other parameters) is missing, and INVALID if it
  // is not possible to generate a valid value under the current state or the
  // generator otherwise fails. When a ConcordConfiguration calls a parameter
  // generator, it gives the following arguments:
  //   - config: The ConcordConfiguration calling this function.
  //   - path: Path to the parameter for which a value should be generated.
  //   - output: String pointer to output the generated value to if generation
  //   is successful (Note any value written to this pointer will be ignored by
  //   the ConcordConfiguration if the generator does not return VALID).
  //   - state: An arbitrary pointer provided at the time this generator was
  //   added to the configuration. It is anticipated this will be used if the
  //   generator requires additional state.
  typedef ParameterStatus (*Generator)(const ConcordConfiguration& config,
                                       const ConfigurationPath& path,
                                       std::string* output, void* state);

  // Type for a scope sizer function. A scope sizer function is called by the
  // ConcordConfiguration when instantiation of a scope is requested to
  // determine the appropriate size for it under the current configuration. A
  // scope sizer should return VALID if it was able to successfully determine
  // the correct size for the requested scope, INSUFFICIENT_INFORMATION if needs
  // information that is currently not available, and INVALID if it is not
  // possible to get a valid size for this scope under the current state or if
  // the sizer otherwise fails. When a ConcordConfiguration calls a scope sizer,
  // it gives the following arguments:
  //   - config: The ConcordConfiguration calling this function.
  //   - path: Path to the scope for which a size is being requested.
  //   - output: size_t pointer to which to output the appropriate size for
  //   the requested scope (Note the ConcordConfiguration will ignore any value
  //   written here if the sizer does not return VALID).
  //   - state: An arbitrary pointer provided at the time the scope being sized
  //   was declared. It is anticipated this pointer will be used if the scope
  //   sizer requires any additional state.
  typedef ParameterStatus (*ScopeSizer)(const ConcordConfiguration& config,
                                        const ConfigurationPath& path,
                                        size_t* output, void* state);

 private:
  struct ConfigurationScope {
    std::unique_ptr<ConcordConfiguration> instanceTemplate;

    bool instantiated;
    std::vector<ConcordConfiguration> instances;
    std::string description;
    ScopeSizer size;
    void* sizerState;

    ConfigurationScope() {}
    ConfigurationScope(const ConfigurationScope& original);
    ConfigurationScope& operator=(const ConfigurationScope& original);
  };
  struct ConfigurationParameter {
    std::string description;
    bool hasDefaultValue;
    std::string defaultValue;
    bool initialized;
    std::string value;
    std::unordered_set<std::string> tags;
    Validator validator;
    void* validatorState;
    Generator generator;
    void* generatorState;

    ConfigurationParameter() {}
    ConfigurationParameter(const ConfigurationParameter& original);
    ConfigurationParameter& operator=(const ConfigurationParameter& original);
  };

  std::string configurationState;

  // Both these pointers point to null if this ConcordConfiguration is the root
  // scope of its configuration.
  ConcordConfiguration* parentScope;
  std::unique_ptr<ConfigurationPath> scopePath;

  std::unordered_map<std::string, ConfigurationScope> scopes;
  std::unordered_map<std::string, ConfigurationParameter> parameters;

  // Private helper functions.
  void invalidateIterators();
  ConfigurationPath* getCompletePath(const ConfigurationPath& localPath) const;
  std::string printCompletePath(const ConfigurationPath& localPath) const;
  std::string printCompletePath(const std::string& localParameter) const;
  void updateSubscopePaths();
  ConfigurationParameter& getParameter(const std::string& parameter,
                                       const std::string& failureMessage);
  const ConfigurationParameter& getParameter(
      const std::string& parameter, const std::string& failureMessage) const;

 public:
  // Complete definition of the iterator type for iterating through a
  // ConcordConfiguration returned by begin and end.
  class ConfigurationIterator : public std::iterator<std::forward_iterator_tag,
                                                     const ConfigurationPath> {
   private:
    bool recursive;
    bool scopes;
    bool parameters;
    bool instances;
    bool templates;

    ConcordConfiguration* config;

    // Value returned by this iterator; the iterator stores this value itself
    // because it returns values by reference rather than by value.
    ConfigurationPath retVal;

    // State of iteration.
    std::unordered_map<std::string, ConfigurationScope>::iterator currentScope;
    std::unordered_map<std::string, ConfigurationScope>::iterator endScopes;
    bool usingInstance;
    size_t instance;
    std::unique_ptr<ConfigurationIterator> currentScopeContents;
    std::unique_ptr<ConfigurationIterator> endCurrentScope;
    std::unordered_map<std::string, ConfigurationParameter>::iterator
        currentParam;
    std::unordered_map<std::string, ConfigurationParameter>::iterator endParams;
    bool invalid;

    void updateRetVal();

   public:
    ConfigurationIterator();
    ConfigurationIterator(ConcordConfiguration& configuration,
                          bool recursive = true, bool scopes = true,
                          bool parameters = true, bool instances = true,
                          bool templates = true, bool end = false);
    ConfigurationIterator(const ConfigurationIterator& original);
    ~ConfigurationIterator();

    ConfigurationIterator& operator=(const ConfigurationIterator& original);
    bool operator==(const ConfigurationIterator& other) const;
    bool operator!=(const ConfigurationIterator& other) const;
    const ConfigurationPath& operator*() const;
    ConfigurationIterator& operator++();
    ConfigurationIterator operator++(int);

    void invalidate();
  };

 private:
  // Iterators that need to be invalidated in the event this
  // ConcordConfiguration is modified.
  std::unordered_set<ConfigurationIterator*> iterators;

  void registerIterator(ConfigurationIterator* iterator);
  void deregisterIterator(ConfigurationIterator* iterator);

 public:
  // Type for iterators over this ConcordConfiguration returned by the begin and
  // end functions below. These iterators return a series of const
  // ConfigurationPath references pointing to each of the objects (scopes or
  // parameters) in the configuration that the iterator iterates through.
  // Iterators are forward iterators and have all behvior that is standard of
  // such iterators in C++. This includes:
  //   - Support for copy construction and copy assignment.
  //   - Support for operators == and != to check if two iterators are currently
  //   at the same position.
  //   - Support for operator * to get the value at the iterator's current
  //   position.
  //   - Support for prefix and postfix operator ++ to advance the iterator.
  // Note ConcordConfiguration::Iterators currently do not guarantee that the
  // ConfigurationPath references they return will still refer to the same
  // ConfigurationPath as they returned once the iterator has been advanced past
  // the point where the reference was obtained; code using
  // ConcordConfiguration::Iterators should make its own copy of the value
  // stored by the reference the iterator returns if it needs the value past
  // advancing the iterator.
  typedef ConfigurationIterator Iterator;

  ConcordConfiguration();
  ConcordConfiguration(const ConcordConfiguration& original);
  ~ConcordConfiguration();
  ConcordConfiguration& operator=(const ConcordConfiguration& original);

  // Removes all parameter definitions and scope definitions for this
  // ConcordConfiguration and resets its "configuration state" to the empty
  // string.
  void clear();

  // Sets the "configuration state" for this configuration to the given string.
  // ConcordConfiguration itself does not use its own "configuration state", but
  // it can be retrieved and used at any time by client code with
  // getConfigurationState.
  void setConfigurationState(const std::string& state);

  // Gets the most recent value passed to setConfigurationState. Returns an
  // empty string if setConfigurationState has never been called for this
  // ConcordConfiguration or if clear has been called more recently than
  // setConfigurationState.
  std::string getConfigurationState() const;

  // Adds a new instantiable scope to this configuration. The scope begins empty
  // and uninstantiated.
  // Arguments:
  //   - scope:  name for the new scope being declared. The empty string is not
  //   a valid scope name, and an std::invalid_argument will be thrown if scope
  //   is the empty string. A ConfigurationRedefinitionException will be thrown
  //   if a scope with this name already exists or if the name is already in use
  //   by a parameter.
  //   - description: a description for this scope.
  //   - size: pointer to a scope sizer function to be used to get the
  //   appropriate size for this scope when it is instantiated. An
  //   std::invalid_argument will be thrown if a nullptr is given.
  //   - sizerState: an arbitrary pointer to be passed to the scope sizer
  //   whenever it is called. It is anticipated this pointer will be used if the
  //   sizer requires any additional state.
  void declareScope(const std::string& scope, const std::string& description,
                    ScopeSizer size, void* sizerState);

  // Gets the description the scope named by the parameter was constructed with.
  // Throws a ConfigurationResourceNotFoundException if the requested scope does
  // not exist.
  std::string getScopeDescription(const std::string& scope) const;

  // Instantiates the scope named by the parameter. The sizer function provided
  // when the scope was declared will be called to determine its size.
  // Instantiation will not be attempted unless the sizer function returns
  // VALID. If the scope is instantiated, a new copy of current scope template
  // will be made for each instance. If this scope has was already instantiated,
  // any existing instances will be deleted when this function instantiates it
  // again. Returns the ParameterStatus returned by the scope's sizer function.
  // Throws a ConfigurationResourceNotFoundException if no scope exists with the
  // given name.
  ParameterStatus instantiateScope(const std::string& scope);

  // Gets a reference to the scope template for the scope with the name
  // specified by the parameter. Modifying the ConcordConfiguration returned by
  // this function will also modify the scope it represents within the
  // ConcordConfiguration used to call this function. Throws a
  // ConfigurationResourceNotFoundException if no scope exists with the given
  // name.
  ConcordConfiguration& subscope(const std::string& scope);
  const ConcordConfiguration& subscope(const std::string& scope) const;

  // Gets a reference to a scope instance from the scope named by the scope
  // (string) parameter with index specified by the index (size_t) parameter
  // (note scope instances are 0-indexed). Modifying the ConcordConfiguration
  // returned by this function will also modify the instance within the
  // ConcordConfiguration used to call this function. Throws a
  // ConfigurationResourceNotFoundEsception if no scope exists with the
  // requested name, if the reuested scope has not been instantiated, or if the
  // requested index is out of ragne.
  ConcordConfiguration& subscope(const std::string& scope, size_t index);
  const ConcordConfiguration& subscope(const std::string& scope,
                                       size_t index) const;

  // Gets a reference to a scope template or scope instance within this
  // ConcordConfiguration (or recursively within any scope within this
  // ConcordConfiguration, where "within" is fully transitive) specified by the
  // given path. Throws a ConfigurationResourceNotFoundException if no scope
  // exists at the specified path within this ConcordConfiguration.
  ConcordConfiguration& subscope(const ConfigurationPath& path);
  const ConcordConfiguration& subscope(const ConfigurationPath& path) const;

  // Returns true if this configuration contains a scope with the given name and
  // false otherwise.
  bool containsScope(const std::string& name) const;

  // Returns true if this configuration contains a scope template or scope
  // instance at the given path and false otherwise.
  bool containsScope(const ConfigurationPath& path) const;

  // Returns true if this configuration contains a scope with the given name and
  // that scope has been instantiated and false otherwise. Note a scope may be
  // considered instantiated even if it has 0 instances in the event the scope
  // sizer returned 0 at instantiation time.
  bool scopeIsInstantiated(const std::string& name) const;

  // Returns the size (number of instances) that the scope named by the
  // parameter is currently instantiated to. Throws a
  // ConfigurationResourceNotFoundException if the requested scope does not
  // exist or has not been instantiated.
  size_t scopeSize(const std::string& scope) const;

  // Declares a new parameter in this configuration with the given name and
  // description and with no default value. Throws a
  // ConfigurationRedefinitionException if a parameter already exists with the
  // given name or if that name is already used by a scope. Furthermore, the
  // empty string is disallowed as a parameter name and an std::invalid_argument
  // will be thrown if it is given as such.
  void declareParameter(const std::string& name,
                        const std::string& description);

  // Declares a new parameter in this configuration with the given name,
  // description, and default value. Throws a ConfigurationRedifinitionException
  // if a parameter already exists with the given name or if that name is
  // already used by a scope. Furthermore, the empty string is disallowed as a
  // parameter name and an std::invalid_argument will be thrown if it is given
  // as such.
  void declareParameter(const std::string& name, const std::string& description,
                        const std::string& defaultValue);

  // Adds any tags in the given vector that the parameter in this configuration
  // with the given name is not already tagged with to that parameter. Throws a
  // ConfigurationResourceNotFoundException if no parameter exists  in this
  // configuration with the given name.
  void tagParameter(const std::string& name,
                    const std::vector<std::string>& tags);

  // Returns true if the parameter in this configuration with the given name
  // exists and has been tagged with the given tag, and false if it exists but
  // has not been tagged with this tag. Throws a
  // ConfigurationResourceNotFoundException if no parameter exists with this
  // name.
  bool isTagged(const std::string& name, const std::string& tag) const;

  // Gets the description that the parameter with the given name was declared
  // with. Throws a ConfigurationResourceNotFoundException if no parameter
  // exists in this ConcordConfiguration with the given name.
  std::string getDescription(const std::string& name) const;

  // Adds a validation function to a configuration parameter in this
  // ConcordConfiguration. Note that we allow only one validation function per
  // parameter. Calling this function for a parameter that already has a
  // validator will cause the existing validator to be replaced. Arguments:
  //   - name: The name of the parameter to add the validator to. Throws a
  //   ConfigurationResourceNotFoundException if no parameter exists in this
  //   ConcordConfiguration with this name.
  //   - validator: Function pointer to the validation function to add to this
  //   parameter. Throws an std::invalid_argument if this is a null pointer.
  //   - validatorState: Arbitrary pointer to pass to the validator each time it
  //   is called; it is expected this pointer will be used if the validator
  //   requires any additional state.
  void addValidator(const std::string& name, Validator validator,
                    void* validatorState);

  // Adds a generation function to a configuration parameter in this
  // ConcordConfiguration. Note that a parameter can only have a single
  // generator function; calling this function for a parameter that already has
  // a generator function will replace the existing generator if this function
  // is successful. Arguments:
  //   - name: The name of the parameter to add the generator to. Throws a
  //   ConfigurationResourceNotFoundException if no parameter exists in this
  //   ConcordConfiguration with this name.
  //   - generator: Function pointer to the generator function to be added. An
  //   std::invalid_argument will be thrown if a nullptr is given for this
  //   parameter.
  //   - generatorState: An arbitrary pointer to pass to the generator each time
  //   it is called; it is expected this pointer will be used if the generator
  //   requires additional state.
  void addGenerator(const std::string& name, Generator generator,
                    void* generatorState);

  // Returns true if this ConcordConfiguration contains a parameter with the
  // given name and false otherwise.
  bool contains(const std::string& name) const;

  // Returns true if there is a parameter within this ConcordConfiguraiton at
  // the given path and false otherwise. Note this function handles checking the
  // appropriate subscope for the parameter if the path has multiple segments.
  bool contains(const ConfigurationPath& path) const;

  // Returns true if this ConcordConfiguration contains a parameter with the
  // given name and that parameter has a value loaded and false otherwise.
  bool hasValue(const std::string& name) const;

  // Returns true if there is a parameter within this ConcordConfiguration at
  // the given path and that parameter has a value loaded and false otherwise.
  // Note this function handles checking the appropriate subscope for the
  // parameter if the path has multiple segments.
  bool hasValue(const ConfigurationPath& path) const;

  // Gets the value currently loaded to the parameter in this
  // ConcordConfiguration with the given name. Throws a
  // ConfigurationResourceNotFoundException if this ConcordConfiguration does
  // not contain a parameter with the given name or if the requested parameter
  // does not have a value loaded.
  std::string getValue(const std::string& name) const;

  // Gets the value currently loaded to the parameter in this
  // ConcordConfiguration at the given path. Throws a
  // ConfigurationResourceNotFoundException if there is no parameter at the
  // given path or if the requested parameter does not have a value loaded.
  std::string getValue(const ConfigurationPath& path) const;

  // Loads a value to a parameter in this ConcordConfiguration. This function
  // will return without loading the requested value if the parameter's
  // validator (if any) returns an INVALID status for the requested value.
  // Arguments:
  //   - name: The name of the parameter to which to load the value. Throws a
  //   ConfigurationResourceNotFoundException if no parameter exists with this
  //   name.
  //   - value: Value to attempt to load to this parameter.
  //   - failureMessage: If a non-null pointer is given for failureMessage, the
  //   named parameter has a validator, and that validator returns a status
  //   other than valid for value, then any failure message the validator
  //   provides will be written back to failureMessage.
  //   - overwrite: If the requested parameter already has a value loaded,
  //   loadValue will not overwrite it unless true is given for this parameter.
  //   - prevValue: If loadValue successfully overwrites an existing value and
  //   prevValue is non-null, the existing value that was overwritten will be
  //   written to prevValue.
  // Returns: the result of running the validator (if any) for this parameter
  // for the requested value. If the parameter has no validator, VALID will be
  // returned.
  ParameterStatus loadValue(const std::string& name, const std::string& value,
                            std::string* failureMessage = nullptr,
                            bool overwrite = true,
                            std::string* prevValue = nullptr);

  // Erases the value currently loaded (if any) for a parameter with the given
  // name in this ConcordConfiguration. The parameter will have no value loaded
  // after this function runs. If a non-null pointer is provided for prevValue
  // and this function does erase a value, then the erased value will be written
  // back to prevValue. Throws a ConfigurationResourceNotFoundException if no
  // parameter exists with the given name.
  void eraseValue(const std::string& name, std::string* prevValue = nullptr);

  // Erases any and all values currently loaded in this ConcordConfiguration. No
  // parameter in this configuraiton (including in any subscope of this
  // configuration) will have a value loaded after this function runs.
  void eraseAllValues();

  // Loads the default value for a given parameter. The value will not be loaded
  // if the validator for the parameter (if any) returns an INVALID status for
  // the default value. Arguments:
  //   - name: The name of the parameter for which to load the default value.
  //   Throws a ConfigurationResourceNotFoundException if no parameter exists
  //   with this name, or if the named parameter has no default value.
  //   - failureMessage: If a non-null pointer is given for failureMessage, the
  //   named parameter has a validator, and that validator returns a status
  //   other than valid for the default value of the named parameter, then any
  //   failure message the validator provides will be written back to
  //   failureMessage.
  //   - overwrite: If the selected parameter already has a value loaded, that
  //   value will only be overwritten if true is given for the overwrite
  //   parameter.
  //   - prevValue: If a non-null pointer is given for prevValue and loadDefault
  //   does successfully overwrite an existing value, the existing value will be
  //   written back to prevValue.
  // Returns: the result of running the validator (if any) for the default value
  // for this parameter. If the parameter has no validators, VALID will be
  // returned.
  ParameterStatus loadDefault(const std::string& name,
                              std::string* failureMessage = nullptr,
                              bool overwrite = false,
                              std::string* prevValue = nullptr);

  // Load the default values for all parameters within this
  // ConcordConfiguration, including in any subscopes, that have default values.
  // Any default value for which a validator of its parameter returns INVALID
  // will not be loaded. Existing values will not be overwritten unless true is
  // given for the overwrite parameter. Scope templates will be ignored unless
  // true is given for the includeTemplates parameter. Returns: the result of
  // running the validator(s) for every default parameter considered (i.e.
  // excluding those in scope templates unless includeTemplates is specified).
  // In aggregating the results of running these validator(s), the "least valid"
  // result obtained will be returned. That is, VALID will only be returned if
  // every validator run returns VALID, and INVALID will be returned over
  // INSUFFICIENT_INFORMATION if any validator returns INVALID.
  ParameterStatus loadAllDefaults(bool overwrite = false,
                                  bool includeTemplates = false);

  // Runs the validator (if any) for the currently loaded value of the named
  // parameter and return its result. Throws a
  // ConfigurationResourceNotFoundException if no parameter exists with the
  // given name. If the requested parameter exists but has no value loaded,
  // validate will return INSUFFICIENT_INFORMATION. If the named parameter
  // exists, has a value loaded, and has no validators, VALID will be returned.
  // If a non-null pointer is given for failureMessage, the named parameter has
  // a validator, and the result is not VALID, then any failure message provided
  // by the validator will be written back to failureMessage.
  ParameterStatus validate(const std::string& name,
                           std::string* failureMessage = nullptr) const;

  // Runs the validator(s) for the currently loaded value(s) in all parameter(s)
  // in this ConcordConfiguration, including in any subscopes, and returns their
  // result. If this results in no validators being run, then VALID will be
  // returned. In aggregating the results of multiple validators, the "least
  // valid" result obtained will be returned. That is, VALID will only be
  // returned if every validator run returns VALID, and INVALID will be returned
  // over INSUFFICIENT_INFORMATION if any validator returns INVALID. If false is
  // given for the includeTemplates parameter, then scope templates will be
  // skipped. If true is given for ignoreUninitializedParameters, then any
  // parameter with no value loaded will be skipped entirely. Otherwise, the
  // result of validation for any uninitialized parameter will be considered to
  // be INSUFFICIENT_INFORMATION.
  ParameterStatus validateAll(bool ignoreUninitializedParameters = true,
                              bool inclueTemplates = false);

  // Runs a parameter's generation function and loads the result. generate will
  // not attempt to load the generated value unless the generation function
  // returns VALID. Generate also will not load any generated value for which
  // the validator of the selected parameter returns INVALID. Parameters:
  //   - name: Name of the parameter for which to generate a value. A
  //   ConfigurationResourceNotFoundException will be thrown if no parameter
  //   exists with the given name or if the named parameter has no generation
  //   funciton.
  //   - failureMessage: If a non-null pointer is given for failure message, the
  //   named parameter has both a generator and validator, and the geneerator
  //   returns the status VALID but the validator returns a non-VALID status for
  //   the generated value, then any failure message provided by the validator
  //   will be written back to failureMessage.
  //   - overwrite: If the requested parameter is already initialized, the
  //   existing value will only be overwritten if true is given for overwrite.
  //   - prevValue: If generate does successfully overwrite an existing value
  //   and a non-null pointer was given for prevValue, the existing value will
  //   be written back to prevValue.
  // Returns:
  // If the parameter's generation function returns a value other than VALID,
  // then that value will be returned; otherwise, generate returns the result of
  // running the parameter's validator (if any) on its generated values. If the
  // generator reutnred VALID but the parameter has no validators, VALID will be
  // returned.
  ParameterStatus generate(const std::string& name,
                           std::string* failureMessage = nullptr,
                           bool overwrite = false,
                           std::string* prevValue = nullptr);

  // Runs the generation functions for any and all parameters in this
  // ConcordConfiguration that have generation functions and loads the generated
  // values. A value produced by a generator will not be loaded unless the
  // generator returned VALID. Furthermore, a value will not be loaded if the
  // parameter it would be loaded to has a validator function and that validator
  // returns INVALID. generateAll will only overwrite existing values of
  // initialized parameters if true is given for overwrite. generateAll will
  // ignore scope templates if false is given for includeTemplates. Returns: The
  // "least valid" result from the generation of any parameter that is not
  // skipped (templates are skipped if false is given for includeTemplates, and
  // parameters without generation functions are always skipped). That is, VALID
  // will not be returned unless the result for every non-skipped parameter is
  // VALID, and INVALID will be returned over INSUFFICIENT_INFORMATION if
  // INVALID is obtained for any parameter. The result for each non-skipped
  // parameter individually in this aggregation process will be considered to be
  // the same as the result the generate function would return for generation of
  // just that parameter.
  ParameterStatus generateAll(bool overwrite = false,
                              bool includeTemplates = false);

  // ConcordConfiguration::begin and ConcordConfiguration::end, which both
  // should be declared immediately below these constant definitions; both
  // essentially require enough boolean parameters that having them all as
  // individual parameters in their function signatures would be somewhat
  // unsightly. To make calls to these functions more intuitive and legible, we
  // encode these five booleans in a bitmask. The following constants give the
  // bits used in this bitmasks and a number of complete bitmasks for common
  // selections of features. At the time of this comment's writing, there are
  // five distinct feature bits used in selecting the features of a
  // ConcordConfiguration::Iterator, specifically:
  // - kTraverseParameters: If this bit is set to 0, the iterator will not
  // return any paths to parameters.
  // - kTraverseScopes: If this bit is set to 0, the iterator will not return
  // any paths to scopes.
  // - kTraverseTemplates: If this bit is set to 0, the iterator will ignore
  // scope templates entirely.
  // - kTraverseInstances: If this bit is set to 0, the iterator will ignore
  // scope instances entirely.
  // - kTraverseRecursively: If this bit is set to 0, the iterator will not
  // recursively traverse subscopes of the ConcordConfiguration this iterator is
  // constructed for. That is, it may return parameters directly in this
  // configuration objectand single-step paths to scope templates and/or
  // instances that lie directly within this configuration object, but it will
  // not return any paths with more than one step which would require entering
  // subscopes.
  typedef uint8_t IteratorFeatureSelection;

  const static IteratorFeatureSelection kTraverseParameters = (0x01 << 0);
  const static IteratorFeatureSelection kTraverseScopes = (0x01 << 1);
  const static IteratorFeatureSelection kTraverseTemplates = (0x01 << 2);
  const static IteratorFeatureSelection kTraverseInstances = (0x01 << 3);
  const static IteratorFeatureSelection kTraverseRecursively = (0x01 << 4);

  const static IteratorFeatureSelection kIterateTopLevelParameters =
      kTraverseParameters;
  const static IteratorFeatureSelection kIterateTopLevelScopeTemplates =
      kTraverseScopes | kTraverseTemplates;
  const static IteratorFeatureSelection kIterateTopLevelScopeInstances =
      kTraverseScopes | kTraverseInstances;
  const static IteratorFeatureSelection kIterateAllTemplateParameters =
      kTraverseParameters | kTraverseTemplates | kTraverseRecursively;
  const static IteratorFeatureSelection kIterateAllInstanceParameters =
      kTraverseParameters | kTraverseInstances | kTraverseRecursively;
  const static IteratorFeatureSelection kIterateAllParameters =
      kTraverseParameters | kTraverseTemplates | kTraverseInstances |
      kTraverseRecursively;
  const static IteratorFeatureSelection kIterateAll =
      kTraverseParameters | kTraverseScopes | kTraverseTemplates |
      kTraverseInstances | kTraverseRecursively;

  // Obtains an iterator to the beginning of this ConcordConfiguration. The
  // iterator returns the selected contents of this configuration as a series of
  // const ConfigurationPath references.
  // begin accepts one parameter, features, which is a bitmask for feature
  // selection. Definition and documentation of constants for the bits in this
  // bitmask and some common feature selections combining them should be
  // immediately above this declaration. Note ConcordConfiguration::Iterators
  // currently do not guarantee that the ConfigurationPath references they
  // return will still refer to the same ConfigurationPath as they returned once
  // the iterator has been advanced past the point where the reference was
  // obtained; code using ConcordConfiguration::Iterators should make its own
  // copy of the value stored by the reference the iterator returns if it needs
  // the value past advancing the iterator.
  Iterator begin(
      IteratorFeatureSelection features = kIterateAllInstanceParameters);

  // Obtains an iterator to the end of this ConcordConfiguration. The iterator
  // will match the state of an iterator obtained with the begin function
  // immediately above, given the same feature selection bitmask, once that
  // iterator has exhausted all its contents.
  Iterator end(
      IteratorFeatureSelection features = kIterateAllInstanceParameters);
};

// A ParameterSelection object is intended to wrap a function that picks
// parameters from a configuration (by giving a boolean verdict on whether any
// particular parameter is contained in the set) and make it an iterable
// collection (with iteration returning ConfigurationPaths for each parameter
// that the function approves of). This class is intended to facilitate jobs
// that should be performed for some arbitrary subset of the configuration
// parameters, such as as I/O on certain parameters.
//
// This class currently does not provide any synchronization or guarantees of
// synchronization.
class ParameterSelection {
 public:
  // Type for parameter selector functions that this ParameterSelection wraps. a
  // ParameterSelector is to accept the following parameters:
  //   - config: The ConcordConfiguration to which the parameter assessed
  //   belongs.
  //   - path: Path to a parameter within the provided conviguration to be
  //   evaluated. The ParameterSelector should return true if the specified
  //   parameter is within this selection and false otherwise.
  //   - state: An arbitrary pointer provided at the time the ParameterSelection
  //   was constructed with this ParameterSelector. It is anticipated this
  //   pointer will be used if the ParameterSelector requires any additional
  //   state.
  typedef bool (*ParameterSelector)(const ConcordConfiguration& config,
                                    const ConfigurationPath& path, void* state);

 private:
  class ParameterSelectionIterator
      : public std::iterator<std::forward_iterator_tag,
                             const ConfigurationPath> {
   private:
    ParameterSelection* selection;
    ConcordConfiguration::Iterator unfilteredIterator;
    ConcordConfiguration::Iterator endUnfilteredIterator;
    bool invalid;

   public:
    ParameterSelectionIterator();
    ParameterSelectionIterator(ParameterSelection* selection, bool end = false);
    ParameterSelectionIterator(const ParameterSelectionIterator& original);
    virtual ~ParameterSelectionIterator();

    ParameterSelectionIterator& operator=(
        const ParameterSelectionIterator& original);
    bool operator==(const ParameterSelectionIterator& other) const;
    bool operator!=(const ParameterSelectionIterator& other) const;
    const ConfigurationPath& operator*() const;
    ParameterSelectionIterator& operator++();
    ParameterSelectionIterator operator++(int);

    void invalidate();
  };

  ConcordConfiguration* config;
  ParameterSelector selector;
  void* selectorState;

  // Set for keeping track of any iterators over this ParameterSelection so
  // they can be invalidated in the event of a concurrent modification of this
  // ParameterSelection. At the time of this writing, the only
  // ParameterSelection function which is actually considered to modify it and
  // invalidate its iterators is its destructor.
  std::unordered_set<ParameterSelectionIterator*> iterators;

  // Private helper functions.
  void registerIterator(ParameterSelectionIterator* iterator);
  void deregisterIterator(ParameterSelectionIterator* iterator);
  void invalidateIterators();

 public:
  // Type for iterators through this ParameterSelection returned by the begin
  // and end functions below. These iterators return a series of const
  // ConfigurationPath references pointing to each parameter in the selction.
  // Iterators are forward iterators and have all behvior that is standard of
  // such iterators in C++. This includes:
  //   - Support for copy construction and copy assignment.
  //   - Support for operators == and != to check if two iterators are currently
  //   at the same position.
  //   - Support for operator * to get the value at the iterator's current
  //   position.
  //   - Support for prefix and postfix operator ++ to advance the iterator.
  typedef ParameterSelectionIterator Iterator;

  // Primary constructor for ParameterSelections.
  // Parameters:
  //   - config: ConcordConfiguration that this ParameterSelection should select
  //   its parameters from.
  //   - selector: Function pointer of the ParameterSelector type defined above
  //   that decides whether given parameters are within this selection. This
  //   constructor will throw an std::invalid_argument if selector is a null
  //   pointer.
  //   - selectorState: Arbitrary pointer to be passed to selector each time it
  //   is called. It is anticipated this pointer will be used for any
  //   additioinal state the selector requires.
  ParameterSelection(ConcordConfiguration& config, ParameterSelector selector,
                     void* selectorState);

  ParameterSelection(const ParameterSelection& original);
  ~ParameterSelection();

  bool contains(const ConfigurationPath& parameter) const;
  ParameterSelection::Iterator begin();
  ParameterSelection::Iterator end();
};

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif /* end if CONFIGURATION_MANAGER_HPP defined */
