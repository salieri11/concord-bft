// Copyright 2019 VMware, all rights reserved
//
// Unit tests the configuration management classes defined in
// src/configuration_manager.hpp.

#include "configuration_manager.hpp"
#include "gtest/gtest.h"

namespace {

using namespace com::vmware::concord;

TEST(config_test, path_constructors) {
  ConfigurationPath path;

  ConfigurationPath parameterA("A");
  EXPECT_EQ(parameterA.name, "A") << "ConfigurationPath does not retain the"
                                     " name it is constructed with.";
  EXPECT_FALSE(parameterA.isScope) << "ConfigurationPath type does not default"
                                      " to parameter.";

  ConfigurationPath parameterB("B", false);
  EXPECT_FALSE(parameterB.isScope) << "ConfigurationPath constructed as a"
                                      " parameter is a scope.";

  ConfigurationPath scopeC("C", true);
  EXPECT_TRUE(scopeC.isScope) << "ConfigurationPath constructed as scope is a"
                                 " parameter.";
  EXPECT_FALSE(scopeC.useInstance) << "ConfigurationPath to a scope refers to"
                                      " an instance when no index is given.";

  ConfigurationPath instanceD("D", (size_t)12);
  EXPECT_TRUE(instanceD.isScope) << "ConfigurationPath constructed to a scope"
                                    " instance is a parameter";
  EXPECT_TRUE(instanceD.useInstance)
      << "ConfigurationPath constructed to a scope instance does not refer to "
         "an instance.";
  EXPECT_EQ(instanceD.index, 12) << "ConfigurationPath constructed to a scope"
                                    " instance does not retain its index.";

  ConfigurationPath instanceEmptyString("", (size_t)0);
  EXPECT_EQ(instanceEmptyString.name, "")
      << "ConfigurationPath given empty string as its name does not retain "
         "this name.";
  EXPECT_TRUE(instanceEmptyString.isScope && instanceEmptyString.useInstance)
      << "ConfigurationPath constructed to a scope instance does not refer to "
         "an instance.";
  EXPECT_EQ(instanceEmptyString.index, 0)
      << "ConfigurationPath constructed to a scope instance does not retain "
         "its index.";
}

TEST(config_test, path_copy) {
  ConfigurationPath path("A", true);
  path.subpath.reset(new ConfigurationPath("B", (size_t)12));
  path.subpath->subpath.reset(new ConfigurationPath("C", true));
  path.subpath->subpath->subpath.reset(new ConfigurationPath("", (size_t)0));
  path.subpath->subpath->subpath->subpath.reset(
      new ConfigurationPath("D", false));

  ConfigurationPath* pathRef = &path;
  while (pathRef) {
    ConfigurationPath copyConstructed(*pathRef);
    ConfigurationPath copyAssigned("E", true);
    copyAssigned.subpath.reset(new ConfigurationPath("F", (size_t)9));
    copyAssigned = *pathRef;

    EXPECT_EQ(*pathRef, copyConstructed)
        << "ConfigurationPath copy constructor yields instance unequal to the "
           "original.";
    EXPECT_EQ(*pathRef, copyAssigned)
        << "ConfigurationPath instance is unequal to the instance it copied "
           "after copy assignment.";
    EXPECT_EQ(copyConstructed, copyAssigned)
        << "ConfigurationPaths copy constructed and copy assigned from the "
           "same original source are unequal.";
    if (pathRef->subpath) {
      EXPECT_NE(pathRef->subpath, copyConstructed.subpath)
          << "ConfigurationPath copy constructor alliases subpath rather than"
             " copying it.";
      EXPECT_NE(pathRef->subpath, copyAssigned.subpath)
          << "ConfigurationPath copy assignment alliases subpath rather than "
             "copying it.";
    }

    pathRef = pathRef->subpath.get();
  }
}

TEST(config_test, path_equality) {
  ConfigurationPath pathA("A");
  ConfigurationPath alsoPathA("A");
  ConfigurationPath pathB("B");
  EXPECT_TRUE(pathA == alsoPathA) << "ConfigurationPath::operator == gives"
                                     " false for equal configuration paths.";
  EXPECT_FALSE(pathA == pathB) << "ConfigurationPath::operator == gives true"
                                  " for unequal configuration paths.";

  ConfigurationPath parameterC("C", true);
  ConfigurationPath scopeC("C", false);
  EXPECT_FALSE(parameterC == scopeC)
      << "ConfigurationPath::operator == fails to distinguish between a path "
         "to a parameter and a path to a scope.";

  ConfigurationPath scopeD("D", true);
  ConfigurationPath instanceD0("D", (size_t)0);
  ConfigurationPath instanceD12("D", (size_t)12);
  EXPECT_FALSE(scopeD == instanceD0)
      << "ConfigurationPath::operator == fails to distinguish betweeen a path "
         "to a scope and a path to a scope instance.";
  EXPECT_FALSE(instanceD0 == instanceD12)
      << "ConfigurationPath::operator == fails to distinguish between two "
         "different istances in the same scope.";

  ConfigurationPath pathE("E", true);
  pathE.subpath.reset(new ConfigurationPath("EE", true));
  pathE.subpath->subpath.reset(new ConfigurationPath("EEE", true));
  ConfigurationPath alsoPathE("E", true);
  alsoPathE.subpath.reset(new ConfigurationPath("EE", true));
  alsoPathE.subpath->subpath.reset(new ConfigurationPath("EEE", true));
  ConfigurationPath differentPathE("E", true);
  differentPathE.subpath.reset(new ConfigurationPath("EE", true));
  differentPathE.subpath->subpath.reset(new ConfigurationPath("EEF", true));
  EXPECT_TRUE(pathE == alsoPathE) << "ConfigurationPath::operator == fails to"
                                     " recognize equal subpaths.";
  EXPECT_FALSE(pathE == differentPathE)
      << "ConfigurationPath::operator == fails to detect differences in "
         "subpaths.";

  ConfigurationPath parameterF("F", false);
  parameterF.useInstance = false;
  parameterF.index = 0;
  parameterF.subpath.reset();
  ConfigurationPath alsoParameterF("F", false);
  alsoParameterF.useInstance = true;
  alsoParameterF.index = 12;
  alsoParameterF.subpath.reset(new ConfigurationPath("F", true));
  EXPECT_TRUE(parameterF == alsoParameterF)
      << "ConfigurationPath::operator == does not ignore scope-specific fields "
         "when comparing parameter paths.";

  ConfigurationPath scopeG("G", true);
  scopeG.index = 0;
  ConfigurationPath alsoScopeG("G", true);
  alsoScopeG.index = 12;
  EXPECT_TRUE(scopeG == alsoScopeG) << "ConfigurationPath::operator == does not"
                                       " ignore instance-specific fields when "
                                       "comparing scope template paths.";

  std::vector<std::pair<ConfigurationPath*, ConfigurationPath*>> pathsCompared =
      {{&pathA, &alsoPathA},        {&pathA, &pathB},
       {&parameterC, &scopeC},      {&scopeD, &instanceD0},
       {&instanceD0, &instanceD12}, {&pathE, &alsoPathE},
       {&pathE, &differentPathE},   {&parameterF, &alsoParameterF},
       {&scopeG, &alsoScopeG}};
  for (auto pair : pathsCompared) {
    EXPECT_EQ(!(*pair.first == *pair.second), (*pair.first != *pair.second))
        << "Behavior of operator == and operator != are inconsistent for"
           " ConfigurationPath.";
    if (*pair.first == *pair.second) {
      std::hash<ConfigurationPath> hasher;
      EXPECT_EQ(hasher(*pair.first), hasher(*pair.second))
          << "Behavior of ConfigurationPath hash function is inconsistent with "
             "its equality operator.";
    }
  }
}

TEST(config_test, path_to_string) {
  ConfigurationPath path("untitled", false);
  ASSERT_EQ(path.toString(), "untitled") << "ConfigurationPath::toString"
                                            " outputs incorrect path.";
  path.isScope = true;
  path.useInstance = false;
  ASSERT_EQ(path.toString(), "untitled") << "ConfigurationPath::toString"
                                            " incorrectly formats scope paths.";
  path.useInstance = true;
  path.index = 0;
  ASSERT_EQ(path.toString(), "untitled[0]")
      << "ConfigurationPath::toString incorrectly formats scope instance"
         " indexes.";
  path.useInstance = true;
  path.index = 144;
  ASSERT_EQ(path.toString(), "untitled[144]")
      << "ConfigurationPath::toString"
         " incorrectly formats scope instance indexes.";
  path.subpath.reset(new ConfigurationPath("temp", true));
  path.subpath->subpath.reset(new ConfigurationPath("A", (size_t)12));
  path.subpath->subpath->subpath.reset(new ConfigurationPath("B", false));
  ASSERT_EQ(path.toString(), "untitled[144]/temp/A[12]/B")
      << "ConfigurationPath::toString incorrectly formats multi-step path.";
  ConfigurationPath emptyPath("", false);
  ASSERT_EQ(emptyPath.toString(), "") << "ConfigurationPath::toString"
                                         " incorrectly handles empty paths.";
  emptyPath.subpath.reset(new ConfigurationPath("_tmp", true));
  ASSERT_EQ(emptyPath.toString(), "")
      << "ConfigurationPath::toString fails to"
         " ignore subpaths currently masked by parameter path.";
}

TEST(config_test, path_contains) {
  ConfigurationPath path("A", true);
  path.subpath.reset(new ConfigurationPath("B", (size_t)12));
  path.subpath->subpath.reset(new ConfigurationPath("C", true));
  ConfigurationPath containing(path);
  EXPECT_TRUE(containing.contains(path))
      << "ConfigurationPath::contains fails"
         " to recognize an equivalent path is contained.";

  path.subpath->subpath->isScope = false;
  EXPECT_FALSE(containing.contains(path))
      << "ConfigurationPath::contains fails"
         " to differentiate between paths to a scope and paths to a parameter.";

  path.subpath->subpath->isScope = true;
  containing.subpath->subpath.reset();
  EXPECT_TRUE(containing.contains(path))
      << "ConfigurationPath::contains fails"
         " to recognize a prefix of another path contains that path.";
  EXPECT_FALSE(path.contains(containing))
      << "ConfigurationPath::contains fails"
         " to recognize a path does not contain a strict prefix of itself.";

  containing.subpath->index = 13;
  EXPECT_FALSE(containing.contains(path))
      << "ConfiguraitonPath::contains fails"
         " to account for instance indexes.";

  containing.subpath->useInstance = false;
  path.subpath->useInstance = false;
  EXPECT_TRUE(containing.contains(path))
      << "ConfigurationPath::contains does"
         " not ignore index for non-instance scopes.";

  ConfigurationPath contained(*(path.subpath->subpath));
  EXPECT_FALSE(contained.contains(path))
      << "ConfigurationPath::contains"
         " considers a suffix of a path to contain that path.";
  EXPECT_FALSE(path.contains(contained))
      << "ConfigurationPath::contains"
         " considers a path to contain a suffix of itself.";
}

TEST(config_test, path_concatenate) {
  ConfigurationPath pathA("A", false);
  ConfigurationPath pathB("B", true);
  try {
    ConfigurationPath pathAB(pathA.concatenate(pathB));
    FAIL() << "ConfigurationPath::concatenate did not throw an exception when"
              " trying to concatenate to a parameter.";
  } catch (std::invalid_argument e) {
  }

  ConfigurationPath pathBA("B", true);
  pathBA.subpath.reset(new ConfigurationPath("A", false));
  EXPECT_EQ(pathBA, pathB.concatenate(pathA))
      << "ConfigurationPath::concatenate does not concatenate paths properly.";

  ConfigurationPath pathCD("C", true);
  pathCD.subpath.reset(new ConfigurationPath("D", (size_t)12));
  ConfigurationPath pathCDBA("C", true);
  pathCDBA.subpath.reset(new ConfigurationPath("D", (size_t)12));
  pathCDBA.subpath->subpath.reset(new ConfigurationPath("B", true));
  pathCDBA.subpath->subpath->subpath.reset(new ConfigurationPath("A", false));
  EXPECT_EQ(pathCDBA, pathCD.concatenate(pathBA))
      << "ConfigurationPath::concatenate does not handle concatenation of "
         "nested paths correctly.";

  pathBA.index = 12;
  pathBA.subpath.reset();
  EXPECT_EQ(pathB.concatenate(pathB), pathBA.concatenate(pathBA))
      << "ConfigurationPath::concatenate fails to ignore irrelevant"
         " ConfigurationPath fields.";
}

TEST(config_test, path_leaf_functions) {
  ConfigurationPath path("A", true);
  path.subpath.reset(new ConfigurationPath("B", (size_t)12));
  path.subpath->subpath.reset(new ConfigurationPath("C", false));
  EXPECT_EQ(path, (path.trimLeaf().concatenate(path.getLeaf())))
      << "ConfigurationPath is not equal to the concatenation of its non-leaf"
         " component and leaf.";

  ConfigurationPath leaf(*(path.subpath->subpath));
  ConfigurationPath* pathRef = &path;
  while (pathRef) {
    EXPECT_EQ(leaf, (pathRef->getLeaf())) << "ConfigurationPath::getLeaf fails"
                                             " to fetch the correct path.";
    pathRef = pathRef->subpath.get();
  }

  ConfigurationPath stem(path);
  stem.subpath->subpath.reset();
  EXPECT_EQ(stem, path.trimLeaf())
      << "ConfigurationPath::trimLeaf fails to"
         " correctly trim the leaf from a ConfigurationPath.";
  stem.subpath.reset();
  EXPECT_EQ(stem, path.trimLeaf().trimLeaf())
      << "ConfigurationPath::trimLeaf"
         " fails to correctly trim the leaf from a ConfigurationPath.";
  EXPECT_EQ(stem, path.trimLeaf().trimLeaf().trimLeaf())
      << "ConfigurationPath::trimLeaf fails to leave the root of the path "
         "intact if there is no leaf to trim.";

  path.subpath->isScope = false;
  EXPECT_NE(leaf, path.getLeaf()) << "ConfigurationPath::getLeaf fails to"
                                     " ignore subpath after a parameter.";
  EXPECT_EQ(stem, path.trimLeaf()) << "ConfigurationPath::trimLeaf fails to"
                                      " ignore subpath after a parameter.";
}

TEST(config_test, configuration_state) {
  ConcordConfiguration config;
  EXPECT_EQ(config.getConfigurationState(), "")
      << "Configuration state does not default to an empty string.";
  config.setConfigurationState("Configuration Generation");
  EXPECT_EQ(config.getConfigurationState(), "Configuration Generation")
      << "ConcordConfiguration fails to retain its state when set.";
}

static size_t mockScopeSizerResult = 0;
static ConcordConfiguration::ParameterStatus mockScopeSizer(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  if (state) {
    bool* wasCalled = static_cast<bool*>(state);
    *wasCalled = true;
  }
  *output = mockScopeSizerResult;
  return ConcordConfiguration::ParameterStatus::VALID;
}

static ConcordConfiguration::ParameterStatus mockScopeSizerThatNeverWorks(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    size_t* output, void* state) {
  *output = 144;
  return ConcordConfiguration::ParameterStatus::INVALID;
}

TEST(config_test, configuration_scope_creation) {
  ConcordConfiguration config;
  try {
    config.declareScope("inexistent_scope", "description", nullptr, &config);
    FAIL() << "ConcordConfiguration::declareScope failed to reject the creation"
              " of a scope with a null scope sizer function.";
  } catch (std::invalid_argument e) {
  }
  EXPECT_FALSE(config.containsScope("inexistent_scope"))
      << "ConcordConfiguration::declareScope still creates a scope if it was"
         " given invalid input.";

  bool mockScopeSizerCalled = false;
  config.declareScope("scope", "description", mockScopeSizer,
                      &mockScopeSizerCalled);
  EXPECT_TRUE(config.containsScope("scope"))
      << "ConcordConfiguration::declareScope fails to create the requested"
         " scope.";

  config.declareScope("otherScope", "description", mockScopeSizer, nullptr);
  EXPECT_TRUE(config.containsScope("otherScope"))
      << "ConcordConfiguration::declareScope fails to accept a scope with no"
         " state for the scope sizer function.";

  try {
    config.declareScope("otherScope", "description", mockScopeSizer,
                        &mockScopeSizerCalled);
    FAIL() << "ConcordConfiguration::declareScope fails to reject a new"
              " declaration for a scope that was already declared.";
  } catch (ConfigurationRedefinitionException e) {
  }

  try {
    config.instantiateScope("undefined_scope");
    FAIL() << "ConcordConfiguration::instantiateScope fails to reject a request"
              " to instantiate an inexistent scope.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  config.declareScope("new_scope", "New description.", mockScopeSizer,
                      &mockScopeSizerCalled);
  ConcordConfiguration& newScope = config.subscope("new_scope");
  newScope.declareParameter("parameter_a", "A parameter.");
  newScope.declareParameter("parameter_b", "A parameter.", "B");
  mockScopeSizerResult = 3;
  config.instantiateScope("new_scope");
  EXPECT_TRUE(mockScopeSizerCalled)
      << "ConcordConfiguration::instantiateScope"
         " failed to correctly call the appropriate scope sizer.";
  EXPECT_TRUE(config.scopeIsInstantiated("new_scope") &&
              (config.scopeSize("new_scope") == mockScopeSizerResult))
      << "ConcordConfiguration::instantiateScope failed to instantiate a scope"
         " to the correct size.";

  for (size_t i = 0; i < mockScopeSizerResult; ++i) {
    ConcordConfiguration& instance = config.subscope("new_scope", i);
    EXPECT_TRUE(instance.contains("parameter_a") &&
                instance.contains("parameter_b"))
        << "ConcordConfiguration::instantiateScope fails to copy parameters "
           "from instance template to instances.";
  }

  newScope.clear();
  newScope.declareParameter("parameter_b", "A newer parameter.");
  newScope.declareParameter("parameter_c", "A newer parameter.", "C");
  mockScopeSizerResult = 4;
  config.instantiateScope("new_scope");
  EXPECT_TRUE(config.scopeIsInstantiated("new_scope") &&
              (config.scopeSize("new_scope") == mockScopeSizerResult))
      << "ConcordConfiguration::instantiateScope failed to correctly"
         " re-instantiate a scope that has already been instantiated.";

  for (size_t i = 0; i < mockScopeSizerResult; ++i) {
    ConcordConfiguration& instance = config.subscope("new_scope", i);
    EXPECT_FALSE(instance.contains("parameter_a"))
        << "ConcordConfiguration::instantiateScope fails to correctly overwrite"
           " existing templates when re-instantiating a scope.";
    EXPECT_TRUE(instance.contains("parameter_b") &&
                instance.contains("parameter_c"))
        << "ConcordConfiguration::instantiateScope fails to copy parameters "
           "from instance template to instances.";
    try {
      instance.loadDefault("parameter_b");
      FAIL() << "ConcordConfiguratiom::instantiateScope fails to correctly "
                "overwrite parameter properties of existing instances when "
                "re-instantiating a scope.";
    } catch (ConfigurationResourceNotFoundException e) {
    }
  }

  config.declareScope("unsizable_scope", "description",
                      mockScopeSizerThatNeverWorks, nullptr);
  ConcordConfiguration::ParameterStatus sizeStatus =
      config.instantiateScope("unsizable_scope");
  EXPECT_EQ(sizeStatus, ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration::instantiateScope fails to return the parameter"
         " status returned by the sizer function it has been given.";
  EXPECT_FALSE(config.scopeIsInstantiated("unsizable_scope"))
      << "ConcordConfiguration::instantiateScope instantiated a scope despite"
         " the fact its scopeSizer returned a non-valid status.";
  try {
    size_t scopeSize = config.scopeSize("unsizable_scope");
    FAIL() << "ConcordConfiguration::scopeSize returns a size for a scope whose"
              " instantiation failed.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
}

TEST(config_test, configuration_scope_access) {
  ConcordConfiguration config;
  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  config.declareScope("scope_b", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeB = config.subscope("scope_b");
  scopeB.declareScope("scope_ba", "A description.", mockScopeSizer, nullptr);
  scopeB.declareScope("scope_bb", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeBA = scopeB.subscope("scope_ba");
  ConcordConfiguration& scopeBB = scopeB.subscope("scope_bb");
  mockScopeSizerResult = 4;
  scopeB.instantiateScope("scope_ba");
  ConcordConfiguration& scopeBA0 = scopeB.subscope("scope_ba", 0);
  ConcordConfiguration& scopeBA2 = scopeB.subscope("scope_ba", 2);
  scopeBA2.declareScope("scope_baa", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeBAA = scopeBA2.subscope("scope_baa");
  scopeBA.setConfigurationState("marked BA");
  scopeBB.setConfigurationState("marked BB");
  scopeBA0.setConfigurationState("marked BA0");
  scopeBA2.setConfigurationState("marked BA2");
  scopeBAA.setConfigurationState("marked BAA");

  try {
    ConcordConfiguration& scopeC = config.subscope("scope_c");
    FAIL() << "ConcordConfiguration::subscope fails to reject a request for a"
              " scope that does not exist.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  try {
    ConcordConfiguration& scopeC = config.subscope("scope_a", 2);
    FAIL() << "ConcordConfiguration::subscope fails to reject a request for a"
              " scope instance from a non-instantiated scope.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  try {
    ConcordConfiguration& scopeC = scopeB.subscope("scope_ba", 4);
    FAIL() << "ConcordConfiguration::subscope fails to reject a request for a"
              " scope instance with index beyond those that should be "
              "instantiated.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  EXPECT_EQ(config.getScopeDescription("scope_a"), "A description.")
      << "ConcordConfiguration::getScopeDescription fails to fetch the"
         " description a scope was created with.";

  try {
    std::string description = config.getScopeDescription("scope_c");
    FAIL() << "ConcordConfiguration::getScopeDescription fails to reject a"
              " request for the description of a scope that does not exist.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  ConfigurationPath pathBB("scope_bb", true);
  EXPECT_EQ(scopeB.subscope(pathBB).getConfigurationState(), "marked BB")
      << "ConfigurationPath::subscope fails to fetch the correct subscope.";

  EXPECT_EQ(scopeB.subscope(pathBB).getConfigurationState(), "marked BB")
      << "ConfigurationPath::subscope fails to fetch the correct subscope.";

  ConfigurationPath pathBA0("scope_ba", (size_t)0);
  ConfigurationPath pathBA2("scope_ba", (size_t)2);
  EXPECT_EQ(scopeB.subscope(pathBA0).getConfigurationState(), "marked BA0")
      << "ConfigurationPath::subscope fails to fetch the correct subscope.";
  EXPECT_EQ(scopeB.subscope(pathBA2).getConfigurationState(), "marked BA2")
      << "ConfigurationPath::subscope fails to fetch the correct subscope.";

  pathBA2.useInstance = false;
  EXPECT_EQ(scopeB.subscope(pathBA2).getConfigurationState(), "marked BA")
      << "ConfigurationPath::subscope fails to ignore index for non-instance"
         " paths.";
  pathBA0.useInstance = true;

  ConfigurationPath pathB_BA2_BAA("scope_b", true);
  pathB_BA2_BAA.subpath.reset(new ConfigurationPath("scope_ba", (size_t)2));
  pathB_BA2_BAA.subpath->subpath.reset(
      new ConfigurationPath("scope_baa", true));
  EXPECT_EQ(config.subscope(pathB_BA2_BAA).getConfigurationState(),
            "marked BAA")
      << "ConfigurationPath::subscope fails to fetch the correct"
         " subscope for a multi-step path.";

  ConfigurationPath pathC = ConfigurationPath("scope_c", true);
  try {
    ConcordConfiguration& scopeC = config.subscope(pathC);
    FAIL() << "ConcordConfiguration::subscope fails to reject a request with a"
              " path to a scope that does not exist.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  ConfigurationPath parameterA = ConfigurationPath("scope_a", false);
  try {
    ConcordConfiguration& scopeA = config.subscope(parameterA);
    FAIL() << "ConcordConfiguration::subscope fails to reject a request with a"
              " path to a parameter.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  ConfigurationPath pathBA4 = ConfigurationPath("scope_ba", (size_t)4);
  try {
    ConcordConfiguration& scopeBA4 = scopeB.subscope(pathBA4);
    FAIL() << "ConcordConfiguration::subscope fails to reject a request with a"
              " path to an instance that is out of range.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  EXPECT_TRUE(config.containsScope("scope_a"))
      << "ConcordConfiguration::containsScope fails to recognize an existing"
         " scope.";
  EXPECT_FALSE(config.containsScope("scope_c"))
      << "ConcordConfiguration::containsScope fails to recognize a scope that"
         " does not exist.";

  EXPECT_TRUE(scopeB.containsScope(pathBB))
      << "ConcordConfiguration::containsScope fails to reognize a path to an"
         " existing scope.";
  EXPECT_TRUE(scopeB.containsScope(pathBA0))
      << "ConcordConfiguration::containsScope fails to reognize a path to an"
         " existing scope instance.";
  EXPECT_TRUE(scopeB.containsScope(pathBA2))
      << "ConcordConfiguration::containsScope fails to reognize a path to an"
         " existing scope instance.";

  EXPECT_TRUE(config.containsScope(pathB_BA2_BAA))
      << "ConcordConfiguration::containsScope fails to reognize a multi-step"
         " path to an existing scope.";

  EXPECT_FALSE(config.containsScope(pathC))
      << "ConcordConfiguration::containsScope fails to reognize a path to a "
         "scope that does not exist.";
  EXPECT_FALSE(config.containsScope(parameterA))
      << "ConcordConfiguration::containsScope fails to distinguish between a "
         "path to a parameter and a path to a scope.";
  EXPECT_FALSE(scopeB.containsScope(pathBA4))
      << "ConcordConfiguration::containsScope fails to reognize that a scope"
         " instance is out of range.";

  EXPECT_TRUE(scopeB.scopeIsInstantiated("scope_ba"))
      << "ConcordConfiguration::scopeIsInstantiated fails to recognize a "
         "subscope that does have instances.";
  EXPECT_FALSE(config.scopeIsInstantiated("scope_a"))
      << "ConcordConfiguration::scopeIsInstantiated fails to recognize that a "
         "scope has not been instantiated.";
  EXPECT_FALSE(config.scopeIsInstantiated("scope_c"))
      << "Concordconfiguration::scopeIsInstantiated fails to recognize that a "
         "scope that has not been defined must have no instances.";

  EXPECT_EQ(scopeB.scopeSize("scope_ba"), 4)
      << "ConcordConfiguration::scopeSize fails to give the correct number of "
         "instances for a scope that has been instantiated.";
  try {
    size_t scopeSize = config.scopeSize("scope_a");
    FAIL() << "ConcordConfiguration::scopeSize fails to reject a request for "
              "the size of a scope that has not been instantiated.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    size_t scopeSize = config.scopeSize("scope_c");
    FAIL() << "ConcordConfiguration::scopeSize fails to reject a request for "
              "the size of a scope that has not been defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  config.declareScope("instanceless_scope", "A description.", mockScopeSizer,
                      nullptr);
  mockScopeSizerResult = 0;
  config.instantiateScope("instanceless_scope");
  EXPECT_TRUE(config.scopeIsInstantiated("instanceless_scope"))
      << "ConcordConfiguration::scopeIsInstantiated fails to recognize that a "
         "scope instantiated to 0 instances was instantiated.";
  EXPECT_EQ(config.scopeSize("instanceless_scope"), 0)
      << "ConcordConfiguration::scopeSize fails to correctly report the size "
         "of a scope that was instantiated to 0 instances.";
}

TEST(config_test, configuration_parameter_creation) {
  ConcordConfiguration config;

  config.declareParameter("parameter_a", "A description.");
  EXPECT_TRUE(config.contains("parameter_a"))
      << "ConcordConfiguration fails to detect the existance of a parameter it "
         "was asked to create.";
  EXPECT_EQ(config.getDescription("parameter_a"), "A description.")
      << "ConcordConfiguration fails to fetch the description a parameter was "
         "created with.";
  try {
    config.declareParameter("parameter_a", "A different description.");
    FAIL() << "ConcordConfiguration::declareParameter does not fail when "
              "trying to re-declare a parameter.";
  } catch (ConfigurationRedefinitionException e) {
  }

  config.declareParameter("parameter_b", "A description.", "default");
  EXPECT_TRUE(config.contains("parameter_b"))
      << "ConcordConfiguration fails to detect the existance of a parameter it "
         "was asked to create.";
  EXPECT_EQ(config.getDescription("parameter_b"), "A description.")
      << "ConcordConfiguration fails to fetch the description a parameter was "
         "created with.";
  try {
    config.declareParameter("parameter_b", "A different description.",
                            "default");
    FAIL() << "ConcordConfiguration::declareParameter does not fail when "
              "trying to re-declare a parameter.";
  } catch (ConfigurationRedefinitionException e) {
  }
  config.loadDefault("parameter_b");
  EXPECT_EQ(config.getValue("parameter_b"), "default")
      << "ConcordConfiguration fails to retain the default value a parameter "
         "is created with.";

  config.declareParameter("parameter_c", "A description.", "");
  config.loadDefault("parameter_c");
  EXPECT_EQ(config.getValue("parameter_c"), "")
      << "ConcordConfiguration::declareParameter fails to consider the empty "
         "string an acceptable default value.";

  std::vector<std::string> tags;
  config.tagParameter("parameter_a", tags);
  tags.push_back("tag A");
  config.tagParameter("parameter_a", tags);
  EXPECT_TRUE(config.isTagged("parameter_a", "tag A"))
      << "ConcordConfiguration fails to detect a tag a parameter was tagged "
         "with.";
  config.tagParameter("parameter_a", tags);
  tags.push_back("tag B");
  tags.push_back("tag C");
  tags.push_back("");
  config.tagParameter("parameter_a", tags);
  EXPECT_TRUE(config.isTagged("parameter_a", "tag B"))
      << "ConcordConfiguration fails to detect a tag a parameter was tagged "
         "with.";
  EXPECT_TRUE(config.isTagged("parameter_a", ""))
      << "ConcordConfiguration fails to handle an empty string as a tag.";

  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  try {
    config.declareParameter("scope_a", "A description.");
    FAIL() << "ConcordConfiguration fails to disallow naming conflicts between "
              "scopes and parameters.";
  } catch (ConfigurationRedefinitionException e) {
  }
  try {
    config.declareScope("parameter_a", "A description.", mockScopeSizer,
                        nullptr);
    FAIL() << "ConcordConfiguration fails to disallow naming conflicts between "
              "scopes and parameters.";
  } catch (ConfigurationRedefinitionException e) {
  }
}

TEST(config_test, configuration_parameter_access) {
  ConcordConfiguration config;

  config.declareParameter("parameter_a", "A description.");
  config.declareParameter("parameter_b", "A description.");
  config.loadValue("parameter_b", "A value.");
  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  mockScopeSizerResult = 3;
  config.instantiateScope("scope_a");
  ConcordConfiguration& scopeA2 = config.subscope("scope_a", 2);
  scopeA2.declareScope("scope_a2a", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeA2A = scopeA2.subscope("scope_a2a");
  scopeA2A.declareParameter("parameter_a2aa", "A description.");
  scopeA2A.declareParameter("parameter_a2ab", "A description.");
  scopeA2A.loadValue("parameter_a2ab", "A different value.");

  EXPECT_FALSE(config.isTagged("parameter_a", "tag A"))
      << "ConcordConfiguration::isTagged fails to correctly handle parameters "
         "with no tags.";
  std::vector<std::string> tags({"tag A"});
  config.tagParameter("parameter_a", tags);
  EXPECT_TRUE(config.isTagged("parameter_a", "tag A"))
      << "ConcordConfiguration::isTagged fails to recognize a tag a parameter "
         "is tagged with.";
  EXPECT_FALSE(config.isTagged("parameter_a", "tag B"))
      << "ConcordConfiguration::isTagged fails to recognize a tag a parameter "
         "is not tagged with.";
  try {
    bool tagged = config.isTagged("parameter_c", "tag A");
    FAIL() << "ConcordConfiguration::isTagged fails to reject a request for a"
              " parameter that has not been defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  EXPECT_EQ(config.getDescription("parameter_a"), "A description.")
      << "ConcordConfiguration::getDescription fails to fetch the correct "
         "description for a parameter.";
  try {
    std::string description = config.getDescription("parameter_c");
    FAIL() << "ConcordConfiguration::getDescription fails to reject a request "
              "for the description of a parameter that has not been defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  EXPECT_TRUE(config.contains("parameter_a"))
      << "ConcordConfiguration::contains fails to recognize an existing "
         "parameter.";
  EXPECT_FALSE(config.contains("parameter_c"))
      << "ConcordConfiguration::contains fails to recognize a parameter that "
         "does not exist.";

  ConfigurationPath pathA("parameter_a", false);
  ConfigurationPath pathC("parameter_c", false);
  EXPECT_TRUE(config.contains(pathA)) << "ConcordConfiguration::contains fails "
                                         "to recognize an existing parameter.";
  EXPECT_FALSE(config.contains(pathC))
      << "ConcordConfiguration::contains fails to recognize a parameter that "
         "does not exist.";

  ConfigurationPath scopeA("scope_a", true);
  EXPECT_FALSE(config.contains(scopeA))
      << "ConcordConfiguration::contains fails to recognize a path that is to "
         "a scope rather than a parameter.";

  ConfigurationPath pathA2AA("scope_a", (size_t)2);
  pathA2AA.subpath.reset(new ConfigurationPath("scope_a2a", true));
  pathA2AA.subpath->subpath.reset(
      new ConfigurationPath("parameter_a2aa", false));
  ConfigurationPath pathAAA(pathA2AA);
  pathAAA.useInstance = false;
  EXPECT_TRUE(config.contains(pathA2AA))
      << "ConcordConfiguration::contains fails to correctly traverse a path to "
         "a parameter.";
  EXPECT_FALSE(config.contains(pathAAA))
      << "ConcordConfiguration::contains fails to correctly traverse a path to "
         "a parameter that does not exist.";

  EXPECT_TRUE(config.hasValue("parameter_b"))
      << "ConcordConfiguration::hasValue fails to correctly detect a parameter "
         "with a value.";
  EXPECT_FALSE(config.hasValue("parameter_a"))
      << "ConcordConfiguration::hasValue fails to correctly detect a parameter "
         "that has no value.";
  EXPECT_FALSE(config.hasValue("parameter_c"))
      << "ConcordConfiguration::hasValue fails to correctly report that a "
         "parameter which has not been defined cannot have any value.";

  ConfigurationPath pathB("parameter_b", false);
  EXPECT_TRUE(config.hasValue(pathB))
      << "ConcordConfiguration::hasValue fails to correctly detect a parameter "
         "with a value.";
  EXPECT_FALSE(config.hasValue(pathA))
      << "ConcordConfiguration::hasValue fails to correctly detect a parameter "
         "that has no value.";
  EXPECT_FALSE(config.hasValue(pathC))
      << "ConcordConfiguration::hasValue fails to correctly report that a "
         "parameter which has not been defined cannot have any value.";

  ConfigurationPath pathA2AB(pathA2AA);
  pathA2AB.subpath->subpath->name = "parameter_a2ab";
  EXPECT_FALSE(config.hasValue(pathA2AA))
      << "ConcordConfiguration::hasValue fails to correctly traverse a path to "
         "a parameter.";
  EXPECT_TRUE(config.hasValue(pathA2AB))
      << "ConcordConfiguration::hasValue fails to correctly traverse a path to "
         "a parameter.";

  EXPECT_EQ(config.getValue("parameter_b"), "A value.")
      << "ConcordConfiguration::getValue fails to fetch the correct value for "
         "a parameter.";
  try {
    std::string value = config.getValue("parameter_a");
    FAIL() << "ConcordConfiguration::getValue fails to reject a request to get "
              "the value of an uninitialized parameter.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    std::string value = config.getValue("parameter_c");
    FAIL() << "ConcordConfiguration::getValue fails to reject a request to get "
              "the value of a parameter which does not exist.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  EXPECT_EQ(config.getValue(pathB), "A value.")
      << "ConcordConfiguration::getValue fails to fetch the correct value for "
         "a parameter.";
  try {
    std::string value = config.getValue(pathA);
    FAIL() << "ConcordConfiguration::getValue fails to reject a request to get "
              "the value of an uninitialized parameter.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    std::string value = config.getValue(pathC);
    FAIL() << "ConcordConfiguration::getValue fails to reject a request to get "
              "the value of a parameter which does not exist.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  try {
    std::string value = config.getValue(pathA2AA);
    FAIL() << "ConcordConfiguration::getValue fails to correctly traverse a "
              "path to a parameter.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  EXPECT_EQ(config.getValue(pathA2AB), "A different value.")
      << "ConcordConfiguration::getValue fails to correctly traverse a path to "
         "a parameter.";

  try {
    ConcordConfiguration::ParameterStatus status =
        config.loadValue("parameter_c", "A value.");
    FAIL() << "ConcordConfiguration::loadValue fails to reject a request to "
              "load a value to a parameter which has not been defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  config.loadValue("parameter_b", "A new value.", nullptr, false);
  EXPECT_EQ(config.getValue("parameter_b"), "A value.")
      << "ConcordConfiguration::loadValue overwrites existing value even when "
         "overwrite is not requested.";
  config.loadValue("parameter_b", "A new value.", nullptr, true);
  EXPECT_EQ(config.getValue("parameter_b"), "A new value.")
      << "ConcordConfiguration::loadValue fails to overwrite existing value "
         "even when overwrite is requested.";
  std::string prevValue;
  config.loadValue("parameter_b", "An even newer value.", nullptr, true,
                   &prevValue);
  EXPECT_EQ(prevValue, "A new value.")
      << "ConcordConfiguration::loadValue fails to write back the overwritten "
         "value when a pointer to receive it is given.";
  prevValue = "prevValue";
  config.loadValue("parameter_b", "A still newer value.", nullptr, false,
                   &prevValue);
  EXPECT_EQ(prevValue, "prevValue")
      << "ConcordConfiguration::loadValue writes back an overwritten value "
         "even when it does not overwrite a value.";

  config.eraseValue("parameter_a");
  EXPECT_FALSE(config.hasValue("parameter_a"))
      << "ConcordConfiguration reports it still has a value for a parameter "
         "after erasing the value for that parameter.";
  try {
    config.eraseValue("parameter_c");
    FAIL() << "ConcordConfiguration::eraseValue fails to reject a request to "
              "erase the value of a parameter which has not been defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  config.loadValue("parameter_b", "A replacement value.");
  config.eraseValue("parameter_b", &prevValue);
  EXPECT_EQ(prevValue, "A replacement value.")
      << "ConcordConfiguration::eraseValue fails to write back the erased "
         "value when a pointer to receive it is given.";
  prevValue = "prevValue";
  config.eraseValue("parameter_b", &prevValue);
  EXPECT_EQ(prevValue, "prevValue")
      << "ConcordConfiguration::eraseValue writes back an erased value even "
         "when it does not erase a value.";

  config.loadValue("parameter_b", "A replacement value.");
  config.eraseAllValues();
  auto iterator = config.begin(ConcordConfiguration::kIterateAllParameters);
  auto end = config.end(ConcordConfiguration::kIterateAllParameters);
  while (iterator != end) {
    EXPECT_FALSE(config.hasValue(*iterator))
        << "ConcordConfiguration::eraseAllValues fails to erase all values "
           "stored.";
    ++iterator;
  }

  ConcordConfiguration emptyConfig;
  emptyConfig.eraseAllValues();
}

static ConcordConfiguration::ParameterStatus mockValidatorResult;
static std::string mockValidatorFailureMessage;
static ConcordConfiguration::ParameterStatus mockValidator(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  if (state) {
    bool* wasCalled = static_cast<bool*>(state);
    *wasCalled = true;
  }
  if (failureMessage &&
      (mockValidatorResult != ConcordConfiguration::ParameterStatus::VALID)) {
    *failureMessage = mockValidatorFailureMessage;
  }
  return mockValidatorResult;
}

static ConcordConfiguration::ParameterStatus mockValidatorRequireNoSpaces(
    const std::string& value, const ConcordConfiguration& config,
    const ConfigurationPath& path, std::string* failureMessage, void* state) {
  if (value.find(" ") == std::string::npos) {
    return ConcordConfiguration::ParameterStatus::VALID;
  } else {
    if (failureMessage) {
      *failureMessage =
          "Spaces are not allowed in parameter " + path.toString() + ".";
    }
    return ConcordConfiguration::ParameterStatus::INVALID;
  }
}

static ConcordConfiguration::ParameterStatus mockGeneratorStatus;
static std::string mockGeneratorOutput;
static ConcordConfiguration::ParameterStatus mockGenerator(
    const ConcordConfiguration& config, const ConfigurationPath& path,
    std::string* output, void* state) {
  if (state) {
    bool* wasCalled = static_cast<bool*>(state);
    *wasCalled = true;
  }
  *output = mockGeneratorOutput;
  return mockGeneratorStatus;
}

TEST(config_test, configuration_parameter_validation) {
  ConcordConfiguration config;

  config.declareParameter("parameter_a", "A description.");
  config.addValidator("parameter_a", mockValidatorRequireNoSpaces, nullptr);
  EXPECT_EQ(config.loadValue("parameter_a", "has spaces"),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration fails to correctly use a parameter's validator "
         "function.";
  EXPECT_FALSE(config.hasValue("parameter_a"))
      << "ConcordConfiguration fails to enforce a parameter's validator "
         "function.";
  EXPECT_EQ(config.loadValue("parameter_a", "hasn't_spaces"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration fails to correctly use a parameter's validator "
         "function.";
  EXPECT_TRUE(config.hasValue("parameter_a"))
      << "ConcordConfiguration fails to enforce a parameter's validator "
         "function.";
  EXPECT_EQ(config.validate("parameter_a"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validate fails to correctly validate a "
         "parameter.";

  bool mockValidatorCalled = false;
  config.addValidator("parameter_a", mockValidator, &mockValidatorCalled);
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  EXPECT_EQ(config.loadValue("parameter_a", "has spaces"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration fails to correctly replace the validtor for a "
         "parameter with an existing validator.";
  EXPECT_EQ(config.getValue("parameter_a"), "has spaces")
      << "ConcordConfiguration fails to correctly replace the validator for a "
         "parameter with an existing validator.";
  EXPECT_TRUE(mockValidatorCalled) << "ConcordConfiguration fails to correctly "
                                      "call a parameter's validation function.";
  mockValidatorCalled = false;
  EXPECT_EQ(config.validate("parameter_a"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validate fails to correctly replace the "
         "validator for a parameter with an existing validator.";
  EXPECT_TRUE(mockValidatorCalled)
      << "ConcordConfiguration::validate fails to correctly call a parameter's "
         "validation function.";

  config.declareParameter("parameter_b", "A description.");
  config.eraseValue("parameter_a");
  EXPECT_EQ(config.validate("parameter_a"),
            ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION)
      << "ConcordConfiguration::validate fails to correctly handle a request "
         "to validate an uninitialized parameter.";
  EXPECT_EQ(config.loadValue("parameter_b", "A value."),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::loadValue fails to correctly report its "
         "success in loading a value to a parameter with no validators.";
  EXPECT_EQ(config.validate("parameter_b"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validate fails to correctly handle a parameter "
         "with no validators.";

  config.eraseValue("parameter_a");
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  mockValidatorFailureMessage = "Mock validator failed.";
  std::string failureMessage = "Hasn't failed.";
  config.loadValue("parameter_a", "A value.", &failureMessage);
  EXPECT_EQ(failureMessage, "Hasn't failed.")
      << "ConcordConfiguration::loadParameter writes a failure message even "
         "when validation does not fail.";
  config.eraseValue("parameter_a");
  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  config.loadValue("parameter_a", "A value.", &failureMessage);
  EXPECT_EQ(failureMessage, "Mock validator failed.")
      << "ConcordConfiguration::loadParameter fails to write the failure "
         "message provided by the validator when the validator fails.";

  config.eraseValue("parameter_a");
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  mockValidatorFailureMessage = "Mock validator failed.";
  failureMessage = "Hasn't failed.";
  config.loadValue("parameter_a", "A value.");
  config.validate("parameter_a", &failureMessage);
  EXPECT_EQ(failureMessage, "Hasn't failed.")
      << "ConcordConfiguration::validate writes a failure message even when "
         "the validator does not fail.";
  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  config.loadValue("parameter_a", "A different value.");
  config.validate("parameter_a", &failureMessage);
  EXPECT_EQ(failureMessage, "Mock validator failed.")
      << "ConcordConfiguration::validate fails to write the validator's "
         "failureMessage when the validator fails.";

  try {
    config.addValidator("parameter_c", mockValidator, nullptr);
    FAIL() << "ConcordConfiguration::addValidator fails to reject a request to "
              "add a validator to a parameter which is not defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    config.addValidator("parameter_a", nullptr, nullptr);
    FAIL() << "ConcordConfiguration::addValidator fails to reject a request to "
              "add a null validator to a parameter.";
  } catch (std::invalid_argument e) {
  }
  try {
    ConcordConfiguration::ParameterStatus status =
        config.validate("parameter_c");
    FAIL() << "ConcordConfiguration::validate fails to reject a request to "
              "validate a parameter which has not been defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  config.eraseAllValues();
  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeA = config.subscope("scope_a");
  scopeA.declareParameter("parameter_aa", "A description.");
  scopeA.declareParameter("parameter_ab", "A description.");
  scopeA.addValidator("parameter_aa", mockValidator, nullptr);
  scopeA.loadValue("parameter_ab", "hasn spaces");
  scopeA.addValidator("parameter_ab", mockValidatorRequireNoSpaces, nullptr);
  mockScopeSizerResult = 1;
  config.instantiateScope("scope_a");
  ConcordConfiguration& scopeA0 = config.subscope("scope_a", 0);

  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  scopeA0.loadValue("parameter_aa", "A value.");
  config.loadValue("parameter_a", "hasn't_spaces");
  config.loadValue("parameter_b", "A value.");
  EXPECT_EQ(config.validateAll(),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "Concordconfiguration::validateAll fails to correctly aggregate "
         "validation results for parameters.";
  scopeA.loadValue("parameter_ab", "hasn't_spaces");
  scopeA0.loadValue("parameter_ab", "hasn't_spaces");
  EXPECT_EQ(config.validateAll(), ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validateAll fails to correctly validate all "
         "parameters in a configuration.";

  config.eraseValue("parameter_a");
  EXPECT_EQ(config.validateAll(true),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validateAll fails to ignore uninitialized "
         "parameters even when this is requested.";
  EXPECT_EQ(config.validateAll(false),
            ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION)
      << "ConcordConfiguration::validateAll fails ignores uninitialized "
         "parameters even when this is not requested.";
  config.loadValue("parameter_a", "hasn't_spaces");
  scopeA.loadValue("parameter_aa", "has spaces");
  scopeA.addValidator("parameter_aa", mockValidatorRequireNoSpaces, nullptr);
  EXPECT_EQ(config.validateAll(true, false),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validateAll validates instance templates even "
         "when this is not requested.";
  EXPECT_EQ(config.validateAll(true, true),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration::validateAll does not validate instance "
         "templates even when this is requested.";

  ConcordConfiguration emptyConfig;
  EXPECT_EQ(emptyConfig.validateAll(),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::validateAll fails to handle an empty "
         "configuration correctly.";
}

TEST(config_test, configuration_parameter_defaults) {
  ConcordConfiguration config;

  config.declareParameter("parameter_a", "A description.", "default_value");
  config.declareParameter("parameter_b", "A description.");

  EXPECT_EQ(config.loadDefault("parameter_a"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::loadDefault fails to load the correct default "
         "value for a parameter.";
  EXPECT_EQ(config.getValue("parameter_a"), "default_value")
      << "ConcordConfiguration::laodDefault fails to load the correct default "
         "value for a parameter.";

  try {
    config.loadDefault("parameter_b");
    FAIL() << "ConcordConfiguration::loadDefault fails to reject a request to "
              "load the default value for a parameter with no default value.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    config.loadDefault("parameter_c");
    FAIL() << "ConcordConfiguration::loadDefault fails to reject a request to "
              "load the default value for a parameter that does not exist.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  config.loadValue("parameter_a", "a value");
  config.loadDefault("parameter_a", nullptr, false);
  EXPECT_EQ(config.getValue("parameter_a"), "a value")
      << "ConcordConfiguration::loadDefault overwrites a value even when "
         "overwrite is not specified.";
  config.loadDefault("parameter_a", nullptr, true);
  EXPECT_EQ(config.getValue("parameter_a"), "default_value")
      << "ConcordConfiguration::loadDefault fails to overwrite existing value "
         "even when overwrite is specified.";

  std::string prevValue = "prevValue";
  config.loadValue("parameter_a", "a value");
  config.loadDefault("parameter_a", nullptr, false, &prevValue);
  EXPECT_EQ(prevValue, "prevValue")
      << "ConcordConfiguration::loadDefault writes an overwritten value back "
         "even if it does not overwrite a value.";
  config.loadDefault("parameter_a", nullptr, true, &prevValue);
  EXPECT_EQ(prevValue, "a value")
      << "ConcordConfiguration::loadDefault fails to correctly write back the "
         "value it overwrote when appropriate.";

  config.loadValue("parameter_a", "a value");
  config.addValidator("parameter_a", mockValidator, nullptr);
  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  EXPECT_EQ(config.loadDefault("parameter_a", nullptr, true),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration::loadDefault fails to enforce validators on "
         "default values.";
  EXPECT_EQ(config.getValue("parameter_a"), "a value")
      << "ConcordConfiguration::loadDefault fails to leave the existing value "
         "of a parameter intact if the default value fails validation.";

  mockValidatorResult =
      ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  EXPECT_EQ(config.loadDefault("parameter_a", nullptr, true),
            ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION)
      << "ConcordConfiguration::loadDefault fails to correctly report "
         "validator results.";
  EXPECT_EQ(config.getValue("parameter_a"), "default_value")
      << "ConcordConfiguration::loadDefault fails to correctly enforce "
         "validator results.";

  config.eraseValue("parameter_a");
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  mockValidatorFailureMessage = "Mock validator failed.";
  std::string failureMessage = "Hasn't failed.";
  config.loadDefault("parameter_a", &failureMessage);
  EXPECT_EQ(failureMessage, "Hasn't failed.")
      << "ConcordConfiguration::loadDefault writes a failure message even when "
         "the validator does not fail.";
  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  config.loadDefault("parameter_a", &failureMessage);
  EXPECT_EQ(failureMessage, "Mock validator failed.")
      << "ConcordConfiguration::loadDefault fails to write the validator's "
         "failureMessage when the validator fails.";

  config.clear();
  config.declareParameter("parameter_a", "A description.", "default_value");
  config.declareParameter("parameter_b", "A description.");
  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeA = config.subscope("scope_a");
  scopeA.declareParameter("parameter_aa", "A description.",
                          "different_default_value");
  scopeA.declareParameter("parameter_ab", "A description.");
  mockScopeSizerResult = 1;
  config.instantiateScope("scope_a");
  ConcordConfiguration& scopeA0 = config.subscope("scope_a", 0);

  config.loadAllDefaults();
  EXPECT_EQ(config.getValue("parameter_a"), "default_value")
      << "ConcordConfiguration::loadAllDefaults fails to load the correct "
         "default value for parameters.";
  EXPECT_FALSE(config.hasValue("parameter_b"))
      << "ConcordConfiguration::loadAllDefaults appears to load a value for "
         "parameters without default values.";
  EXPECT_EQ(scopeA0.getValue("parameter_aa"), "different_default_value")
      << "ConcordConfiguration::loadAllDefaults fails to act recursively.";

  scopeA0.loadValue("parameter_aa", "not default");
  config.loadAllDefaults(false);
  EXPECT_EQ(scopeA0.getValue("parameter_aa"), "not default")
      << "ConcordConfiguration::loadAllDefaults overwrites existing values "
         "even when overwrite is not specified.";
  config.loadAllDefaults(true);
  EXPECT_EQ(scopeA0.getValue("parameter_aa"), "different_default_value")
      << "ConcordConfiguration::loadAllDefaults fails to overwrite existing "
         "values even when overwrite is specified.";

  config.eraseAllValues();
  config.loadAllDefaults(true, false);
  EXPECT_FALSE(scopeA.hasValue("parameter_aa"))
      << "ConcordConfiguration::loadAllDefaults loads defaults for scope "
         "templates even when asked otherwise.";
  config.loadAllDefaults(true, true);
  EXPECT_EQ(scopeA.getValue("parameter_aa"), "different_default_value")
      << "ConcordConfiguration::getValue fails to load defaults for templates "
         "even when requested.";

  ConcordConfiguration emptyConfig;
  emptyConfig.loadAllDefaults();
}

TEST(config_test, configuration_parameter_generation) {
  ConcordConfiguration config;

  config.declareParameter("parameter_a", "A description.");
  config.declareParameter("parameter_b", "A description.");
  bool mockGeneratorCalled = false;
  config.addGenerator("parameter_a", mockGenerator, &mockGeneratorCalled);

  mockGeneratorStatus = ConcordConfiguration::ParameterStatus::VALID;
  mockGeneratorOutput = "generated value";
  EXPECT_EQ(config.generate("parameter_a"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration fails to correctly generate a value for a "
         "parameter.";
  EXPECT_EQ(config.getValue("parameter_a"), "generated value")
      << "ConcordConfiguration fails to correctly generate a value for a "
         "parameter.";
  EXPECT_TRUE(mockGeneratorCalled)
      << "ConcordConfiguration fails to correctly use a generator function "
         "added to a parameter.";

  config.eraseValue("parameter_a");
  mockGeneratorCalled = false;
  bool mockGeneratorCalledAgain = false;
  config.addGenerator("parameter_a", mockGenerator, &mockGeneratorCalledAgain);
  mockGeneratorOutput = "new generated value";
  config.generate("parameter_a");
  EXPECT_EQ(config.getValue("parameter_a"), "new generated value")
      << "ConcordConfiguration fails to correctly generate a value for a "
         "parameter.";
  EXPECT_FALSE(mockGeneratorCalled)
      << "ConcordConfiguration::addGenerator fails to replace an existing "
         "generator when a new one is added.";
  EXPECT_TRUE(mockGeneratorCalledAgain)
      << "ConcordConfiguration::addGenerator fails to replace a generator with "
         "a new generator.";

  config.addGenerator("parameter_a", mockGenerator, nullptr);
  mockGeneratorStatus = ConcordConfiguration::ParameterStatus::VALID;
  mockGeneratorOutput = "generated value";
  config.loadValue("parameter_a", "a value");
  config.generate("parameter_a", nullptr, false);
  EXPECT_EQ(config.getValue("parameter_a"), "a value")
      << "ConcordConfiguration::generate overwrites existing value even when "
         "this is not requested.";
  config.generate("parameter_a", nullptr, true);
  EXPECT_EQ(config.getValue("parameter_a"), "generated value")
      << "ConcordConfiguration::generate fails to overwrite existing value "
         "even when this is requested.";

  std::string prevValue = "prevValue";
  config.loadValue("parameter_a", "a value");
  config.generate("parameter_a", nullptr, true, &prevValue);
  EXPECT_EQ(prevValue, "a value")
      << "ConcordConfiguration::generate fails to write back the overwritten "
         "value even when a pointer is provided to do so.";
  prevValue = "prevValue";
  config.loadValue("parameter_a", "a value");
  config.generate("parameter_a", nullptr, false, &prevValue);
  EXPECT_EQ(prevValue, "prevValue")
      << "ConcordConfiguration::generate writes back a value to the "
         "overwritten value pointer even when it does not overwrite a value.";

  config.eraseValue("parameter_a");
  config.addValidator("parameter_a", mockValidator, nullptr);
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  mockGeneratorOutput = "generated value";
  mockGeneratorStatus = ConcordConfiguration::ParameterStatus::INVALID;
  EXPECT_EQ(config.generate("parameter_a"),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration::generate fails to correctly aggregate the "
         "status returned by generation and that returned by validation.";
  EXPECT_FALSE(config.hasValue("parameter_a"))
      << "ConcordConfiguration::generate appears to load a value even when the "
         "generator does not report its output is valid.";
  mockGeneratorStatus =
      ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  EXPECT_EQ(config.generate("parameter_a"),
            ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION)
      << "ConcordConfiguration::generate fails to correctly aggregate the "
         "status returned by generation and that returned by validation.";
  EXPECT_FALSE(config.hasValue("parameter_a"))
      << "ConcordConfiguration::generate appears to load a value even when the "
         "generator does not report its output is valid.";
  mockGeneratorStatus = ConcordConfiguration::ParameterStatus::VALID;
  EXPECT_EQ(config.generate("parameter_a"),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::generate fails to correctly aggregate the "
         "status returned by generation and that returned by validation.";
  EXPECT_EQ(config.getValue("parameter_a"), "generated value")
      << "ConcordConfiguration::generate fails to correctly load the "
         "generated value when appropriate.";

  mockValidatorResult =
      ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION;
  config.eraseValue("parameter_a");
  EXPECT_EQ(config.generate("parameter_a"),
            ConcordConfiguration::ParameterStatus::INSUFFICIENT_INFORMATION)
      << "ConcordConfiguration::generate fails to correctly aggregate the "
         "status returned by generation and that returned by validation.";
  EXPECT_EQ(config.getValue("parameter_a"), "generated value")
      << "ConcordConfiguration::generate fails to correctly load the "
         "generated value when appropriate.";
  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  config.eraseValue("parameter_a");
  EXPECT_EQ(config.generate("parameter_a"),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration::generate fails to correctly aggregate the "
         "status returned by generation and that returned by validation.";
  EXPECT_FALSE(config.hasValue("parameter_a"))
      << "ConcordConfiguration::generate appears to load generated values that "
         "fail validation.";

  config.eraseValue("parameter_a");
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  mockValidatorFailureMessage = "Mock validator failed.";
  std::string failureMessage = "Hasn't failed.";
  config.generate("parameter_a", &failureMessage);
  EXPECT_EQ(failureMessage, "Hasn't failed.")
      << "ConcordConfiguration::generate writes a failure message even when "
         "the validator does not fail.";
  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  config.generate("parameter_a", &failureMessage);
  EXPECT_EQ(failureMessage, "Mock validator failed.")
      << "ConcordConfiguration::generate fails to write the validator's "
         "failureMessage when the validator fails.";

  try {
    config.addGenerator("parameter_c", mockGenerator, nullptr);
    FAIL() << "ConcordConfiguration::addGenerator fails to reject a request to "
              "add a generator to a parameter which is not defined.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    config.addGenerator("parameter_a", nullptr, nullptr);
    FAIL() << "ConcordConfiguration::addGenerator fails to reject a request to "
              "add a null generator to a parameter.";
  } catch (std::invalid_argument e) {
  }
  try {
    config.generate("parameter_c");
    FAIL() << "ConcordConfiguration::generate fails to reject a request to "
              "generate a value for an undefined parameter.";
  } catch (ConfigurationResourceNotFoundException e) {
  }
  try {
    config.generate("parameter_b");
    FAIL() << "ConcordConfiguration::generate fails to reject a request to "
              "generate a value for a parameter with no generator.";
  } catch (ConfigurationResourceNotFoundException e) {
  }

  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;
  config.eraseAllValues();
  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeA = config.subscope("scope_a");
  scopeA.declareParameter("parameter_aa", "A description.");
  scopeA.addGenerator("parameter_aa", mockGenerator, nullptr);
  mockScopeSizerResult = 1;
  config.instantiateScope("scope_a");
  ConcordConfiguration& scopeA0 = config.subscope("scope_a", 0);

  EXPECT_EQ(config.generateAll(), ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::generateAll fails to correctly generate "
         "parameters.";
  EXPECT_EQ(config.getValue("parameter_a"), "generated value")
      << "ConcordConfiguration::generateAll fails to correctly generate "
         "parameters.";
  EXPECT_FALSE(config.hasValue("parameter_b"))
      << "ConcordConfiguration::generateAll appears to effect parameters "
         "without generators.";
  EXPECT_EQ(scopeA0.getValue("parameter_aa"), "generated value")
      << "ConcordConfiguration::generateAll fails to act recursively.";

  mockValidatorResult = ConcordConfiguration::ParameterStatus::INVALID;
  EXPECT_EQ(config.generateAll(),
            ConcordConfiguration::ParameterStatus::INVALID)
      << "ConcordConfiguration::generateAll fails to correctly aggregate "
         "generation and validation results.";
  mockValidatorResult = ConcordConfiguration::ParameterStatus::VALID;

  config.loadValue("parameter_a", "a value");
  config.generateAll(false);
  EXPECT_EQ(config.getValue("parameter_a"), "a value")
      << "ConcordConfiguration::generateAll overwrites existing values even "
         "when this is not requested.";
  config.generateAll(true);
  EXPECT_EQ(config.getValue("parameter_a"), "generated value")
      << "ConcordConfiguration::generateAll fails to overwrite existing values "
         "even when this is requested.";

  config.eraseAllValues();
  config.generateAll(true, false);
  EXPECT_FALSE(scopeA.hasValue("parameter_aa"))
      << "ConcordConfiguration::generateAll generates values for scope "
         "templates even when this is not requested.";
  config.generateAll(true, true);
  EXPECT_EQ(scopeA.getValue("parameter_aa"), "generated value")
      << "ConcordConfiguration::generateAll fails to generate values for scope "
         "templates even when this is requested.";

  ConcordConfiguration emptyConfig;
  EXPECT_EQ(emptyConfig.generateAll(),
            ConcordConfiguration::ParameterStatus::VALID)
      << "ConcordConfiguration::generateAll fails to correctly handle an empty "
         "configuration.";
}

TEST(config_test, configuration_iteration) {
  ConcordConfiguration config;

  EXPECT_EQ(config.begin(), config.end())
      << "ConcordConfiguration iterators to the beginning and end of an empty "
         "configuration are not equal.";

  config.declareParameter("parameter_a", "A description.");
  ConfigurationPath pathParameterA("parameter_a", false);
  config.declareParameter("parameter_b", "A description.");
  ConfigurationPath pathParameterB("parameter_b", false);
  config.declareParameter("parameter_c", "A description.");
  ConfigurationPath pathParameterC("parameter_c", false);

  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeA("scope_a", true);
  config.declareScope("scope_b", "A description.", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeB("scope_b", true);
  config.declareScope("scope_c", "A description.", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeC("scope_c", true);

  ConcordConfiguration& scopeA = config.subscope("scope_a");
  scopeA.declareScope("scope_aa", "A description", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeAA(pathScopeA);
  pathScopeAA.subpath.reset(new ConfigurationPath("scope_aa", true));
  ConcordConfiguration& scopeAA = scopeA.subscope("scope_aa");
  scopeA.declareScope("scope_ab", "A description", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeAB(pathScopeA);
  pathScopeAB.subpath.reset(new ConfigurationPath("scope_ab", true));
  ConcordConfiguration& scopeAB = scopeA.subscope("scope_ab");
  scopeA.declareParameter("parameter_aa", "A description.");
  ConfigurationPath pathParameterAA(pathScopeA);
  pathParameterAA.subpath.reset(new ConfigurationPath("parameter_aa", false));
  scopeA.declareParameter("parameter_ab", "A description.");
  ConfigurationPath pathParameterAB(pathScopeA);
  pathParameterAB.subpath.reset(new ConfigurationPath("parameter_ab", false));

  scopeAA.declareParameter("parameter_aaa", "A description.");
  ConfigurationPath pathParameterAAA(pathScopeAA);
  pathParameterAAA.subpath->subpath.reset(
      new ConfigurationPath("parameter_aaa", false));
  scopeAA.declareParameter("parameter_aab", "A description.");
  ConfigurationPath pathParameterAAB(pathScopeAA);
  pathParameterAAB.subpath->subpath.reset(
      new ConfigurationPath("parameter_aab", false));
  scopeAB.declareParameter("parameter_aba", "A description.");
  ConfigurationPath pathParameterABA(pathScopeAB);
  pathParameterABA.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));

  ConcordConfiguration& scopeB = config.subscope("scope_b");
  scopeB.declareParameter("parameter_ba", "A description.");
  ConfigurationPath pathParameterBA(pathScopeB);
  pathParameterBA.subpath.reset(new ConfigurationPath("parameter_ba", false));
  scopeB.declareParameter("parameter_bb", "A description.");
  ConfigurationPath pathParameterBB(pathScopeB);
  pathParameterBB.subpath.reset(new ConfigurationPath("parameter_bb", false));
  scopeB.declareParameter("parameter_bc", "A description.");
  ConfigurationPath pathParameterBC(pathScopeB);
  pathParameterBC.subpath.reset(new ConfigurationPath("parameter_bc", false));

  mockScopeSizerResult = 2;
  scopeA.instantiateScope("scope_ab");
  ConfigurationPath pathScopeAB0(pathScopeA);
  pathScopeAB0.subpath.reset(new ConfigurationPath("scope_ab", (size_t)0));
  ConfigurationPath pathScopeAB1(pathScopeA);
  pathScopeAB1.subpath.reset(new ConfigurationPath("scope_ab", (size_t)1));
  ConfigurationPath pathParameterAB0A(pathScopeAB0);
  pathParameterAB0A.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));
  ConfigurationPath pathParameterAB1A(pathScopeAB1);
  pathParameterAB1A.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));

  config.instantiateScope("scope_a");
  ConfigurationPath pathScopeA0("scope_a", (size_t)0);
  ConfigurationPath pathParameterA0A(pathScopeA0);
  pathParameterA0A.subpath.reset(new ConfigurationPath("parameter_aa", false));
  ConfigurationPath pathParameterA0B(pathScopeA0);
  pathParameterA0B.subpath.reset(new ConfigurationPath("parameter_ab", false));
  ConfigurationPath pathScopeA0A(pathScopeA0);
  pathScopeA0A.subpath.reset(new ConfigurationPath("scope_aa", true));
  ConfigurationPath pathParameterA0AA(pathScopeA0A);
  pathParameterA0AA.subpath->subpath.reset(
      new ConfigurationPath("parameter_aaa", false));
  ConfigurationPath pathParameterA0AB(pathScopeA0A);
  pathParameterA0AB.subpath->subpath.reset(
      new ConfigurationPath("parameter_aab", false));
  ConfigurationPath pathScopeA0B(pathScopeA0);
  pathScopeA0B.subpath.reset(new ConfigurationPath("scope_ab", true));
  ConfigurationPath pathParameterA0BA(pathScopeA0B);
  pathParameterA0BA.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));
  ConfigurationPath pathScopeA0B0(pathScopeA0);
  pathScopeA0B0.subpath.reset(new ConfigurationPath("scope_ab", (size_t)0));
  ConfigurationPath pathScopeA0B1(pathScopeA0);
  pathScopeA0B1.subpath.reset(new ConfigurationPath("scope_ab", (size_t)1));
  ConfigurationPath pathParameterA0B0A(pathScopeA0B0);
  pathParameterA0B0A.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));
  ConfigurationPath pathParameterA0B1A(pathScopeA0B1);
  pathParameterA0B1A.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));

  ConfigurationPath pathScopeA1("scope_a", (size_t)1);
  ConfigurationPath pathParameterA1A(pathScopeA1);
  pathParameterA1A.subpath.reset(new ConfigurationPath("parameter_aa", false));
  ConfigurationPath pathParameterA1B(pathScopeA1);
  pathParameterA1B.subpath.reset(new ConfigurationPath("parameter_ab", false));
  ConfigurationPath pathScopeA1A(pathScopeA1);
  pathScopeA1A.subpath.reset(new ConfigurationPath("scope_aa", true));
  ConfigurationPath pathParameterA1AA(pathScopeA1A);
  pathParameterA1AA.subpath->subpath.reset(
      new ConfigurationPath("parameter_aaa", false));
  ConfigurationPath pathParameterA1AB(pathScopeA1A);
  pathParameterA1AB.subpath->subpath.reset(
      new ConfigurationPath("parameter_aab", false));
  ConfigurationPath pathScopeA1B(pathScopeA1);
  pathScopeA1B.subpath.reset(new ConfigurationPath("scope_ab", true));
  ConfigurationPath pathParameterA1BA(pathScopeA1B);
  pathParameterA1BA.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));
  ConfigurationPath pathScopeA1B0(pathScopeA1);
  pathScopeA1B0.subpath.reset(new ConfigurationPath("scope_ab", (size_t)0));
  ConfigurationPath pathScopeA1B1(pathScopeA1);
  pathScopeA1B1.subpath.reset(new ConfigurationPath("scope_ab", (size_t)1));
  ConfigurationPath pathParameterA1B0A(pathScopeA1B0);
  pathParameterA1B0A.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));
  ConfigurationPath pathParameterA1B1A(pathScopeA1B1);
  pathParameterA1B1A.subpath->subpath.reset(
      new ConfigurationPath("parameter_aba", false));

  mockScopeSizerResult = 3;
  config.instantiateScope("scope_c");
  ConfigurationPath pathScopeC0("scope_c", (size_t)0);
  ConfigurationPath pathScopeC1("scope_c", (size_t)1);
  ConfigurationPath pathScopeC2("scope_c", (size_t)2);

  ConcordConfiguration& scopeC1 = config.subscope("scope_c", 1);
  scopeC1.declareScope("scope_ca", "A description.", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeC1A(pathScopeC1);
  pathScopeC1A.subpath.reset(new ConfigurationPath("scope_ca", true));
  scopeC1.declareScope("scope_cb", "A description.", mockScopeSizer, nullptr);
  ConfigurationPath pathScopeC1B(pathScopeC1);
  pathScopeC1B.subpath.reset(new ConfigurationPath("scope_cb", true));
  scopeC1.declareParameter("parameter_ca", "A description.");
  ConfigurationPath pathParameterC1A(pathScopeC1);
  pathParameterC1A.subpath.reset(new ConfigurationPath("parameter_ca", false));

  scopeC1.instantiateScope("scope_cb");
  ConfigurationPath pathScopeC1B0(pathScopeC1);
  pathScopeC1B0.subpath.reset(new ConfigurationPath("scope_cb", (size_t)0));
  ConcordConfiguration& scopeC1B0 = scopeC1.subscope("scope_cb", 0);
  scopeC1B0.declareParameter("parameter_cba", "A description.");
  ConfigurationPath pathParameterC1B0A(pathScopeC1B0);
  pathParameterC1B0A.subpath->subpath.reset(
      new ConfigurationPath("parameter_cba", false));
  scopeC1B0.declareParameter("parameter_cbb", "A description.");
  ConfigurationPath pathParameterC1B0B(pathScopeC1B0);
  pathParameterC1B0B.subpath->subpath.reset(
      new ConfigurationPath("parameter_cbb", false));

  ConfigurationPath pathScopeC1B1(pathScopeC1);
  pathScopeC1B1.subpath.reset(new ConfigurationPath("scope_cb", (size_t)1));
  ConcordConfiguration& scopeC1B1 = scopeC1.subscope("scope_cb", 1);
  scopeC1B1.declareParameter("parameter_cba", "A description.");
  ConfigurationPath pathParameterC1B1A(pathScopeC1B1);
  pathParameterC1B1A.subpath->subpath.reset(
      new ConfigurationPath("parameter_cba", false));

  ConfigurationPath pathScopeC1B2(pathScopeC1);
  pathScopeC1B2.subpath.reset(new ConfigurationPath("scope_cb", (size_t)2));

  ConcordConfiguration& scopeC2 = config.subscope("scope_c", 2);
  scopeC2.declareParameter("parameter_ca", "A description.");
  ConfigurationPath pathParameterC2A(pathScopeC2);
  pathParameterC2A.subpath.reset(new ConfigurationPath("parameter_ca", false));
  scopeC2.declareParameter("parameter_cb", "A description.");
  ConfigurationPath pathParameterC2B(pathScopeC2);
  pathParameterC2B.subpath.reset(new ConfigurationPath("parameter_cb", false));

  ConcordConfiguration::IteratorFeatureSelection
      iterateNeitherParametersNorScopes =
          ConcordConfiguration::kIterateAll &
          (~ConcordConfiguration::kTraverseParameters) &
          (~ConcordConfiguration::kTraverseScopes);
  EXPECT_EQ(config.begin(iterateNeitherParametersNorScopes),
            config.end(iterateNeitherParametersNorScopes))
      << "When requesting iterators that return neither parameters nor scopes, "
         "ConcordConifugration iterators to the beginning and end of a "
         "configuration are not equal, even though such iterators should have "
         "nothing to return.";
  ConcordConfiguration::IteratorFeatureSelection
      iterateNeitherParametersNorTemplatesNorInstances =
          ConcordConfiguration::kIterateAll &
          (~ConcordConfiguration::kTraverseParameters) &
          (~ConcordConfiguration::kTraverseTemplates) &
          (~ConcordConfiguration::kTraverseInstances);
  EXPECT_EQ(config.begin(iterateNeitherParametersNorTemplatesNorInstances),
            config.end(iterateNeitherParametersNorTemplatesNorInstances))
      << "When requesting iterators that return neither parameters nor the "
         "contents of scope templates or scope instances, ConcordConfiguration "
         "iterators to the beginning and end of a configuration are not equal, "
         "even though such iterators should have nothing to return.";

  std::unordered_set<ConfigurationPath> expected(
      {pathParameterA, pathParameterB, pathParameterC});
  auto iterator =
      config.begin(ConcordConfiguration::kIterateTopLevelParameters);
  auto end = config.end(ConcordConfiguration::kIterateTopLevelParameters);
  while (iterator != end) {
    EXPECT_GT(expected.count(*iterator), 0)
        << "Non-recursive ConcordConfiguration iterator over parameters "
           "returned an unexpected element.";
    expected.erase(*iterator);
    ++iterator;
  }
  EXPECT_EQ(expected.size(), 0)
      << "Non-recursive ConcordConfiguration iterator over parameters failed "
         "to return all parameters.";

  expected = std::unordered_set<ConfigurationPath>(
      {pathScopeA, pathScopeB, pathScopeC});
  iterator = config.begin(ConcordConfiguration::kIterateTopLevelScopeTemplates);
  end = config.end(ConcordConfiguration::kIterateTopLevelScopeTemplates);
  while (iterator != end) {
    EXPECT_GT(expected.count(*iterator), 0)
        << "Non-recursive ConcordConfiguration iterator over scope templates "
           "returned an unexpected element.";
    expected.erase(*iterator);
    ++iterator;
  }
  EXPECT_EQ(expected.size(), 0)
      << "Non-recursive ConcordConfiguration iterator over scope templates "
         "failed to return all scopes.";

  expected = std::unordered_set<ConfigurationPath>(
      {pathScopeA0, pathScopeA1, pathScopeC0, pathScopeC1, pathScopeC2});
  iterator = config.begin(ConcordConfiguration::kIterateTopLevelScopeInstances);
  end = config.end(ConcordConfiguration::kIterateTopLevelScopeInstances);
  while (iterator != end) {
    EXPECT_GT(expected.count(*iterator), 0)
        << "Non-recursive ConcordConfiguration iterator over scope instances "
           "returned an unexpected element.";
    expected.erase(*iterator);
    ++iterator;
  }
  EXPECT_EQ(expected.size(), 0)
      << "Non-recursive ConcordConfiguration iterator over scope instances "
         "failed to return all scopes.";

  expected = std::unordered_set<ConfigurationPath>(
      {pathParameterA, pathParameterB, pathParameterC, pathParameterAA,
       pathParameterAB, pathParameterAAA, pathParameterAAB, pathParameterABA,
       pathParameterBA, pathParameterBB, pathParameterBC});
  iterator = config.begin(ConcordConfiguration::kIterateAllTemplateParameters);
  end = config.end(ConcordConfiguration::kIterateAllTemplateParameters);
  while (iterator != end) {
    EXPECT_GT(expected.count(*iterator), 0)
        << "ConcordConfiguration iterator over the template contents of a "
           "configuration returned an unexpected element.";
    expected.erase(*iterator);
    ++iterator;
  }
  EXPECT_EQ(expected.size(), 0)
      << "ConcordConfiguration iterator over scope the template contents of a "
         "configuration failed to return all expected contents.";

  expected = std::unordered_set<ConfigurationPath>(
      {pathParameterA, pathParameterB, pathParameterC, pathParameterA0A,
       pathParameterA0B, pathParameterA1A, pathParameterA1B, pathParameterA0B0A,
       pathParameterA0B1A, pathParameterA1B0A, pathParameterA1B1A,
       pathParameterC1A, pathParameterC1B0A, pathParameterC1B0B,
       pathParameterC1B1A, pathParameterC2A, pathParameterC2B});
  iterator = config.begin(ConcordConfiguration::kIterateAllInstanceParameters);
  end = config.end(ConcordConfiguration::kIterateAllInstanceParameters);
  while (iterator != end) {
    EXPECT_GT(expected.count(*iterator), 0)
        << "ConcordConfiguration iterator over the instance contents of a "
           "configuration returned an unexpected element.";
    expected.erase(*iterator);
    ++iterator;
  }
  EXPECT_EQ(expected.size(), 0)
      << "ConcordConfiguration iterator over scope the instance contents of a "
         "configuration failed to return all expected contents.";

  expected = std::unordered_set<ConfigurationPath>(
      {pathParameterA,     pathParameterB,     pathParameterC,
       pathScopeA,         pathScopeB,         pathScopeC,
       pathScopeAA,        pathScopeAB,        pathParameterAA,
       pathParameterAB,    pathParameterAAA,   pathParameterAAB,
       pathParameterABA,   pathParameterBA,    pathParameterBB,
       pathParameterBC,    pathScopeAB0,       pathScopeAB1,
       pathParameterAB0A,  pathParameterAB1A,  pathScopeA0,
       pathParameterA0A,   pathParameterA0B,   pathScopeA0A,
       pathParameterA0AA,  pathParameterA0AB,  pathScopeA0B,
       pathParameterA0BA,  pathScopeA0B0,      pathScopeA0B1,
       pathParameterA0B0A, pathParameterA0B1A, pathScopeA1,
       pathParameterA1A,   pathParameterA1B,   pathScopeA1A,
       pathParameterA1AA,  pathParameterA1AB,  pathScopeA1B,
       pathParameterA1BA,  pathScopeA1B0,      pathScopeA1B1,
       pathParameterA1B0A, pathParameterA1B1A, pathScopeC0,
       pathScopeC1,        pathScopeC2,        pathScopeC1A,
       pathScopeC1B,       pathParameterC1A,   pathScopeC1B0,
       pathParameterC1B0A, pathParameterC1B0B, pathScopeC1B1,
       pathParameterC1B1A, pathScopeC1B2,      pathParameterC2A,
       pathParameterC2B});
  iterator = config.begin(ConcordConfiguration::kIterateAll);
  end = config.end(ConcordConfiguration::kIterateAll);
  while (iterator != end) {
    EXPECT_GT(expected.count(*iterator), 0)
        << "ConcordConfiguration iterator over the entire contents of a "
           "configuration returned an unexpected element.";
    expected.erase(*iterator);
    ++iterator;
  }
  EXPECT_EQ(expected.size(), 0)
      << "ConcordConfiguration iterator over scope the entire contents of a "
         "configuration failed to return all expected contents.\n ";

  iterator = config.begin();
  auto postfixIterator = config.begin();
  end = config.end();
  while (!(iterator == end)) {
    EXPECT_EQ(iterator, postfixIterator)
        << "Prefix and postfix increment operators advance "
           "ConcordConfiguration iterators in inconsistent ways.";
    ++iterator;
    postfixIterator++;
  }

  end = config.end();
  try {
    ++end;
    FAIL() << "ConcordConfiguration iterators fail to throw an exception when "
              "attempting to advance an iterator past the end of the "
              "configuration.";
  } catch (std::out_of_range e) {
  }

  ConcordConfiguration* deletedConfig = new ConcordConfiguration();
  iterator = deletedConfig->begin();
  delete deletedConfig;
  try {
    ++iterator;
    FAIL() << "ConcordConfiguration' destructor fails to invalidate iterators "
              "over the modified configuration.";
  } catch (InvalidIteratorException e) {
  }

  ConcordConfiguration otherConfig;
  iterator = otherConfig.begin();
  otherConfig = config;
  try {
    ++iterator;
    FAIL() << "ConcordConfiguration' copy assignment operator fails to "
              "invalidate iterators over the modified configuration.";
  } catch (InvalidIteratorException e) {
  }

  iterator = config.begin();
  config.declareScope("scope_d", "A description", mockScopeSizer, nullptr);
  try {
    ++iterator;
    FAIL() << "ConcordConfiguration::declareScope fails to invalidate "
              "iterators over the modified configuration.";
  } catch (InvalidIteratorException e) {
  }

  iterator = config.begin();
  mockScopeSizerResult = 4;
  config.instantiateScope("scope_d");
  try {
    ++iterator;
    FAIL() << "ConcordConfiguration::instantiateScope fails to invalidate "
              "iterators over the modified configuration.";
  } catch (InvalidIteratorException e) {
  }

  iterator = config.begin();
  config.declareParameter("parameter_d", "A description.");
  try {
    ++iterator;
    FAIL() << "ConcordConfiguration::declareParameter fails to invalidate "
              "iterators over the modified configuration.";
  } catch (InvalidIteratorException e) {
  }

  iterator = config.begin();
  config.clear();
  try {
    ++iterator;
    FAIL() << "ConcordConfiguration::clear fails to invalidate iterators over "
              "the modified configuration.";
  } catch (InvalidIteratorException e) {
  }
}

static bool relevantParameterSelector(const ConcordConfiguration& config,
                                      const ConfigurationPath& path,
                                      void* state) {
  if (state) {
    bool* wasCalled = static_cast<bool*>(state);
    *wasCalled = true;
  }

  if (!config.contains(path)) {
    return false;
  }
  const ConcordConfiguration* containingScope = &(config);
  if (path.isScope && path.subpath) {
    containingScope = &(config.subscope(path.trimLeaf()));
  }
  return containingScope->isTagged(path.getLeaf().name, "relevant");
}

TEST(config_test, parameter_selection) {
  ConcordConfiguration config;

  std::vector<std::string> tags({"relevant"});

  config.declareParameter("parameter_a", "A description.");
  config.declareParameter("parameter_b", "A description.");
  config.declareParameter("parameter_c", "A description.");
  config.declareScope("scope_a", "A description.", mockScopeSizer, nullptr);
  config.declareScope("scope_b", "A description.", mockScopeSizer, nullptr);
  ConcordConfiguration& scopeA = config.subscope("scope_a");
  scopeA.declareParameter("parameter_aa", "A description.");
  scopeA.declareParameter("parameter_ab", "A description.");
  ConcordConfiguration& scopeB = config.subscope("scope_b");
  scopeB.declareParameter("parameter_ba", "A description.");
  scopeB.declareParameter("parameter_bb", "A description.");

  config.tagParameter("parameter_c", tags);
  scopeA.tagParameter("parameter_aa", tags);
  scopeB.tagParameter("parameter_bb", tags);
  mockScopeSizerResult = 3;
  config.instantiateScope("scope_b");

  bool selectorCalled = false;
  ParameterSelection selection(config, relevantParameterSelector,
                               &selectorCalled);

  auto iterator = config.begin(ConcordConfiguration::kIterateAllParameters);
  auto end = config.end(ConcordConfiguration::kIterateAllParameters);
  std::unordered_set<ConfigurationPath> expectedSelection;
  while (iterator != end) {
    ConfigurationPath path = *iterator;

    ConcordConfiguration* containingScope = &config;
    if (path.isScope && path.subpath) {
      containingScope = &(config.subscope(path.trimLeaf()));
    }
    bool isRelevant =
        containingScope->isTagged(path.getLeaf().name, "relevant");
    if (isRelevant) {
      expectedSelection.insert(path);
    }

    selectorCalled = false;
    EXPECT_EQ(isRelevant, selection.contains(path))
        << "ParameterSelection::contains fails to correctly identify a "
           "parameter in the selection.";
    EXPECT_TRUE(selectorCalled)
        << "ParameterSelection does not appear to correctly use the selection "
           "function it was constructed with.";

    ++iterator;
  }

  auto selectionIterator = selection.begin();
  auto selectionEnd = selection.end();
  std::unordered_set<ConfigurationPath> observedSelection;
  while (selectionIterator != selectionEnd) {
    EXPECT_LE(observedSelection.count(*selectionIterator), 0)
        << "Iteration through a ParameterSelection includes duplicate values.";
    observedSelection.insert(*selectionIterator);
    ++selectionIterator;
  }

  EXPECT_EQ(expectedSelection, observedSelection)
      << "Iteration through a ParameterSelection does not yield the expected "
         "set of parameters.";
}

}  // end namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
