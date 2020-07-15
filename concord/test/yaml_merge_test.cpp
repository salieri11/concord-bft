#include "config/configuration_manager.hpp"
#include "gtest/gtest.h"

namespace {

bool yaml_structure_compare(
    const YAML::Node& a, const YAML::Node& b, std::stringstream& log,
    const YAML::Node& parent = YAML::Node(YAML::NodeType::Null),
    std::string path = "") {
  auto logError = [&parent, &path, &log](std::string errorMsg) {
    log << "\n---------------------------------------\n"
        << errorMsg << "\nparent=" << parent << "\npath=" << path
        << "\n---------------------------------------\n";
  };
  if (a.IsMap() && b.IsMap()) {
    bool bEqual = true;
    for (const auto& n : a) {
      if (!b[n.first.as<std::string>()]) {
        logError("key (" + n.first.as<std::string>() +
                 ") in A not found in B!");
        bEqual = false;
      } else {
        if (!yaml_structure_compare(n.second, b[n.first.as<std::string>()], log,
                                    n.first,
                                    path + "/" + n.first.as<std::string>())) {
          bEqual = false;
        }
      }
    }
    for (const auto& n : b) {
      if (!a[n.first.as<std::string>()]) {
        logError("key (" + n.first.as<std::string>() +
                 ") in B not found in A!");
        bEqual = false;
      } else {
        if (!yaml_structure_compare(a[n.first.as<std::string>()], n.second, log,
                                    n.first,
                                    path + "/" + n.first.as<std::string>())) {
          bEqual = false;
        }
      }
    }
    return bEqual;
  } else if (a.IsSequence() && b.IsSequence()) {
    bool bEqual = true;
    if (a.size() != b.size()) {
      logError("Mismatching sizes of sequenceses!");
      bEqual = false;
    }
    auto itA = a.begin();
    auto itB = b.begin();
    int count = 0;
    for (; itA != a.end() && itB != b.end(); ++itA, ++itB, count++) {
      if (!yaml_structure_compare(*itA, *itB, log, parent,
                                  path + "/" + std::to_string(count))) {
        bEqual = false;
      }
    }
    return bEqual;
  } else if (a.IsScalar() && b.IsScalar()) {
    return true;  // we only check structure of the merged, values could be
                  // different
  }
  logError("Mismatching types of nodes!");
  return false;
}

TEST(YamlMergeTest, merge_nodes_test) {
  for (int i = 1; i <= 7; i++) {
    YAML::Node yaml_app;
    YAML::Node yaml_depl;
    YAML::Node yaml_secr;

    YAML::Node yaml_concord;  // the entire configuration extracted from concord
                              // in 1 file (the old way)

    auto app_file = "./resources/yaml_merge/app1.config";
    auto depl_file =
        "./resources/yaml_merge/dep" + std::to_string(i) + ".config";
    auto secr_file =
        "./resources/yaml_merge/sec" + std::to_string(i) + ".config";

    auto entire_file =
        "resources/yaml_merge/concord" + std::to_string(i) + ".config";

    yaml_app.reset(YAML::LoadFile(app_file));
    yaml_depl.reset(YAML::LoadFile(depl_file));
    yaml_secr.reset(YAML::LoadFile(secr_file));

    std::stringstream log;

    log << "app_file=" << app_file << "\n"
        << "depl_file=" << depl_file << "\n"
        << "sec_file=" << secr_file << "\n"
        << "entire_file=" << entire_file << "\n";

    yaml_concord.reset(YAML::LoadFile(entire_file));

    auto yaml_merged =
        YAML::Clone(yaml_merge(yaml_app, yaml_depl, {"principal_id"}));

    yaml_merged =
        YAML::Clone(yaml_merge(yaml_merged, yaml_secr, {"principal_id"}));

    {
      auto merged_file = "./merged" + std::to_string(i) + ".yml";
      std::ofstream f(merged_file);
      f << yaml_merged;
      f.close();
    }
    {
      auto concord_file = "./concord" + std::to_string(i) + ".yml";
      std::ofstream f(concord_file);
      f << yaml_concord;
      f.close();
    }

    log << "--------------------- BEGIN COMPARE ---------------------------"
           "-----------------------------\n";
    auto compare_result =
        yaml_structure_compare(yaml_merged, yaml_concord, log);
    log << "yaml_structure_compare=" << compare_result << "\n"
        << "--------------------- END COMPARE -----------------------------"
           "-----------------------------\n";
    if (!compare_result) {
      std::cout << log.str();
    }
    ASSERT_TRUE(compare_result);
  }
}

}  // namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
