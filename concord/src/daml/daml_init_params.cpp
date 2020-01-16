#include "daml_init_params.hpp"
#include <fstream>
#include <nlohmann/json.hpp>

namespace {
const std::string kAdminAuthenticationKey = "admin_authentication_key";
}

namespace concord {
namespace daml {

DamlInitParams::DamlInitParams(const std::string& genesis_file_path) {
  std::ifstream genesis_stream(genesis_file_path);
  nlohmann::json genesis_block;
  if (genesis_stream.good()) {
    genesis_stream >> genesis_block;
  } else {
    throw DamlInitParamException("Unable to read genesis file at: " +
                                 genesis_file_path);
  }

  if (genesis_block.find(kAdminAuthenticationKey) != genesis_block.end()) {
    admin_authentication_key_ =
        genesis_block[kAdminAuthenticationKey].get<std::string>();
  } else {
    throw DamlInitParamException("Key " + kAdminAuthenticationKey +
                                 " is absent in " + genesis_file_path);
  }
}

}  // namespace daml
}  // namespace concord
