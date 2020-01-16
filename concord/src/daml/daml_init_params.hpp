#ifndef DAML_INIT_PARAMS_HPP
#define DAML_INIT_PARAMS_HPP

#include <stdexcept>

#include <string>

namespace concord {
namespace daml {

class DamlInitParamException : public std::runtime_error {
 public:
  explicit DamlInitParamException(const std::string& what)
      : std::runtime_error(what){};
  explicit DamlInitParamException(const char* what)
      : std::runtime_error(what){};
};

class DamlInitParams {
 public:
  explicit DamlInitParams(const std::string& genesis_file_path);
  std::string get_admin_authentication_key() const {
    return admin_authentication_key_;
  };

 private:
  std::string admin_authentication_key_;
};

}  // namespace daml
}  // namespace concord

#endif /* end of include guard: DAML_INIT_PARAMS_HPP */
