//
// Copyright 2020 VMware, all rights reserved
//

#ifndef UTILS_CONCORD_LOGGING_HPP
#define UTILS_CONCORD_LOGGING_HPP

#include <log4cplus/mdc.h>

namespace concord::utils {
class RAIIMDC {
  std::string key_;

 public:
  RAIIMDC(const std::string& key, const std::string& value) : key_(key) {
    log4cplus::getMDC().put(key_, value);
  }
  ~RAIIMDC() { log4cplus::getMDC().remove(key_); }
};

}  // namespace concord::utils

#endif  // UTILS_CONCORD_LOGGING_HPP
