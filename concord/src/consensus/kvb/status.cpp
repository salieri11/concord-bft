// Copyright (c) 2018 VMware. All rights reserved.

#include "status.hpp"

#include <ostream>

namespace concord {
namespace consensus {

std::ostream& operator<<(std::ostream& s, Status const& status) {
  return status.operator<<(s);
}

}  // namespace consensus
}  // namespace concord
