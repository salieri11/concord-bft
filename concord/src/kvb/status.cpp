// Copyright (c) 2018 VMware. All rights reserved.

#include <ostream>

#include "status.hpp"

std::ostream& Blockchain::operator<<(std::ostream& s,
                                     Blockchain::Status const& status) {
  return status.operator<<(s);
}
