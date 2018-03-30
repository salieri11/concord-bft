// Copyright 2018 VMware, all rights reserved
//
// Athena common Utilities.

#ifndef ATHENA_UTILS_HPP
#define ATHENA_UTILS_HPP

#include <log4cplus/loggingmacros.h>
#include <fstream>
#include "json.hpp"

namespace com {
namespace vmware {
namespace athena {

nlohmann::json parse_genesis_block(std::string genesis_file_path);

}
}
}

#endif //ATHENA_UTILS_HPP
