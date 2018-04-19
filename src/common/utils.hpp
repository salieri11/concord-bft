// Copyright 2018 VMware, all rights reserved
//
// Athena common Utilities.

#ifndef ATHENA_UTILS_HPP
#define ATHENA_UTILS_HPP

#include <log4cplus/loggingmacros.h>
#include <fstream>
#include <iostream>
#include <evmjit.h>
#include "json.hpp"

namespace com {
namespace vmware {
namespace athena {

std::vector<uint8_t> dehex(const std::string &str);

void to_evm_uint256be(uint64_t val, evm_uint256be *ret);

uint64_t from_evm_uint256be(const evm_uint256be *val);

}
}
}

#endif //ATHENA_UTILS_HPP
