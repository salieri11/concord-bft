// Copyright 2018 VMware, all rights reserved
//
// concord common Utilities.

#ifndef CONCORD_UTILS_HPP
#define CONCORD_UTILS_HPP

#include <evmjit.h>
#include <log4cplus/loggingmacros.h>
#include <chrono>
#include <fstream>
#include <iostream>
#include "json.hpp"

namespace com {
namespace vmware {
namespace concord {

std::vector<uint8_t> dehex(const std::string &str);

void to_evm_uint256be(uint64_t val, evm_uint256be *ret);

uint64_t from_evm_uint256be(const evm_uint256be *val);

int64_t get_epoch_millis();

std::ostream &hexPrint(std::ostream &s, const uint8_t *data, size_t size);

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif  // CONCORD_UTILS_HPP
