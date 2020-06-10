// Copyright 2018 VMware, all rights reserved
//
// Ethereum Hashing.

#ifndef UTILS_CONCORD_ETH_HASH_HPP
#define UTILS_CONCORD_ETH_HASH_HPP

#include "Logger.hpp"
#include "evm.h"

namespace concord {
namespace utils {
namespace eth_hash {

evm_uint256be keccak_hash(const std::vector<uint8_t> &data);
evm_uint256be keccak_hash(const uint8_t *data, size_t size);

}  // namespace eth_hash
}  // namespace utils
}  // namespace concord

#endif  // UTILS_CONCORD_ETH_HASH_HPP
