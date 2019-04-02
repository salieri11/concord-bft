// Copyright 2018 VMware, all rights reserved
//
// Ethereum Hashing.

#ifndef CONCORD_ETH_HASH_HPP
#define CONCORD_ETH_HASH_HPP

#include <log4cplus/loggingmacros.h>
#include "evm.h"

namespace com {
namespace vmware {
namespace concord {
namespace EthHash {

evm_uint256be keccak_hash(const std::vector<uint8_t> &data);
evm_uint256be keccak_hash(const uint8_t *data, size_t size);

}  // namespace EthHash
}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif
