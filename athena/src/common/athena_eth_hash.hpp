// Copyright 2018 VMware, all rights reserved
//
// Ethereum Hashing.

#ifndef ATHENA_ETH_HASH_HPP
#define ATHENA_ETH_HASH_HPP

#include <log4cplus/loggingmacros.h>
#include "evm.h"

namespace com {
namespace vmware {
namespace athena {
namespace EthHash {

evm_uint256be keccak_hash(const std::vector<uint8_t> &data);
evm_uint256be keccak_hash(const uint8_t *data, size_t size);

}
}
}
}


#endif
