// Copyright 2018 VMware, all rights reserved
//
// Common types passed among Athena components.

#ifndef ATHENA_TYPES_HPP
#define ATHENA_TYPES_HPP

#include "athena_log.hpp"
#include "athena_types.hpp"
#include "kvb/slice.h"

const evm_address zero_address{{0}};
const evm_uint256be zero_hash{{0}};

const int64_t tx_storage_version = 1;

namespace com {
namespace vmware {
namespace athena {

typedef struct EthTransaction {
   uint64_t nonce;
   // TODO: block info
   evm_address from;
   evm_address to;
   evm_address contract_address;
   std::vector<uint8_t> input;
   evm_status_code status;
   uint64_t value;
   // TODO: all the other fields

   evm_uint256be hash();
   void serialize(std::string &out);
   static struct EthTransaction deserialize(Blockchain::Slice &input);
} EthTransaction;

typedef struct EthBlock {
   uint64_t number;
   evm_uint256be hash;
   evm_uint256be parent_hash;
   std::vector<evm_uint256be> transactions;
} EthBlock;

}
}
}

// Byte-wise comparators for evm_uint256be and evm_address. This allows us to
// use these types as keys in a std::map. Must be in the global namespace.
bool operator<(const evm_uint256be &a, const evm_uint256be &b);
bool operator<(const evm_address &a, const evm_address &b);
bool operator!=(const evm_address &a, const evm_address &b);
bool operator==(const evm_address &a, const evm_address &b);

#endif
