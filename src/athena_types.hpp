// Copyright 2018 VMware, all rights reserved
//
// Common types passed among Athena components.

#ifndef ATHENA_TYPES_HPP
#define ATHENA_TYPES_HPP

#include "athena_log.hpp"

const evm_address zero_address{{0}};

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
   // TODO: all the other fields

   EthTransaction()
      : nonce(0), from(), to(), contract_address(), input(),
        status(EVM_INTERNAL_ERROR) { }

   EthTransaction(const EthTransaction &other);
   EthTransaction(const uint64_t nonce_,
                  const evm_address from_,
                  const evm_address to_,
                  const evm_address contract_address_,
                  const std::vector<uint8_t> input_,
                  const evm_status_code status_)
      : nonce(nonce_),
        from(from_),
        to(to_),
        contract_address(contract_address_),
        input(input_),
        status(status_) { }
   EthTransaction& operator=(const EthTransaction &other);
} EthTransaction;

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
