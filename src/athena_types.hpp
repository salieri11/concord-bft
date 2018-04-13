// Copyright 2018 VMware, all rights reserved
//
// Common types passed among Athena components.

#ifndef ATHENA_TYPES_HPP
#define ATHENA_TYPES_HPP

#include "athena_log.hpp"

namespace com {
namespace vmware {
namespace athena {

typedef struct EthTransaction {
   uint64_t nonce;
   // TODO: block info
   std::vector<uint8_t> from;
   std::vector<uint8_t> to;
   std::vector<uint8_t> contract_address;
   std::vector<uint8_t> input;
   evm_status_code status;
   // TODO: all the other fields

   EthTransaction()
      : nonce(0), from(), to(), contract_address(), input(),
        status(EVM_INTERNAL_ERROR) { }

   EthTransaction(const EthTransaction &other);
   EthTransaction(const uint64_t nonce_,
                  const std::vector<uint8_t> from_,
                  const std::vector<uint8_t> to_,
                  const std::vector<uint8_t> contract_address_,
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

// Byte-wise comparator for evm_uint256be. This allows us to use this type as a
// key in a std::map. Must be in the global namespace.
bool operator<(const evm_uint256be &a, const evm_uint256be &b);

#endif
