// Copyright 2018 VMware, all rights reserved
//
// Common types passed among Athena components.

#include "athena_types.hpp"

com::vmware::athena::EthTransaction::EthTransaction(
   const EthTransaction &other)
   : nonce(other.nonce),
     from(other.from),
     to(other.to),
     contract_address(other.contract_address),
     input(other.input),
     status(other.status)
{
}

com::vmware::athena::EthTransaction&
com::vmware::athena::EthTransaction::operator=(
   const EthTransaction &other)
{
   nonce = other.nonce;
   from = other.from;
   to = other.to;
   contract_address = other.contract_address;
   input = other.input;
   status = other.status;
   return *this;
}

// Byte-wise comparator for evm_uint256be. This allows us to use this type as a
// key in a std::map. Must be in the global namespace.
bool operator<(const evm_uint256be &a, const evm_uint256be &b)
{
   for (int i = 0; i < sizeof(evm_uint256be); ++i) {
      if (a.bytes[i] < b.bytes[i]) {
         return true;
      } else if (a.bytes[i] > b.bytes[i]) {
         return false;
      }
   }

   return false;
}

// Byte-wise comparator for evm_address. This allows us to use this type as a
// key in a std::map. Must be in the global namespace.
bool operator<(const evm_address &a, const evm_address &b)
{
   for (int i = 0; i < sizeof(evm_address); ++i) {
      if (a.bytes[i] < b.bytes[i]) {
         return true;
      } else if (a.bytes[i] > b.bytes[i]) {
         return false;
      }
   }

   return false;
}
