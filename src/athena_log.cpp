// Copyright 2018 VMware, all rights reserved
//
// Logging utilities

#include "athena_log.hpp"

// Print <size> bytes from <data> to <s> as their 0x<hex> representation.
std::ostream& com::vmware::athena::hexPrint(
   std::ostream &s, const uint8_t *data, size_t size)
{
   s << "0x";
   for (size_t i = 0; i < size; i++) {
      s << std::hex << std::setw(2) << std::setfill('0') << (uint)data[i];
   }
   return s;
};

// Print a vector of bytes as its 0x<hex> representation.
std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, HexPrintVector v)
{
   return hexPrint(s, &v.vec[0], v.vec.size());
};

// Print an evm_address as its 0x<hex> representation.
std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, const evm_address &a)
{
   return hexPrint(s, a.bytes, sizeof(evm_address));
};

// Print an evm_uint256be as its 0x<hex> representation.
std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, const evm_uint256be &u)
{
   return hexPrint(s, u.bytes, sizeof(evm_uint256be));
};
