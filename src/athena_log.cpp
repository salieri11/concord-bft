// Copyright 2018 VMware, all rights reserved
//
// Logging utilities

#include <ios>

#include "athena_log.hpp"
#include "evm.h"
#include "kvb/HexTools.h"

// Print a vector of bytes as its 0x<hex> representation.
std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, const HexPrintVector v)
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


std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, evm_call_kind kind) {
   switch (kind) {
   case EVM_CALL:
      s << "EVM_CALL";
      break;
   case EVM_DELEGATECALL:
      s << "EVM_DELEGATECALL";
      break;
   case EVM_CALLCODE:
      s << "EVM_CALLCODE";
      break;
   case EVM_CREATE:
      s << "EVM_CREATE";
      break;
   }
   return s;
}

std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, struct evm_message msg)
{
   s << "\nMessage: {\ndestination: " << msg.destination
   << "\nsender: " << msg.sender
   << "\nether: " << msg.value
   << "\ncall_kind: " << msg.kind
   << "\ndepth: " << msg.depth
   << "\ngas: " << msg.gas
   << "\ninput size: " << msg.input_size;
   s << "\n}\n";
   return s;
};
