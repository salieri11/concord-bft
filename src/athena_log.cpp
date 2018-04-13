// Copyright 2018 VMware, all rights reserved
//
// Logging utilities

#include "athena_log.hpp"
#include "evm.h"
#include <ios>

std::ostream& com::vmware::athena::hexPrint(
   std::ostream &s, const uint8_t *data, size_t size)
{
   // Store current state of ostream flags
   std::ios::fmtflags f(s.flags());
   s << "0x";
   for (size_t i = 0; i < size; i++) {
      s << std::hex << std::setw(2) << std::setfill('0') << (uint)data[i];
   }
   // restore current state
   s.flags(f);
   return s;
};

std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, HexPrintVector v)
{
   return hexPrint(s, &v.vec[0], v.vec.size());
};

std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, HexPrintAddress a)
{
   return hexPrint(s, &a.addr->bytes[0], sizeof(evm_address));
};

std::ostream& com::vmware::athena::operator<<(
   std::ostream& s, HexPrintUint256Be u)
{
   return hexPrint(s, &u.uibe->bytes[0], sizeof(evm_uint256be));
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
   s << "\nMessage: {\ndestination: " << HexPrintAddress{&msg.destination}
   << "\nsender: " << HexPrintAddress{&msg.sender}
   << "\nether: " << HexPrintUint256Be{&msg.value}
   << "\ncall_kind: " << msg.kind
   << "\ndepth: " << msg.depth
   << "\ngas: " << msg.gas
   << "\ninput size: " << msg.input_size;
   s << "\n}\n";
   return s;
};
