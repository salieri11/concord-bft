// Copyright 2018 VMware, all rights reserved
//
// Logging utilities

#include "concord_log.hpp"

#include <ios>

#include "evm.h"
#include "hex_tools.h"

using concordUtils::hexPrint;

namespace concord {
namespace common {

// Print a vector of bytes as its 0x<hex> representation.
std::ostream& operator<<(std::ostream& s, const HexPrintVector v) {
  return hexPrint(s, reinterpret_cast<const char*>(&v.vec[0]), v.vec.size());
};

// Print a char* of bytes as its 0x<hex> representation.
std::ostream& operator<<(std::ostream& s, const HexPrintBytes p) {
  return hexPrint(s, p.bytes, p.size);
};

// Print an evm_address as its 0x<hex> representation.
std::ostream& operator<<(std::ostream& s, const evm_address& a) {
  return hexPrint(s, reinterpret_cast<const char*>(a.bytes),
                  sizeof(evm_address));
};

// Print an evm_uint256be as its 0x<hex> representation.
std::ostream& operator<<(std::ostream& s, const evm_uint256be& u) {
  return hexPrint(s, reinterpret_cast<const char*>(u.bytes),
                  sizeof(evm_uint256be));
};

std::ostream& operator<<(std::ostream& s, evm_call_kind kind) {
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

std::ostream& operator<<(std::ostream& s, struct evm_message msg) {
  s << "\nMessage: {\ndestination: " << msg.destination
    << "\nsender: " << msg.sender << "\nether: " << msg.value
    << "\ncall_kind: " << msg.kind << "\ndepth: " << msg.depth
    << "\ngas: " << msg.gas << "\ninput size: " << msg.input_size;
  s << "\n}\n";
  return s;
};

}  // namespace common
}  // namespace concord
