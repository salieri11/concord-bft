// Copyright 2018 VMware, all rights reserved
//
// Logging utilities

// TODO: We may want "release" versions of these macros that print fewer bytes.

#include <ostream>
#include <vector>
#include "evm.h"

namespace com {
namespace vmware {
namespace athena {


struct HexPrintVector {
   std::vector<uint8_t> const& vec;
};
struct HexPrintAddress {
   evm_address const *addr;
};
struct HexPrintUint256Be {
   evm_uint256be const *uibe;
};

std::ostream& hexPrint(std::ostream &s, const uint8_t *data, size_t size) {
   s << "0x";
   for (size_t i = 0; i < size; i++) {
      s << std::hex << std::setw(2) << std::setfill('0') << (uint)data[i];
   }
   return s;
};

std::ostream& operator<<(std::ostream& s, HexPrintVector v) {
   return hexPrint(s, &v.vec[0], v.vec.size());
};
std::ostream& operator<<(std::ostream& s, HexPrintAddress a) {
   return hexPrint(s, &a.addr->bytes[0], sizeof(evm_address));
};
std::ostream& operator<<(std::ostream& s, HexPrintUint256Be u) {
   return hexPrint(s, &u.uibe->bytes[0], sizeof(evm_uint256be));
};

}
}
}
