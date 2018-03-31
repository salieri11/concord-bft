// Copyright 2018 VMware, all rights reserved
//
// Logging utilities

#ifndef ATHENA_LOG_HPP
#define ATHENA_LOG_HPP

// TODO: We may want "release" versions of these macros that print fewer bytes.

#include <ostream>
#include <iomanip>
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

std::ostream& hexPrint(std::ostream &s, const uint8_t *data, size_t size);

std::ostream& operator<<(std::ostream& s, HexPrintVector v);
std::ostream& operator<<(std::ostream& s, HexPrintAddress a);
std::ostream& operator<<(std::ostream& s, HexPrintUint256Be u);

}
}
}

#endif //ATHENA_LOG_HPP
