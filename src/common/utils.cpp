// Copyright 2018 VMware, all rights reserved
//
// Athena common Utilities.


#include "utils.hpp"

using namespace std;

char hexval(char c) {
   if (c >= '0' && c <= '9') {
      return c - '0';
   } else if (c >= 'a' && c <= 'f') {
      return 10 + c - 'a';
   } else if (c >= 'A' && c <= 'F') {
      return 10 + c - 'A';
   } else {
      throw "non-hex character";
   }
}

/**
   Converts the given string into a vector of uint8_t
   Every pair of consecutive character is considered as a
   hex number and then that is converted into a uint8_t
   For example. `ABCD` will convert to vector {171, 205}
*/
vector<uint8_t> com::vmware::athena::dehex(const std::string &str) {
   if (str.size() % 2 != 0) {
      throw "nibble missing in string";
   }
   // allow people to include "0x" prefix, or not
   size_t adjust = (str[0] == '0' && str[1] == 'x') ? 2 : 0;
   size_t binsize = (str.size()-adjust)/2;

   vector<uint8_t> ret;

   for (int i = 0; i < binsize; i++) {
      ret.push_back((hexval(str[i*2+adjust]) << 4)
                    | hexval(str[i*2+adjust+1]));
   }
   return ret;
}

/** Converts the given uint64_t into a evm_uint256be type
    The top 24 bytes are always going to be 0 in this conversion
*/
void com::vmware::athena::to_evm_uint256be(uint64_t val, evm_uint256be *ret) {
   uint8_t mask = 0xff;
   for (int i = 0; i < sizeof(evm_uint256be); i++) {
      uint8_t byte = val & mask;
      ret->bytes[sizeof(evm_uint256be) - i - 1] = byte; // big endian order
      val = val >> 8;
   }
}

/** Converts the given evm_uint256be into a uint64_t, if the value of
    @val is more than 2^64 then return value will simply contain the
    lower 8 bytes of @val
*/
uint64_t com::vmware::athena::from_evm_uint256be(const evm_uint256be *val)
{
   const size_t offset = sizeof(evm_uint256be) - sizeof(uint64_t);
   uint64_t ret = 0;
   for (int i = 0; i < sizeof(uint64_t); i++) {
      ret = ret << 8;
      ret |= val->bytes[i+offset];
   }
    return ret;
}
