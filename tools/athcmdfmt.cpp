// Copyright 2018 VMware, all rights reserved
//
// Formatting utilities for command line tools.

#include <ostream>
#include <assert.h>
#include "athcmdex.hpp"
#include "athcmdfmt.hpp"

char hexval(char c) {
   if (c >= '0' && c <= '9') {
      return c - '0';
   } else if (c >= 'a' && c <= 'f') {
      return 10 + c - 'a';
   } else if (c >= 'A' && c <= 'F') {
      return 10 + c - 'A';
   } else {
      throw AthCmdException("non-hex character");
   }
}

/**
 * Convert a 0x-hex string to plain bytes. 0x prefix is optional.
 *   "0x1234" -> {18, 52}
 */
void dehex0x(const std::string &str, std::string &bin /* out */) {
   if (str.size() % 2 != 0) {
      throw AthCmdException("nibble missing in string");
   }

   // allow people to include "0x" prefix, or not
   size_t adjust = (str[0] == '0' && str[1] == 'x') ? 2 : 0;

   size_t binsize = (str.size()-adjust)/2;

   if (binsize > 0) {
      bin.resize(binsize);
      for (int i = 0; i < binsize; i++) {
	 bin[i] = (hexval(str[i*2+adjust]) << 4)
            | hexval(str[i*2+adjust+1]);
      }
   } else {
      bin.assign("");
   }
}

char hexchar(char c) {
   assert(c < 16);
   return "0123456789abcdef"[c];
}

/**
 * Convert a vector of bytes into a 0x-hex string.
 *   {18, 52} -> "0x1234"
 */
void hex0x(const std::string &in, std::string &out /* out */) {
   out.assign("0x");
   for (auto s = in.begin(); s < in.end(); s++) {
      out.push_back(hexchar(((*s) >> 4) & 0xf));
      out.push_back(hexchar((*s) & 0xf));
   }
}
