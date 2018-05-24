// Copyright 2018 VMware, all rights reserved
//
// TODO(BWF): we have so many reimplementations of this; consolidate

#include <stdexcept>

#include "HexTools.h"
#include "slice.h"

using namespace Blockchain;

uint8_t hexToNumeral(char _c)
{
   if (_c >= '0' && _c <= '9') {
      return _c - '0';
   } else if (_c >= 'A' && _c <= 'F') {
      return _c - 'A' + 10;
   } else if (_c >= 'a' && _c <= 'f') {
      return _c - 'a' + 10;
   } else {
      throw std::invalid_argument("Invalid input string");
   }
}

char* numeralToHex(uint8_t _b)
{
   char *s = new char[3]; // LEAKS!
   snprintf(s, 3, "%02x", _b & 0xff);
   return s;
}

char* hexStringToBinary(std::string _s)
{
   // Every letter is two bytes.
   uint8_t *letter = (uint8_t*)malloc(_s.size() * sizeof(char));
   size_t posAtBinary = 0;
   for (std::string::size_type strIdx = 0;
        strIdx + 1 < _s.size();
        strIdx += 2) {
      char c1 = _s[strIdx];
      char c2 = _s[strIdx + 1];
      unsigned char msb = hexToNumeral(c1);
      unsigned char lsb = hexToNumeral(c2);

      letter[posAtBinary] = msb * 0x10 + lsb;

      ++posAtBinary;
   }

   return (char*)letter;
}

std::string binaryToHexString(uint8_t *_b, size_t _sz)
{
   std::string s = std::string(2 * _sz, '\0');
   for (size_t i = 0; i < _sz; ++i) {
      char *hex = numeralToHex(_b[i]);
      s.replace(2*i, 2, hex);
      delete hex;
   }

   return s;
}

std::string sliceToString(const Slice &_s)
{
   return binaryToHexString((uint8_t*)_s.data(), _s.size());
}
