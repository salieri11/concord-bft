// Copyright 2018 VMware, all rights reserved

#ifndef HEXTOOLS_H
#define HEXTOOLS_H

#include <stdio.h>
#include <string>

#include "slice.h"

uint8_t hexToNumeral(char _c);

char* numeralToHex(uint8_t _b);

char* hexStringToBinary(std::string _s);

std::string binaryToHexString(uint8_t *_b, size_t _sz);

std::string sliceToString(const Blockchain::Slice &_s);

std::ostream& hexPrint(std::ostream &s, const uint8_t *data, size_t size);

#endif
