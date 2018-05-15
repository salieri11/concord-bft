// Copyright 2018 VMware, all rights reserved

#ifndef HEXTOOLS_H
#define HEXTOOLS_H

#include <stdio.h>
#include <string>

#include "slice.h"

using namespace Blockchain;

uint8_t hexToNumeral(char _c);

char* numeralToHex(uint8_t _b);

char* hexStringToBinary(std::string _s);

std::string binaryToHexString(uint8_t *_b, size_t _sz);

std::string sliceToString(const Slice &_s);

#endif
