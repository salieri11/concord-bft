// Copyright 2018 VMware, all rights reserved

#ifndef HEXTOOLS_H
#define HEXTOOLS_H

#include <stdio.h>
#include <string>

std::ostream &hexPrint(std::ostream &s, const uint8_t *data, size_t size);

#endif
