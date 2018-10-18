// Copyright 2018 VMware, all rights reserved
//
// Utilities for altering slices.

#include <log4cplus/loggingmacros.h>

#include "slice.h"

#include <stdio.h>

using log4cplus::Logger;

namespace Blockchain {

Slice append(const Slice &out, const Slice &s)
{
   size_t newSize = out.size() + s.size();
   char* newData = (char*)malloc(newSize);
   memcpy(newData, out.data(), out.size());
   memcpy(newData + out.size(), s.data(), s.size());

   return Slice(newData, newSize);
}

}
