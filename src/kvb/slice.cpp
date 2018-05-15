// Copyright 2018 VMware, all rights reserved
//
// Utilities for altering slices.

#include <log4cplus/loggingmacros.h>

#include "slice.h"

#include <stdio.h>

using log4cplus::Logger;

namespace Blockchain {
bool copyToAndAdvance(char *_buf,
                      size_t *_offset,
                      size_t _maxOffset,
                      char *_src,
                      size_t _srcSize)
{
   if (_buf == NULL) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "_buf is NULL");
      return false;
   }

   if (_offset == NULL) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "_offset is NULL");
      return false;
   }

   if (_src == NULL) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "_src is NULL");
      return false;
   }

   if (*_offset >= _maxOffset && _srcSize > 0) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "offset " << *_offset <<
                      " is past size of buffer " << _maxOffset <<
                      " and copying " << _srcSize << " bytes");
      return false;
   }

   memcpy(_buf + *_offset, _src, _srcSize);
   *_offset += _srcSize;

   return true;
}

bool copyFromAndAdvance(const char *_buf,
                        size_t *_offset,
                        size_t _maxOffset,
                        char *_to,
                        size_t _toSize)
{
   if (_buf == NULL) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "_buf is NULL");
      return false;
   }

   if (_offset == NULL) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "_offset is NULL");
      return false;
   }

   if (_to == NULL) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "_to is NULL, while _toSize is " << _toSize);
      return false;
   }

   if (*_offset >= _maxOffset && _toSize > 0) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "offset " << *_offset <<
                      " is past size of buffer " << _maxOffset <<
                      " and copying " << _toSize << " bytes");
      return false;
   }

   if (_toSize > 0) {
      memcpy(_to, _buf + *_offset, _toSize);
      *_offset += _toSize;
   }

   return true;
}

Slice append(const Slice &out, const Slice &s)
{
   size_t newSize = out.size() + s.size();
   char* newData = (char*)malloc(newSize);
   memcpy(newData, out.data(), out.size());
   memcpy(newData + out.size(), s.data(), s.size());

   return Slice(newData, newSize);
}

}
