// Copyright 2018 VMware, all rights reserved
//
// Storage key comparators implementation.

#include <log4cplus/loggingmacros.h>

#include "RocksDBClient.h"
#include "Comparators.h"
#include "slice.h"
#include "BlockchainDBAdapter.h"
#include "HexTools.h"
#include "BlockchainInterfaces.h"

#include <chrono>

using log4cplus::Logger;

/* Comparators start */
namespace Blockchain {

/*
 * If key a is earlier than key b, return a negative number; if larger, return a
 * positive number; if equal, return zero.
 *
 * Comparison is done by decomposed parts. Types are compared first, followed by
 * the application key, and finally the block id. Types and keys are sorted in
 * ascending order, and block IDs are sorted in descending order.
 */
int composedKeyComparison(const Slice& _a, const Slice& _b)
{
   // TODO(BWF): see note about multiple bytes in
   // Blockchain::extractTypeFromKey
   char aType = extractTypeFromKey(_a);
   char bType = extractTypeFromKey(_b);
   if (aType != bType) {
      int ret = aType - bType;
      return ret;
   }

   // if this is a block, we stop with key comparison - it doesn't have a block
   // id component (that would be redundant)
   if (aType == ((char)EDBKeyType::E_DB_KEY_TYPE_BLOCK)) {

      //Extract the block ids to compare so that endianness of environment
      //does not matter.
      BlockId aId = extractBlockIdFromKey(_a);
      BlockId bId = extractBlockIdFromKey(_b);

      if(aId < bId) {
         return -1;
      } else if(bId < aId) {
         return 1;
      } else {
         return 0;
      }
   }

   Slice aKey = extractKeyFromKeyComposedWithBlockId(_a);
   Slice bKey = extractKeyFromKeyComposedWithBlockId(_b);

   int keyComp = aKey.compare(bKey);

   if (keyComp == 0) {
      BlockId aId = extractBlockIdFromKey(_a);
      BlockId bId = extractBlockIdFromKey(_b);

      // within a type+key, block ids are sorted in reverse order
      if(aId < bId) {
         return 1;
      } else if(bId < aId) {
         return -1;
      } else {
         return 0;
      }
   }

   return keyComp;
}

/* RocksDB */
#ifdef USE_ROCKSDB
int RocksKeyComparator::Compare(const rocksdb::Slice& _a,
                                const rocksdb::Slice& _b) const
{
   Slice a = fromRocksdbSlice(_a);
   Slice b = fromRocksdbSlice(_b);
   int ret = composedKeyComparison(a, b);

   Logger logger(Logger::getInstance("com.vmware.athena.kvb"));
   LOG4CPLUS_DEBUG(logger, "Compared " << sliceToString((Slice&)a) <<
                   " with " << sliceToString((Slice&)b) <<
                   ", returning " << ret);

   return ret;
}
#endif

/* In memory */
bool InMemKeyComp(const Slice& _a, const Slice& _b)
{
   int comp = composedKeyComparison(_a, _b);

   Logger logger(Logger::getInstance("com.vmware.athena.kvb"));
   LOG4CPLUS_DEBUG(logger, "Compared " << sliceToString((Slice&)_a) <<
                   " with " << sliceToString((Slice&)_b) <<
                   ", a<b == " << (comp<0));

   // Check: comp < 0 ==> _a < _b
   return comp < 0;
}
}
