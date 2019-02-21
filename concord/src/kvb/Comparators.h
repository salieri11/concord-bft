// Copyright 2018 VMware, all rights reserved
//
// Storage key comparators definition.

#ifndef COMPARATORS_H
#define COMPARATORS_H

#ifdef USE_ROCKSDB
#include "rocksdb/comparator.h"
#include "rocksdb/slice.h"
#endif
#include "sliver.hpp"

namespace Blockchain {
/*
 * Basic comparator. Decomposes storage key into parts (type, version,
 * application key).
 */
int composedKeyComparison(const Sliver& _a, const Sliver& _b);

/* RocksDB */
#ifdef USE_ROCKSDB
class RocksKeyComparator : public rocksdb::Comparator {
 public:
  int Compare(const rocksdb::Slice& _a, const rocksdb::Slice& _b) const;

  // GG: Ignore the following methods for now:
  const char* Name() const { return "RocksKeyComparator"; }
  void FindShortestSeparator(std::string*, const rocksdb::Slice&) const {}
  void FindShortSuccessor(std::string*) const {}
};
#endif

/* In memory */
bool InMemKeyComp(const Sliver& _a, const Sliver& _b);

}  // namespace Blockchain
/* Comparators end */

#endif
