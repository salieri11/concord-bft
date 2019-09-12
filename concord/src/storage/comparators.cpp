// Copyright 2018 VMware, all rights reserved
//
// Storage key comparators implementation.

#include "comparators.h"

#include <log4cplus/loggingmacros.h>

#include "common/concord_log.hpp"
#include "consensus/hex_tools.h"
#include "consensus/sliver.hpp"
#include "storage/blockchain_db_adapter.h"
#include "storage/blockchain_db_types.h"
#include "storage/blockchain_interfaces.h"
#include "storage/rocksdb_client.h"

#include <chrono>

using concord::common::HexPrintBytes;
using concord::consensus::Sliver;
using log4cplus::Logger;

namespace concord {
namespace storage {

/*
 * If key a is earlier than key b, return a negative number; if larger, return a
 * positive number; if equal, return zero.
 *
 * Comparison is done by decomposed parts. Types are compared first, followed by
 * the application key, and finally the block id. Types and keys are sorted in
 * ascending order, and block IDs are sorted in descending order.
 */
int RocksKeyComparator::ComposedKeyComparison(const Logger& logger,
                                              const Sliver& a,
                                              const Sliver& b) {
  return ComposedKeyComparison(logger, a.data(), a.length(), b.data(),
                               b.length());
}

int RocksKeyComparator::ComposedKeyComparison(const Logger& logger,
                                              const uint8_t* a_data,
                                              size_t a_length,
                                              const uint8_t* b_data,
                                              size_t b_length) {
  char aType = KeyManipulator::extractTypeFromKey(a_data);
  char bType = KeyManipulator::extractTypeFromKey(b_data);
  if (aType != bType) {
    int ret = aType - bType;
    return ret;
  }

  // In case it is E_DB_KEY_TYPE_METADATA_KEY, compare object IDs.
  if (aType == (char)EDBKeyType::E_DB_KEY_TYPE_BFT_METADATA_KEY) {
    ObjectId aObjId =
        KeyManipulator::extractObjectIdFromKey(logger, a_data, a_length);
    ObjectId bObjId =
        KeyManipulator::extractObjectIdFromKey(logger, b_data, b_length);

    if (aObjId < bObjId) return -1;
    if (bObjId < aObjId) return 1;
    return 0;
  }

  // Blocks don't have a separate key component.
  if (aType != ((char)EDBKeyType::E_DB_KEY_TYPE_BLOCK)) {
    int keyComp = KeyManipulator::compareKeyPartOfComposedKey(
        logger, a_data, a_length, b_data, b_length);
    if (keyComp != 0) {
      return keyComp;
    }
  }

  // Extract the block ids to compare so that endianness of environment
  // does not matter.
  BlockId aId = KeyManipulator::extractBlockIdFromKey(logger, a_data, a_length);
  BlockId bId = KeyManipulator::extractBlockIdFromKey(logger, b_data, b_length);

  // within a type+key, block ids are sorted in reverse order
  if (aId < bId) {
    return 1;
  } else if (bId < aId) {
    return -1;
  } else {
    return 0;
  }
}

/* RocksDB */
#ifdef USE_ROCKSDB
int RocksKeyComparator::Compare(const rocksdb::Slice& _a,
                                const rocksdb::Slice& _b) const {
  int ret = ComposedKeyComparison(
      logger, reinterpret_cast<const uint8_t*>(_a.data()), _a.size(),
      reinterpret_cast<const uint8_t*>(_b.data()), _b.size());

  LOG4CPLUS_DEBUG(
      logger, "Compared " << (HexPrintBytes{_a.data(),
                                            static_cast<uint32_t>(_a.size())})
                          << " with "
                          << (HexPrintBytes{_b.data(),
                                            static_cast<uint32_t>(_b.size())})
                          << ", returning " << ret);

  return ret;
}
#endif

/* In memory */
bool RocksKeyComparator::InMemKeyComp(const Sliver& _a, const Sliver& _b) {
  Logger logger(
      log4cplus::Logger::getInstance("concord.storage.RocksKeyComparator"));
  int comp = ComposedKeyComparison(logger, _a, _b);

  LOG4CPLUS_DEBUG(logger, "Compared " << _a << " with " << _b
                                      << ", a < b == " << (comp < 0));

  // Check: comp < 0 ==> _a < _b
  return comp < 0;
}

}  // namespace storage
}  // namespace concord
