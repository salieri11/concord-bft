// Copyright 2019 VMware, all rights reserved
//
// Filtered access to the KV Blockchain.

#ifndef CONCORD_STORAGE_KVB_APP_FILTER_H_
#define CONCORD_STORAGE_KVB_APP_FILTER_H_

#include <log4cplus/logger.h>
#include <atomic>
#include <boost/lockfree/spsc_queue.hpp>
#include <future>

#include "blockchain/db_interfaces.h"

namespace concord {
namespace storage {

// TODO: Move into concordUtils
typedef size_t KvbStateHash;

class KvbAppFilter {
 public:
  enum AppType {
    kDaml = 0,
  };

  KvbAppFilter(const concord::storage::blockchain::ILocalKeyValueStorageReadOnly
                   *rostorage,
               const KvbAppFilter::AppType app_type)
      : logger_(log4cplus::Logger::getInstance("concord.storage.KvbFilter")),
        rostorage_(rostorage),
        type_(app_type) {}

  // Return all key-value pairs from the KVB in the block range [earliest block
  // available, given block_id] with the following conditions:
  //   * The key-value pair is part of a block
  //   * The key is of type type_ (see KvbAppFilter::AppType)
  // The result is pushed to the given queue. Thereby, the caller is responsible
  // for consuming the elements from the queue. The function will block if the
  // queue is full and therefore, it cannot push a new key-value pair.
  // Note: single producer & single consumer queue.
  concordUtils::Status ReadState(
      concordUtils::BlockId current_block_id,
      boost::lockfree::spsc_queue<concordUtils::KeyValuePair *> &queue_out,
      std::atomic_bool &stop_execution);

  // Compute the hash of all key-value pairs in the range of [earliest block
  // available, given block_id] based on the given KvbAppFilter::AppType.
  concordUtils::Status ReadStateHash(concordUtils::BlockId block_id,
                                     KvbStateHash &hash_out);

 private:
  log4cplus::Logger logger_;
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly *rostorage_;
  const KvbAppFilter::AppType type_;
};

}  // namespace storage
}  // namespace concord

#endif  // CONCORD_STORAGE_KVB_APP_FILTER_H_
