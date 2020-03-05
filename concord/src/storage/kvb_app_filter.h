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
typedef std::pair<concordUtils::BlockId, concordUtils::SetOfKeyValuePairs>
    KvbUpdate;

class KvbReadError : public std::exception {
 public:
  explicit KvbReadError(const std::string &what) : msg(what){};
  virtual const char *what() const noexcept override { return msg.c_str(); }

 private:
  std::string msg;
};

class InvalidBlockRange : public std::runtime_error {
 public:
  InvalidBlockRange() : std::runtime_error("Invalid block range"){};
};

class KvbAppFilter {
 public:
  enum AppType {
    kDaml = 0,
    kCid = 1,
  };

  KvbAppFilter(const concord::storage::blockchain::ILocalKeyValueStorageReadOnly
                   *rostorage,
               const std::set<KvbAppFilter::AppType> &app_types,
               const std::string &client_id, const std::string &key_prefix)
      : logger_(log4cplus::Logger::getInstance("concord.storage.KvbFilter")),
        rostorage_(rostorage),
        types_(app_types),
        client_id_(client_id),
        key_prefix_(key_prefix) {}

  // Filter the given update
  KvbUpdate FilterUpdate(const KvbUpdate &update);

  // Compute hash for the given update
  size_t HashUpdate(const KvbUpdate update);

  // Return all key-value pairs from the KVB in the block range [earliest block
  // available, given block_id] with the following conditions:
  //   * The key-value pair is part of a block
  //   * The key is of type type_ (see KvbAppFilter::AppType)
  //   * The key starts with the given key_prefix
  // The result is pushed to the given queue. Thereby, the caller is responsible
  // for consuming the elements from the queue. The function will block if the
  // queue is full and therefore, it cannot push a new key-value pair.
  // Note: single producer & single consumer queue.
  void ReadBlockRange(concordUtils::BlockId start, concordUtils::BlockId end,
                      boost::lockfree::spsc_queue<KvbUpdate> &queue_out,
                      const std::atomic_bool &stop_execution);

  // Compute the hash of all key-value pairs in the range of [earliest block
  // available, given block_id] based on the given KvbAppFilter::AppType.
  KvbStateHash ReadBlockRangeHash(concordUtils::BlockId start,
                                  concordUtils::BlockId end);
  KvbStateHash ReadBlockHash(concordUtils::BlockId block_id);

 private:
  log4cplus::Logger logger_;
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly *rostorage_;
  const std::set<KvbAppFilter::AppType> types_;
  const std::string client_id_;
  const std::string key_prefix_;

  // Filter the given set of key-value pairs and return the result.
  SetOfKeyValuePairs FilterKeyValuePairs(const SetOfKeyValuePairs &kvs);
};

}  // namespace storage
}  // namespace concord

#endif  // CONCORD_STORAGE_KVB_APP_FILTER_H_
