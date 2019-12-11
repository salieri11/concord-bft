// Copyright 2019 VMware, all rights reserved
//
// Filtered access to the KV Blockchain.

#include "kvb_app_filter.h"

#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>
#include <boost/lockfree/spsc_queue.hpp>
#include <cassert>
#include <chrono>
#include <sstream>

#include "kv_types.hpp"
#include "kvb_key_types.h"
// TODO: SetOfKeyValuePairs needs a hash definition provided in hash_defs.h
// It should be included in kv_types.hpp
#include "hash_defs.h"

using namespace std::chrono_literals;

using boost::lockfree::spsc_queue;

using concordUtils::BlockId;
using concordUtils::Key;
using concordUtils::KeyValuePair;
using concordUtils::SetOfKeyValuePairs;
using concordUtils::Status;

namespace concord {
namespace storage {

Status KvbAppFilter::ReadState(BlockId block_id_end, std::string &key_prefix,
                               spsc_queue<KeyValuePair *> &queue_out,
                               std::atomic_bool &stop_execution) {
  BlockId block_id;
  SetOfKeyValuePairs out_kvs;

  // We assume DAML data (simplified implementation)
  assert(type_ == KvbAppFilter::kDaml);
  char kvb_key_id = concord::storage::kKvbKeyDaml;

  // TODO: Determine oldest block available (pruning)
  block_id = 1;
  assert(block_id <= block_id_end);

  LOG4CPLUS_INFO(logger_,
                 "ReadState block " << block_id << " to " << block_id_end);

  for (; block_id <= block_id_end; ++block_id) {
    Status status = rostorage_->getBlockData(block_id, out_kvs);
    if (!status.isOK()) {
      std::stringstream msg;
      msg << "Couldn't retrieve block data for block id " << block_id;
      return Status::GeneralError(msg.str());
    }

    for (const auto &[key, value] : out_kvs) {
      // Filter by appliction type
      if (key[0] != kvb_key_id) {
        continue;
      }

      // Filter by key prefix
      if (key.toString().compare(1, key_prefix.size(), key_prefix) != 0) {
        continue;
      }

      // Strip KVB key type
      Key new_key =
          key.subsliver(sizeof kvb_key_id, key.length() - (sizeof kvb_key_id));

      // Try to push to the queue until we are told to stop
      KeyValuePair *new_elem = new KeyValuePair(new_key, value);
      while (!stop_execution) {
        if (queue_out.push(std::move(new_elem))) {
          break;
        }
        std::this_thread::sleep_for(10ms);
      }
    }
  }
  return Status::OK();
}

Status KvbAppFilter::ReadStateHash(BlockId block_id_end,
                                   std::string &key_prefix,
                                   KvbStateHash &hash_out) {
  BlockId block_id;
  SetOfKeyValuePairs out_kvs;

  // We assume DAML data (simplified implementation)
  assert(type_ == KvbAppFilter::kDaml);
  char kvb_key_id = concord::storage::kKvbKeyDaml;

  // TODO: Determine oldest block available (pruning)
  block_id = 1;
  assert(block_id <= block_id_end);

  LOG4CPLUS_INFO(logger_,
                 "ReadStateHash block " << block_id << " to " << block_id_end);

  hash_out = 0;
  for (; block_id <= block_id_end; ++block_id) {
    Status status = rostorage_->getBlockData(block_id, out_kvs);
    if (!status.isOK()) {
      std::stringstream msg;
      msg << "Couldn't retrieve block data for block id " << block_id;
      return Status::GeneralError(msg.str());
    }

    for (const auto &[key, value] : out_kvs) {
      // Filter by appliction type
      if (key[0] != kvb_key_id) {
        continue;
      }

      // Filter by key prefix
      if (key.toString().compare(1, key_prefix.size(), key_prefix) != 0) {
        continue;
      }

      // Strip KVB key type
      Key new_key =
          key.subsliver(sizeof kvb_key_id, key.length() - (sizeof kvb_key_id));

      // (key1 XOR value1) XOR (key2 XOR value2) ...
      auto key_hash =
          std::hash<string>{}(string{new_key.data(), new_key.length()});
      key_hash ^= std::hash<string>{}(string{value.data(), value.length()});
      hash_out ^= key_hash;
    }
  }
  return Status::OK();
}

}  // namespace storage
}  // namespace concord
