// Copyright 2019 VMware, all rights reserved
//
// Filtered access to the KV Blockchain.

#include "kvb_app_filter.h"

#include <boost/lockfree/spsc_queue.hpp>
#include <cassert>
#include <chrono>
#include <exception>
#include <sstream>
#include "Logger.hpp"

#include "concord_storage.pb.h"
#include "kv_types.hpp"
#include "kvb_key_types.h"

using namespace std::chrono_literals;

using boost::lockfree::spsc_queue;

using concord::kvbc::BlockId;
using concord::kvbc::Key;
using concord::kvbc::KeyValuePair;
using concord::kvbc::SetOfKeyValuePairs;
using concord::storage::InvalidBlockRange;
using concordUtils::Sliver;
using concordUtils::Status;

using com::vmware::concord::kvb::ValueWithTrids;

namespace concord {
namespace storage {

SetOfKeyValuePairs KvbAppFilter::FilterKeyValuePairs(
    const SetOfKeyValuePairs &kvs) {
  assert(types_.find(KvbAppFilter::kDaml) != types_.end());
  char kvb_key_id = concord::storage::kKvbKeyDaml;

  SetOfKeyValuePairs filtered_kvs;

  bool keep_cid = types_.find(KvbAppFilter::kCid) != types_.end();
  for (const auto &[key, value] : kvs) {
    if (keep_cid && key[0] == kKvbKeyCorrelationId) {
      filtered_kvs.insert({key, value});
      continue;
    }
    // Filter by appliction type
    if (key[0] != kvb_key_id) {
      continue;
    }

    // Filter by key prefix
    if (key.toString().compare(1, key_prefix_.size(), key_prefix_) != 0) {
      continue;
    }

    // Strip KVB key type
    Key new_key =
        key.subsliver(sizeof kvb_key_id, key.length() - (sizeof kvb_key_id));

    ValueWithTrids proto;
    if (!proto.ParseFromArray(value.data(), value.length())) {
      continue;
    }

    // If no TRIDs attached then everyon is allowed to view the pair
    // Otherwise, check against the client id
    if (proto.trid_size() > 0) {
      bool contains_client_id = false;
      for (const auto &trid : proto.trid()) {
        if (trid.compare(client_id_) == 0) {
          contains_client_id = true;
          break;
        }
      }
      if (!contains_client_id) {
        continue;
      }
    }

    // We expect a value - this should never trigger
    if (!proto.has_value()) {
      std::stringstream msg;
      msg << "Couldn't decode value with trids " << new_key.string_view();
      throw KvbReadError(msg.str());
    }

    auto val = proto.release_value();
    filtered_kvs.insert({new_key, Sliver(std::move(*val))});
    delete val;
  }

  return filtered_kvs;
}

KvbUpdate KvbAppFilter::FilterUpdate(const KvbUpdate &update) {
  return KvbUpdate{update.first, FilterKeyValuePairs(update.second)};
}

size_t KvbAppFilter::HashUpdate(const KvbUpdate update) {
  size_t hash = std::hash<std::string>{}(std::to_string(update.first));
  for (const auto &[key, value] : update.second) {
    // (key1 XOR value1) XOR (key2 XOR value2) ...
    auto key_hash =
        std::hash<std::string>{}(std::string{key.data(), key.length()});
    key_hash ^=
        std::hash<std::string>{}(std::string{value.data(), value.length()});
    hash ^= key_hash;
  }
  return hash;
}

void KvbAppFilter::ReadBlockRange(BlockId block_id_start, BlockId block_id_end,
                                  spsc_queue<KvbUpdate> &queue_out,
                                  const std::atomic_bool &stop_execution) {
  if (block_id_start > block_id_end ||
      block_id_end > rostorage_->getLastBlock()) {
    throw InvalidBlockRange();
  }

  BlockId block_id(block_id_start);

  SetOfKeyValuePairs kvb_kvs;

  LOG_DEBUG(logger_,
            "ReadBlockRange block " << block_id << " to " << block_id_end);

  for (; block_id <= block_id_end; ++block_id) {
    Status status = rostorage_->getBlockData(block_id, kvb_kvs);
    if (!status.isOK()) {
      std::stringstream msg;
      msg << "Couldn't retrieve block data for block id " << block_id;
      throw KvbReadError(msg.str());
    }

    KvbUpdate update{block_id, FilterKeyValuePairs(kvb_kvs)};
    while (!stop_execution) {
      if (queue_out.push(update)) {
        break;
      }
    }

    if (stop_execution) {
      LOG_WARN(logger_, "ReadBlockRange was stopped");
      break;
    }
  }
}

KvbStateHash KvbAppFilter::ReadBlockHash(BlockId block_id) {
  return ReadBlockRangeHash(block_id, block_id);
}

KvbStateHash KvbAppFilter::ReadBlockRangeHash(BlockId block_id_start,
                                              BlockId block_id_end) {
  if (block_id_start > block_id_end ||
      block_id_end > rostorage_->getLastBlock()) {
    throw InvalidBlockRange();
  }
  BlockId block_id(block_id_start);

  SetOfKeyValuePairs kvb_kvs;

  LOG_DEBUG(logger_,
            "ReadBlockRangeHash block " << block_id << " to " << block_id_end);

  size_t hash_out = 0;
  for (; block_id <= block_id_end; ++block_id) {
    Status status = rostorage_->getBlockData(block_id, kvb_kvs);
    if (!status.isOK()) {
      std::stringstream msg;
      msg << "Couldn't retrieve block data for block id " << block_id;
      throw KvbReadError(msg.str());
    }

    KvbUpdate filtered_update{block_id, FilterKeyValuePairs(kvb_kvs)};
    hash_out ^= HashUpdate(filtered_update);
  }
  return hash_out;
}

}  // namespace storage
}  // namespace concord
