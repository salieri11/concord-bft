// Copyright 2020 VMware, all rights reserved

// The MultiBlockData holds pre-generated data for any ammount of blocks.
// It may be used by Performance Execution Engine for generating all blocks in
// advance, thus minimizing the execution time. When the blocks are created,
// they are hold in the MultiBlockData struct, and the PerfInitData class holds
// map of init id (in form of
// {num_of_blocks}_{num_of_keys_per_block}_{key_size}_{value_size}). We can hold
// in memory different blocks and it will allow to create dynamic flows without
// spending time on creating these blocks while executing. We want to measure
// pure concord peformance. The data can be removed from memory using Clean
// method. This class is invoked from the measure.py script via Peformance gRPC
// service.

#pragma once
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <list>
#include <memory>
#include <mutex>
#include <unordered_map>
#include <vector>
#include "Logger.hpp"
#include "Logging4cplus.hpp"
#include "kv_types.hpp"

using namespace std;
using namespace concord::kvbc;
using namespace concordUtils;

namespace concord {
namespace performance {

struct MultiBlockData {
  string id;
  vector<SetOfKeyValuePairs> blocks;
  uint64_t total_size = 0;
};

class PerfInitData {
 public:
  PerfInitData() : logger_(logging::getLogger("concord.perf.initdata")) {}

  string GetInfo() {
    lock_guard lock(data_lock_);
    return "Total " + to_string(blocks_data_.size()) +
           " worksets, workset size: " + to_string(total_size_);
  }

  bool GetBlocksData(string id, shared_ptr<MultiBlockData> &outData) {
    lock_guard lock(data_lock_);
    auto it = blocks_data_.find(id);
    if (it == blocks_data_.end()) {
      LOG_DEBUG(logger_, "get_init_data, id: " << id << " not found");
      return false;
    }
    outData = it->second;
    LOG_DEBUG(logger_, "get_init_data, id: "
                           << id
                           << "  found, data size: " << outData->total_size
                           << " blocksCount: " << outData->blocks.size());
    return true;
  }

  void Clean(string id) {
    auto info = GetInfo();
    lock_guard lock(data_lock_);
    auto it = blocks_data_.find(id);
    if (it == blocks_data_.end()) {
      LOG_DEBUG(logger_,
                "clean, clean all, current info: " << info << ", id: " << id);
      blocks_data_.clear();
      total_size_ = 0;
    } else {
      total_size_ -= it->second->total_size;
      blocks_data_.erase(it);
      LOG_DEBUG(logger_, "clean, id: " << id << ", new size: " << total_size_);
    }
  }

  string CreateData(uint32_t blocks_count, uint32_t kv_count, uint32_t key_size,
                    uint32_t value_size, shared_ptr<MultiBlockData> &outData,
                    uint32_t keyRand = 0, uint32_t valRand = 0) {
    size_t iSize = sizeof(uint32_t);
    string msg;
    if (kv_count == 0) msg = "kv_count must be >= 0";
    if (key_size < 3 * iSize)
      msg += ",key_size should be at least " + to_string(3 * iSize);
    if (value_size < 3 * iSize)
      msg += ",value_size should be at least " + to_string(3 * iSize);
    if (blocks_count == 1 && ((keyRand & valRand) == 0))
      msg += ",either block_count should be > 1 or prefixes != 0";
    if (!msg.empty()) return msg;

    lock_guard lock(data_lock_);
    string id = to_string(blocks_count) + "_" + to_string(kv_count) + "_" +
                to_string(key_size) + "_" + to_string(value_size) + "_" +
                to_string(keyRand) + "_" + to_string(valRand);
    LOG_DEBUG(logger_, "create_data, id: " << id);
    auto it = blocks_data_.find(id);
    if (it != blocks_data_.end()) {
      outData = it->second;
      return it->second->id;
    };
    LOG_DEBUG(logger_, "create_data, creating, id: " << id);

    auto bdata = make_shared<MultiBlockData>();
    bdata->id = id;
    for (uint32_t j = 0; j < blocks_count; j++) {
      SetOfKeyValuePairs blockContent;
      uint32_t size = kv_count * (key_size + value_size);
      total_size_ += size;
      char c = 'a';
      char c1 = 'z';
      bdata->blocks.push_back(SetOfKeyValuePairs());
      for (uint32_t i = 0; i < kv_count; i++) {
        // key
        char *key = new char[key_size];
        memcpy(key, &j, iSize);
        memcpy(key + iSize, &i, iSize);
        keyRand = keyRand == 0 ? i : keyRand;
        memcpy(key + iSize * 2, &keyRand, iSize);
        memset(key + iSize * 3, c, key_size - iSize * 3);
        Sliver keyS(key, key_size);
        // value
        char *val = new char[value_size];
        memcpy(val, &j, iSize);
        memcpy(val + iSize, &i, iSize);
        valRand = valRand == 0 ? j : valRand;
        memcpy(val + iSize * 2, &valRand, iSize);
        memset(val + iSize * 3, c1, value_size - iSize * 3);
        Sliver valS(val, value_size);
        if (++c == 'z') c = 'a';
        if (--c1 == 'a') c1 = 'z';
        bdata->blocks[j][move(keyS)] = move(valS);
      }

      bdata->total_size += kv_count * (key_size + value_size);
    }

    if (blocks_count > 1) {
      blocks_data_[id] = move(bdata);
      outData = blocks_data_[id];
    } else {
      outData = move(bdata);
    }
    LOG_DEBUG(logger_, "create_data, created, id: " << id << ", totalSize: "
                                                    << outData->total_size);
    return id;
  }

  unordered_map<string, shared_ptr<MultiBlockData>> blocks_data_;
  uint64_t total_size_ = 0;
  logging::Logger logger_;
  mutex data_lock_;
};

}  // namespace performance
}  // namespace concord
