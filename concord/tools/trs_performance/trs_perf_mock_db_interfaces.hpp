// Copyright 2020 VMware, all rights reserved
#pragma once

#include <time.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <random>

#include "Logger.hpp"
#include "concord_storage.pb.h"
#include "db_interfaces.h"
#include "kv_types.hpp"
#include "status.hpp"

using com::vmware::concord::kvb::ValueWithTrids;
using concord::storage::kKvbKeyDaml;
using logging::Logger;

typedef std::pair<concord::kvbc::BlockId, concord::kvbc::SetOfKeyValuePairs>
    KvbUpdate;

namespace concord::perf::trs {

static std::vector<std::string> trs_time_vec;

class TrsPerfFakeStorage : public concord::kvbc::ILocalKeyValueStorageReadOnly {
 public:
  Logger logger_db = Logger::getInstance("concord.perf.trs");

 private:
  concord::kvbc::BlockId blockId_ = 0;
  std::vector<KvbUpdate> data_;

 public:
  TrsPerfFakeStorage() {}

  concordUtils::Status get(const concord::kvbc::Key& key,
                           concord::kvbc::Value& val) const override {
    return concordUtils::Status::IllegalOperation(
        "get() should not be called by this mock");
  }

  concordUtils::Status get(concord::kvbc::BlockId, const concordUtils::Sliver&,
                           concordUtils::Sliver&,
                           concord::kvbc::BlockId&) const override {
    return concordUtils::Status::IllegalOperation(
        "get() should not be called by this mock");
  }

  concord::kvbc::BlockId getGenesisBlock() const override { return 0; }

  // Return the latest block's id
  concord::kvbc::BlockId getLastBlock() const override {
    LOG_DEBUG(logger_db, "The last known block id is: " << blockId_);
    return blockId_;
  }

  // Get the data in a block given the block id
  concordUtils::Status getBlockData(
      concord::kvbc::BlockId b_id,
      concord::kvbc::SetOfKeyValuePairs& kvp) const override {
    if (b_id > 0 && b_id <= blockId_) {
      LOG_DEBUG(logger_db, "Getting data at block id: "
                               << b_id << " last known block id: " << blockId_);
      kvp = data_.at(b_id - 1).second;

      // Calculate the length of key value pair sent to the clients
      // Note: The key type and trid are also considered when calculating the
      // length
      int len_sent = 0;
      for (auto kv : kvp) {
        len_sent += kv.first.length() + kv.second.length();
      }

      // Get and save the start_time for latency calculation
      const auto start_time = std::chrono::system_clock::now();
      auto duration = start_time.time_since_epoch();
      auto millis =
          std::chrono::duration_cast<std::chrono::milliseconds>(duration)
              .count();
      trs_time_vec.push_back(std::to_string(b_id) + "," +
                             std::getenv("CLIENT_ID") + "," +
                             std::to_string(millis) + "," +
                             std::to_string(len_sent * sizeof(char)) + "\n");

      // Add all the timestamps to file when getting data after getting the
      // start_time for the last block, note, that this may impact the latency
      // for the last block
      if (b_id == blockId_) {
        std::ofstream trs_time_file;
        const char* addr = std::getenv("ADDRESS");
        if (addr != NULL) {
          std::string server_addr(addr);
          std::string trs_time_file_path = "/concord/trc-trs-perf-results/trs" +
                                           std::string(1, server_addr.back()) +
                                           ".csv";
          trs_time_file.open(trs_time_file_path);
          for (int i = 0; i < trs_time_vec.size(); i++) {
            trs_time_file << trs_time_vec[i];
          }
          trs_time_file.close();
        } else {
          LOG_FATAL(logger_db, "Server address environment variable not set!");
        }
      }
      return concordUtils::Status::OK();
    }
    return concordUtils::Status::InvalidArgument("Provide a valid block range");
  }

  concordUtils::Status mayHaveConflictBetween(const concordUtils::Sliver&,
                                              concord::kvbc::BlockId,
                                              concord::kvbc::BlockId,
                                              bool&) const override {
    return concordUtils::Status::IllegalOperation(
        "mayHaveConflictBetween() not supported in test mode");
  }

  std::string concatKeyType(std::string key, char keyType) {
    std::string str1 = keyType + key;
    return str1;
  }

  // Dummy method to fill DB for testing
  // each block is of the type:
  // <blockId,<"<keyType><random-alphanum string>","<random-alphanum string>">>
  void fillWithData(concord::kvbc::BlockId _start_block_id,
                    concord::kvbc::BlockId _last_block_id, const int _kv_count,
                    const int _key_size, const int _value_size) {
    KvbUpdate block;
    LOG_DEBUG(logger_db, "Filling data with:\n start_block_id: "
                             << _start_block_id << "\n last_block_id: "
                             << _last_block_id << "\n kv_count: " << _kv_count
                             << "\n key_size: " << _key_size
                             << "\n value_size: " << _value_size);
    for (concord::kvbc::BlockId i = _start_block_id; i <= _last_block_id; i++) {
      concord::kvbc::SetOfKeyValuePairs kvp{};
      for (int j = 0; j < _kv_count; j++) {
        // generate a key value, and add the key type
        std::string random_key =
            concatKeyType(generateRandom(_key_size, i, j), kKvbKeyDaml);
        concordUtils::Sliver key(std::move(random_key));

        // generate a value, add the TRID
        std::string random_val = generateRandom(_value_size, i, j);
        ValueWithTrids proto;
        proto.add_trid(std::getenv("CLIENT_ID"));
        proto.set_value(random_val);
        std::string serialized = proto.SerializeAsString();
        concordUtils::Sliver val(std::move(serialized));

        LOG_DEBUG(logger_db, " Generated kvp with - block id:"
                                 << i << " kvp: " << j << " key:" << key.data()
                                 << " val: " << val.data());

        kvp.insert(std::make_pair(key, val));
      }
      block.first = i;
      block.second = kvp;
      data_.push_back(block);
      blockId_ = i;
    }
  }

 private:
  // Generate a random string of a given byte size
  // block number and kvp number are used as seed for srand
  std::string generateRandom(const int byte_size, int block, int kvp) {
    int len = byte_size / sizeof(char);
    std::string temp = std::to_string(block) + std::to_string(kvp);
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    for (int i = 0; i < len; ++i) {
      srand(block * kvp * i);
      temp += alphanum[rand() % (sizeof(alphanum) - 1)];
    }

    return temp;
  }
};  // namespace concord::perf::trs
}  // namespace concord::perf::trs
