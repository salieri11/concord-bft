// Copyright 2020 VMware, all rights reserved
//
// Hash utility functions for internal use by the Thin Replica Client

#pragma once

#include "thin_replica.grpc.pb.h"
#include "update.hpp"

namespace thin_replica_client {

size_t HashUpdate(const Update& update) {
  size_t hash = std::hash<std::string>{}(std::to_string(update.block_id));
  for (const auto& [key, value] : update.kv_pairs) {
    auto key_hash = std::hash<std::string>{}(key);
    key_hash ^= std::hash<std::string>{}(value);
    hash ^= key_hash;
  }
  return hash;
}

size_t HashUpdate(const com::vmware::concord::thin_replica::Data& update) {
  size_t hash = std::hash<std::string>{}(std::to_string(update.block_id()));
  for (const auto& kvp : update.data()) {
    auto key_hash = std::hash<std::string>{}(kvp.key());
    key_hash ^= std::hash<std::string>{}(kvp.value());
    hash ^= key_hash;
  }
  return hash;
}

}  // namespace thin_replica_client
