// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#pragma once

#include <chrono>
#include <cstdint>
#include <set>
#include <string>
#include <variant>
#include <vector>

using namespace std::chrono_literals;

namespace bft::client {
struct RequestConfig {
  bool pre_execute;
  uint64_t sequence_number = 0;
  uint32_t max_reply_size = 64 * 1024;
  std::chrono::milliseconds timeout = 5s;
  std::string correlation_id = "";
};

struct ReplicaId {
  uint16_t val;

  bool operator==(const ReplicaId& other) const { return val == other.val; }
  bool operator!=(const ReplicaId& other) const { return val != other.val; }
  bool operator<(const ReplicaId& other) const { return val < other.val; }
};

struct LinearizableQuorum {
  std::set<ReplicaId> destinations;
};

struct ByzantineSafeQuorum {
  std::set<ReplicaId> destinations;
};

typedef std::variant<LinearizableQuorum, ByzantineSafeQuorum> WriteQuorum;

struct WriteConfig {
  RequestConfig request;
  WriteQuorum quorum = LinearizableQuorum();
};

typedef std::vector<char> Msg;
}  // namespace bft::client

namespace concord::concord_client_pool {
enum SubmitResult {
  Acknowledged,
  Overloaded,
};

enum PoolStatus {
  Serving,
  NotServing,
};

class ConcordClientPool {
 public:
  ConcordClientPool(std::string config_file_path);
  ~ConcordClientPool();
  SubmitResult SendRequest(const bft::client::WriteConfig& config,
                           bft::client::Msg&& request);
  PoolStatus HealthStatus();
};
}  // namespace concord::concord_client_pool
