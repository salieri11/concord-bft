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

#include "concord_client_pool.hpp"

namespace concord::concord_client_pool {
SubmitResult ConcordClientPool::SendRequest(
    const bft::client::WriteConfig& config, bft::client::Msg&& request) {
  return SubmitResult::Acknowledged;
}

PoolStatus ConcordClientPool::HealthStatus() { return PoolStatus::Serving; }

ConcordClientPool::ConcordClientPool(std::string config_file_path) {}

ConcordClientPool::~ConcordClientPool() {}
}  // namespace concord::concord_client_pool
