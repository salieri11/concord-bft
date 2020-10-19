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

#include "concord_client_pool_wrapper.h"

#include <log4cplus/configurator.h>

#include <memory>
#include <unordered_map>

using bft::client::RequestConfig;
using bft::client::WriteConfig;
using concord::concord_client_pool::ConcordClientPool;
using concord::concord_client_pool::PoolStatus;

static uint16_t current_id = 0;
static std::unordered_map<uint16_t, std::unique_ptr<ConcordClientPool>>
    client_handles;

// Replicate the HACK in thin-replica-client/lib/thin_replica_client_facade.cpp
// Log initialization should happen in the "main" function
// Re-read the log4cplus configuration at runtime. Given that we integrate with
// JAVA and a different log framework, let's define a global variable here to
// make sure this thread will stay alive as long as the process is alive. If we
// integrate this library into C++ code then the application should do this.
const static int kLogConfigRefreshIntervalInMs = 60 * 1000;

const static char *GetLog4CplusConfigLocation() {
  auto log_location = std::getenv("LOG4CPLUS_CONFIGURATION");
  return log_location ? log_location : "LOG4CPLUS_CONFIGURATION_NOT_SET";
}
const static log4cplus::ConfigureAndWatchThread kLogConfigThread(
    GetLog4CplusConfigLocation(), kLogConfigRefreshIntervalInMs);

uint16_t BFTClient_create(const char *config_file_path) {
  uint16_t new_client_handle = current_id++;
  client_handles.emplace(new_client_handle, std::make_unique<ConcordClientPool>(
                                                std::string(config_file_path)));
  return new_client_handle;
}

WriteConfig CreateWriteConfig(bool pre_execute, unsigned long timeout_millis,
                              const char *correlation_id);
SubmitResult ToSubmitResult(
    concord::concord_client_pool::SubmitResult concord_submit_result);

BFTClient_SubmitResult_t BFTClient_send_request(
    uint16_t client_handle, const char *concord_request_protobuf_bytes,
    size_t concord_request_protobuf_bytes_length, bool pre_execute,
    unsigned long timeout_millis, const char *correlation_id) {
  try {
    return ToSubmitResult(
        client_handles.at(client_handle)
            ->SendRequest(
                CreateWriteConfig(pre_execute, timeout_millis, correlation_id),
                std::vector<uint8_t>(
                    concord_request_protobuf_bytes,
                    concord_request_protobuf_bytes +
                        concord_request_protobuf_bytes_length)));
  } catch (const concord::concord_client_pool::InternalError &_internal_error) {
    return SubmitResult::InternalError;
  }
}

BFTClient_PoolStatus_t BFTClient_health_status(uint16_t client_handle) {
  return client_handles.at(client_handle)->HealthStatus();
}

void BFTClient_destroy(uint16_t client_handle) {
  client_handles.erase(client_handle);
}

WriteConfig CreateWriteConfig(bool pre_execute, unsigned long timeout_millis,
                              const char *correlation_id) {
  RequestConfig requestConfig;

  requestConfig.pre_execute = pre_execute;
  requestConfig.timeout = std::chrono::milliseconds(timeout_millis);
  requestConfig.correlation_id = std::string(correlation_id);

  WriteConfig config;
  config.request = requestConfig;

  return config;
}

SubmitResult ToSubmitResult(
    concord::concord_client_pool::SubmitResult concord_submit_result) {
  switch (concord_submit_result) {
    case Acknowledged:
      return Acknowledged;

    case Overloaded:
      return Overloaded;
  }
}
