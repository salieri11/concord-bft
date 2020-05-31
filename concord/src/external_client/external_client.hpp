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

#include <bftengine/SimpleClient.hpp>
#include <chrono>
#include <cstdint>
#include <utility>
#include "client_pool_config.hpp"
#include "communication/StatusInfo.h"
#include "external_client_exception.hpp"

namespace concord {

namespace config {
class ConcordConfiguration;
}

namespace kvbc {
class IClient;
}

namespace external_client {

// Represents a Concord BFT client. The purpose of this class is to be easy to
// use for external users. This is achieved by:
//  * providing a simple public interface
//  * providing a generic public interface that allows for various use cases
//  * configuration via a file - users don't need to know what the structure of
//  the file is and changes to the file will not affect the interface of the
//  client
class ConcordClient {
 public:
  // Constructs the client by passing concord configuration
  // object and a client_id to get the specific values for this client.
  // Construction executes all needed steps to provide a ready-to-use
  // object (including starting internal threads, if needed).
  ConcordClient(const concord::config::ConcordConfiguration& config,
                int client_id,
                const config_pool::ClientPoolConfig& pool_config);

  // Destructs the client. This includes stopping any internal threads, if
  // needed.
  ~ConcordClient() noexcept;

  // Sends a synchronous request to the Concord BFT cluster.
  // Parameters:
  //  - request: a request buffer
  //  - request_size: size of the request buffer in bytes
  //  - flags: request bitmask flags as per the bftEngine::ClientMsgFlag enum
  //  - timeout_ms: request timeout in milliseconds
  //  - reply_size: size of the reply buffer in bytes
  //  - out_reply[out]: a reply buffer
  //  - out_actual_reply_size[out]: the actual size in bytes written to the
  //  reply buffer
  //  -correlation_id: the correlation id of the request, defualt value - empty
  //  string
  // This method doesn't allocate or deallocate memory. All buffers should be
  // managed by the user.
  int SendRequest(const char* request, std::uint32_t request_size,
                  bftEngine::ClientMsgFlag flags,
                  std::chrono::milliseconds timeout_ms,
                  std::uint32_t reply_size, void* out_reply,
                  std::uint32_t* out_actual_reply_size, uint64_t seq_num,
                  const std::string& correlation_id = {});

  int getClientId() const;

  uint64_t getClientSeqNum() const;

  void generateClientSeqNum();

  void setStartRequestTime();

  std::chrono::steady_clock::time_point getStartRequestTime() const;

  bool isRunning() const;

 private:
  void CreateClient(const config::ConcordConfiguration& config,
                    config_pool::ClientPoolConfig pool_config);
  void CreateCommConfig(config::CommConfig& comm_config,
                        const config::ConcordConfiguration& config,
                        int num_replicas,
                        config_pool::ClientPoolConfig pool_config);

  std::unique_ptr<bft::communication::ICommunication> comm_;
  std::unique_ptr<bftEngine::SimpleClient> client_;
  // Logger
  log4cplus::Logger logger_;
  int client_id_;
  bftEngine::SeqNumberGeneratorForClientRequests* seqGen_ = nullptr;
  uint64_t seq_num_ = 0;
  std::chrono::steady_clock::time_point start_job_time_ =
      std::chrono::steady_clock::now();
  uint16_t f_val_ = 0;
  uint16_t c_val_ = 0;
};

}  // namespace external_client
}  // namespace concord
