// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License"). You may not use this product except in
// compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright notices and license terms. Your use of
// these subcomponents is subject to the terms and conditions of the sub-component's license, as noted in the LICENSE
// file.


#pragma once

#include <bftengine/SimpleClient.hpp>
#include <chrono>
#include <cstdint>
#include <utility>
#include "StatusInfo.h"
#include "client_pool_config.hpp"
#include "external_client_exception.hpp"

namespace bftEngine {
class ICommunication;
}

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
  ConcordClient(concord::config::ConcordConfiguration const& config,
                int client_id);

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
  void SendRequest(const void* request, std::uint32_t request_size,
                   bftEngine::ClientMsgFlag flags,
                   std::chrono::milliseconds timeout_ms,
                   std::uint32_t reply_size, void* out_reply,
                   std::uint32_t* out_actual_reply_size,
                   const std::string& correlation_id = {});

 private:
  void CreateClient(config::ConcordConfiguration const& config, int client_id);
  void CreateCommConfig(config::CommConfig& comm_config,
                        config::ConcordConfiguration const& config,
                        int num_replicas, int client_id);

  std::unique_ptr<::bftEngine::ICommunication> comm_;
  std::unique_ptr<kvbc::IClient> client_;
};

}  // namespace external_client
}  // namespace concord
