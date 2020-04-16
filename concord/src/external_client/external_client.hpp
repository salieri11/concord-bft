// Copyright 2020 VMware, all rights reserved

#pragma once
#include <SimpleThreadPool.hpp>
#include <bftengine/SimpleClient.hpp>
#include <chrono>
#include <cstdint>
#include <istream>
#include <memory>
#include <mutex>
#include <queue>
#include <string_view>
#include <thread>
#include "StatusInfo.h"
#include "concord_client_pool.hpp"
#include "external_client_exception.hpp"
using util::SimpleThreadPool;
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
  // Constructs the client by passing an absolute path to the configuration
  // file. Construction executes all needed steps to provide a ready-to-use
  // object (including starting internal threads, if needed). The
  // status_callback function is called by the client to report peer
  // connectivity status. Errors are reported by throwing from the constructor.
  ConcordClient(std::string_view config_file_path);

  // Constructs the client by passing an input stream to the configuration file.
  // Construction executes all needed steps to provide a ready-to-use object
  // (including starting internal threads, if needed). The status_callback
  // function is called by the client to report peer connectivity status. Errors
  // are reported by throwing from the constructor.
  ConcordClient(std::istream& config_stream);

  ConcordClient(concord::config::ConcordConfiguration config, int client_id);

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
  //
  // This method doesn't allocate or deallocate memory. All buffers should be
  // managed by the user.
  //
  // Throws a ClientRequestException on known errors and any other exception on
  // unknown ones.

  void SendRequestASync(const void* request, std::uint32_t request_size,
                        bftEngine::ClientMsgFlag flags,
                        std::chrono::milliseconds timeout_ms,
                        std::uint32_t reply_size, void* out_reply,
                        std::uint32_t* out_actual_reply_size,
                        const std::string& correlation_id = {});

 private:
  void Initialize(std::istream& config_stream);
  void CreateClient(const config::ConcordConfiguration&);
  void CreateClient(const config::ConcordConfiguration&, int client_id);

  std::unique_ptr<::bftEngine::ICommunication> comm_;
  std::unique_ptr<kvbc::IClient> client_;
};

}  // namespace external_client
}  // namespace concord
