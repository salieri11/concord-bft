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

#include "SimpleClient.hpp"
#include "StatusInfo.h"
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
// Represents the answer that the DAML Ledger API could get when sending a
// request
enum class SubmitResult {
  Acknowledged,  // The request has been queued for submission
  Overloaded,    // There is no available client to the moment to process the
  // request
  InternalError  // An internal error has occurred. Reason is recorded in logs.
};

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
  ConcordClient(std::string_view config_file_path,
                UPDATE_CONNECTIVITY_FN status_callback);

  // Constructs the client by passing an input stream to the configuration file.
  // Construction executes all needed steps to provide a ready-to-use object
  // (including starting internal threads, if needed). The status_callback
  // function is called by the client to report peer connectivity status. Errors
  // are reported by throwing from the constructor.
  ConcordClient(std::istream& config_stream,
                UPDATE_CONNECTIVITY_FN status_callback);

  ConcordClient(concord::config::ConcordConfiguration config, int,
                UPDATE_CONNECTIVITY_FN status_callback);

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

  void SendRequestSync(const void* request, std::uint32_t request_size,
                       bftEngine::ClientMsgFlag flags,
                       std::chrono::milliseconds timeout_ms,
                       std::uint32_t reply_size, void* out_reply,
                       std::uint32_t* out_actual_reply_size);

  void SendRequestASync(const void* request, std::uint32_t request_size,
                        bftEngine::ClientMsgFlag flags,
                        std::chrono::milliseconds timeout_ms,
                        std::uint32_t reply_size, void* out_reply,
                        std::uint32_t* out_actual_reply_size,
                        const std::string& correlation_id);

 private:
  void Initialize(std::istream& config_stream,
                  UPDATE_CONNECTIVITY_FN status_callback);
  void CreateClient(const config::ConcordConfiguration&,
                    UPDATE_CONNECTIVITY_FN status_callback);
  void CreateClient(const config::ConcordConfiguration&, int,
                    UPDATE_CONNECTIVITY_FN status_callback);

  std::unique_ptr<::bftEngine::ICommunication> comm_;
  std::unique_ptr<kvbc::IClient> client_;
};

// Represents a Concord BFT client pool. The purpose of this class is to be easy
// to use for external users. This is achieved by:
//  * providing a simple public interface
//  * providing a generic public interface that allows for various use cases
//  * configuration via a file - users don't need to know what the structure of
//  the file is and changes to the file will not affect the interface of the
//  client
class ConcordClientPool {
 private:
  // Clients queue mutex
  std::mutex clients_queue_lock_;
  // Clients that are available for use (i.e. not already in use).
  std::queue<std::shared_ptr<ConcordClient>> clients_;
  // Thread pool, on each thread on client will run
  util::SimpleThreadPool jobs_thread_pool_;

 public:
  ConcordClientPool(std::istream& config_stream,
                    UPDATE_CONNECTIVITY_FN status_callback);
  ConcordClientPool(std::string_view config_file_path,
                    UPDATE_CONNECTIVITY_FN status_callback);
  ~ConcordClientPool();

  SubmitResult SendRequest(const void* request, std::uint32_t request_size,
                           bftEngine::ClientMsgFlag flags,
                           std::chrono::milliseconds timeout_ms,
                           std::uint32_t reply_size, void* out_reply,
                           std::uint32_t* out_actual_reply_size);

  SubmitResult SendARequest(const void* request, std::uint32_t request_size,
                            bftEngine::ClientMsgFlag flags,
                            std::chrono::milliseconds timeout_ms,
                            std::uint32_t reply_size, void* out_reply,
                            std::uint32_t* out_actual_reply_size,
                            const std::string& correlation_id);

  bool LockClientsQueue() {
    while (!clients_queue_lock_.try_lock())
      ;
    return true;
  }

  void UnlockClientsQueue() { clients_queue_lock_.unlock(); }

  void InsertClientToQueue(std::shared_ptr<ConcordClient> client) {
    clients_.push(client);
  }
};

class ConcordClientProcessingJob : public util::SimpleThreadPool::Job {
 private:
  ConcordClientPool* clients_;
  std::shared_ptr<ConcordClient> client_;
  const void* request_;
  std::uint32_t request_size_;
  bftEngine::ClientMsgFlag flags_;
  std::chrono::milliseconds timeout_ms_;
  std::uint32_t reply_size_;
  void* out_reply_;
  std::uint32_t* out_actual_reply_size_;
  std::string& correlation_id_;

 public:
  ConcordClientProcessingJob(ConcordClientPool* clients,
                             std::shared_ptr<ConcordClient> client,
                             const void* request, std::uint32_t request_size,
                             bftEngine::ClientMsgFlag flags,
                             std::chrono::milliseconds timeout_ms,
                             std::uint32_t reply_size, void* out_reply,
                             std::uint32_t* out_actual_reply_size,
                             const std::string& correlation_id)
      : clients_{clients},
        client_{client},
        request_{request},
        request_size_{request_size},
        flags_{flags},
        timeout_ms_{timeout_ms},
        reply_size_{reply_size},
        out_reply_{out_reply},
        out_actual_reply_size_{out_actual_reply_size},
        correlation_id_{const_cast<std::string&>(correlation_id)} {};

  virtual ~ConcordClientProcessingJob() {}

  virtual void release() override { delete this; }

  virtual void execute() override {
    client_->SendRequestASync(request_, request_size_, flags_, timeout_ms_,
                              reply_size_, out_reply_, out_actual_reply_size_,
                              correlation_id_);
    clients_->LockClientsQueue();
    clients_->InsertClientToQueue(client_);
    clients_->UnlockClientsQueue();
  }
};

}  // namespace external_client
}  // namespace concord
