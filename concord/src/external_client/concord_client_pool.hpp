// Copyright 2020 VMware, all rights reserved

#pragma once

#include <SimpleThreadPool.hpp>
#include <bftengine/SimpleClient.hpp>
#include <chrono>
#include <memory>
#include <mutex>
#include <queue>
#include <string_view>
#include <utility>
#include "external_client.hpp"

namespace concord {
namespace external_client {
class ConcordClient;
}
namespace concord_client_pool {
// Represents the answer that the DAML Ledger API could get when sending a
// request
enum SubmitResult {
  Acknowledged,  // The request has been queued for submission
  Overloaded,    // There is no available client to the moment to process the
  // request
  InternalError  // An internal error has occurred. Reason is recorded in logs.
};

// Represents a Concord BFT client pool. The purpose of this class is to be easy
// to use for external users. This is achieved by:
//  * providing a simple public interface
//  * providing a generic public interface that allows for various use cases
//  * configuration via a file - users don't need to know what the structure of
//  the file is and changes to the file will not affect the interface of the
//  client
class ConcordClientPool {
 public:
  ConcordClientPool(std::string_view config_file_path);

  ConcordClientPool(std::istream& config_stream);

  ~ConcordClientPool();

  SubmitResult SendASyncRequest(const void* request, std::uint32_t request_size,
                                bftEngine::ClientMsgFlag flags,
                                std::chrono::milliseconds timeout_ms,
                                std::uint32_t reply_size, void* out_reply,
                                std::uint32_t* out_actual_reply_size,
                                const std::string& correlation_id = {});

  void InsertClientToQueue(
      std::shared_ptr<concord::external_client::ConcordClient> client) {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
    clients_.push(client);
  }

 private:
  // Clients that are available for use (i.e. not already in use).
  std::queue<std::shared_ptr<external_client::ConcordClient>> clients_;
  // Thread pool, on each thread on client will run
  util::SimpleThreadPool jobs_thread_pool_;
  // Clients queue mutex
  std::mutex clients_queue_lock_;
};

class ConcordClientProcessingJob : public util::SimpleThreadPool::Job {
 public:
  ConcordClientProcessingJob(
      concord_client_pool::ConcordClientPool& clients,
      std::shared_ptr<external_client::ConcordClient> client,
      const void* request, std::uint32_t request_size,
      bftEngine::ClientMsgFlag flags, std::chrono::milliseconds timeout_ms,
      std::uint32_t reply_size, void* out_reply,
      std::uint32_t* out_actual_reply_size, const std::string& correlation_id)
      : clients_{clients},
        client_{std::move(client)},
        request_{request},
        request_size_{request_size},
        flags_{flags},
        timeout_ms_{timeout_ms},
        reply_size_{reply_size},
        out_reply_{out_reply},
        out_actual_reply_size_{out_actual_reply_size},
        correlation_id_{const_cast<std::string&>(correlation_id)} {};

  virtual ~ConcordClientProcessingJob() {}

  void release() override { delete this; }

  void execute() override;

 private:
  concord_client_pool::ConcordClientPool& clients_;
  std::shared_ptr<external_client::ConcordClient> client_;
  const void* request_;
  std::uint32_t request_size_;
  bftEngine::ClientMsgFlag flags_;
  std::chrono::milliseconds timeout_ms_;
  std::uint32_t reply_size_;
  void* out_reply_;
  std::uint32_t* out_actual_reply_size_;
  const std::string& correlation_id_;
};
}  // namespace concord_client_pool

}  // namespace concord