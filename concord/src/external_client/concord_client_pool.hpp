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
#include <prometheus/counter.h>
#include <prometheus/exposer.h>
#include <prometheus/registry.h>
#include <prometheus/summary.h>
#include <config/configuration_manager.hpp>
#include <functional>
#include <mutex>
#include <queue>
#include "SimpleThreadPool.hpp"
#include "bftclient/base_types.h"
#include "bftclient/config.h"
#include "bftclient/quorums.h"
#include "external_client.hpp"

// the parameters are sequence number and cid
typedef std::function<void(const uint64_t& /* seq_num */,
                           const std::string /* cid */,
                           uint32_t /*reply_size*/)>
    EXT_DONE_CALLBACK;

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
};
// An internal error has occurred. Reason is recorded in logs.
class InternalError : public std::exception {
 public:
  InternalError() = default;
  const char* what() const noexcept override {
    return "Internal error occurred, please check the log files";
  }
};

// Represents the answer that the DAML Ledger API could get when sending a
// request
enum PoolStatus {
  Serving,     // At least one client is running
  NotServing,  // All clients not running
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
  // Constructs the clients pool by passing absolute path to concord
  // configuration file.
  // Construction executes all needed steps to provide a ready-to-use
  // object (including starting internal threads, if needed).
  explicit ConcordClientPool(std::string config_file_path);
  // constructor that gets the configuration on istream type,
  // helps on testing
  explicit ConcordClientPool(std::istream& config_stream);

  ~ConcordClientPool();
  // This method is responsible to deal with requests in an asynchronous way,
  // for each request that comes, we will check if there is an available client
  // to deal with the problem if there is a client the request enters into a
  // thread pool and a positive answer is immediately returned to the
  // application. If there is no available client, a negative answer is returned
  // to the application.
  // request - a vector that holds the request from the client application.
  // flags - holds the request flag (EMPTY_FLAG, READ_ONLY, PRE_PROCESS).
  // timeout_ms - the request timeout which specifies for how long a client
  // should wait for the request execution response.
  // reply_buffer - client application allocated buffer that stores returned
  // response.
  // max_reply_size - holds the size of reply_buffer.
  // seq_num - sequence number for the request
  SubmitResult SendRequest(std::vector<uint8_t>&& request,
                           bftEngine::ClientMsgFlag flags,
                           std::chrono::milliseconds timeout_ms,
                           char* reply_buffer, std::uint32_t max_reply_size,
                           uint64_t seq_num, std::string correlation_id = {},
                           std::string span_context = std::string());

  // This method is responsible to get write requests with the new client
  // paramters and parse it to the old SimpleClient interface.
  SubmitResult SendRequest(const bft::client::WriteConfig& config,
                           bft::client::Msg&& request);
  // This method is responsible to get read requests with the new client
  // paramters and parse it to the old SimpleClient interface.
  SubmitResult SendRequest(const bft::client::ReadConfig& config,
                           bft::client::Msg&& request);

  void InsertClientToQueue(
      std::shared_ptr<concord::external_client::ConcordClient>& client,
      uint64_t seq_num, const std::string& correlation_id, uint32_t reply_size);

  PoolStatus HealthStatus();

  void Done(uint64_t sn, const std::string cid, uint32_t reply_size);

  void SetDoneCallback(EXT_DONE_CALLBACK cb);

 private:
  void setUpClientParams(bftEngine::SimpleClientParams& client_params,
                         const config::ConcordConfiguration& config,
                         const config_pool::ClientPoolConfig& pool_config);
  void CreatePool(std::istream& config_stream,
                  config::ConcordConfiguration& config);
  void PrometheusInit(const config::ConcordConfiguration& config,
                      const config_pool::ClientPoolConfig& pool_config);
  static void ConfigInit(config::ConcordConfiguration& config,
                         config_pool::ClientPoolConfig& pool_config,
                         std::istream& config_stream);
  // Clients that are available for use (i.e. not already in use).
  std::deque<std::shared_ptr<external_client::ConcordClient>> clients_;
  // Thread pool, on each thread on client will run
  util::SimpleThreadPool jobs_thread_pool_;
  // Clients queue mutex
  std::mutex clients_queue_lock_;
  // Metric
  std::shared_ptr<prometheus::Exposer> exposer_;
  std::shared_ptr<prometheus::Registry> registry_;
  prometheus::Family<prometheus::Counter>& total_requests_counters_;
  prometheus::Family<prometheus::Counter>& rejected_requests_counters_;
  prometheus::Family<prometheus::Gauge>& total_clients_gauges_;
  prometheus::Family<prometheus::Gauge>& last_request_time_gauges_;
  prometheus::Family<prometheus::Summary>& waiting_time_summaries_;
  prometheus::Counter& requests_counter_;
  prometheus::Counter& rejected_counter_;
  prometheus::Gauge& clients_gauge_;
  prometheus::Gauge& last_request_time_gauge_;
  prometheus::Summary& waiting_time_summary_;
  // Logger
  logging::Logger logger_;
  std::atomic_bool is_overloaded_ = false;
  EXT_DONE_CALLBACK done_callback_ = nullptr;
};

class ConcordClientProcessingJob : public util::SimpleThreadPool::Job {
 public:
  ConcordClientProcessingJob(
      concord_client_pool::ConcordClientPool& clients,
      std::shared_ptr<external_client::ConcordClient> client,
      std::vector<uint8_t>&& request, bftEngine::ClientMsgFlag flags,
      std::chrono::milliseconds timeout_ms, std::uint32_t reply_size,
      const std::string correlation_id, uint64_t seq_num,
      std::string span_context)
      : clients_pool_{clients},
        processing_client_{std::move(client)},
        request_(std::move(request)),
        flags_{flags},
        timeout_ms_{timeout_ms},
        reply_size_{reply_size},
        correlation_id_{correlation_id},
        span_context_{span_context},
        seq_num_{seq_num} {};

  virtual ~ConcordClientProcessingJob() = default;

  void release() override { delete this; }

  void execute() override;

 private:
  concord_client_pool::ConcordClientPool& clients_pool_;
  std::shared_ptr<external_client::ConcordClient> processing_client_;
  std::vector<uint8_t> request_;
  bftEngine::ClientMsgFlag flags_;
  std::chrono::milliseconds timeout_ms_;
  std::uint32_t reply_size_;
  const std::string correlation_id_;
  std::string span_context_;
  uint64_t seq_num_;
};
}  // namespace concord_client_pool

}  // namespace concord
