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


#include "concord_client_pool.hpp"

namespace concord {

namespace concord_client_pool {

using bftEngine::ClientMsgFlag;
using config::ConcordConfiguration;
using namespace config_pool;

SubmitResult ConcordClientPool::SendRequest(
    const void *request, std::uint32_t request_size, ClientMsgFlag flags,
    std::chrono::milliseconds timeout_ms, std::uint32_t reply_size,
    void *out_reply, std::uint32_t *out_actual_reply_size,
    const std::string &correlation_id) {
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  if (!clients_.empty()) {
    // start thread with client
    auto &client = clients_.front();
    clients_.pop();
    std::unique_ptr<ConcordClientProcessingJob> job =
        std::make_unique<ConcordClientProcessingJob>(
            *this, move(client), request, request_size, flags, timeout_ms,
            reply_size, out_reply, out_actual_reply_size, correlation_id);
    clients_lock.unlock();
    jobs_thread_pool_.add(job.get());
    return SubmitResult::Acknowledged;
  }
  return SubmitResult::Overloaded;
}

ConcordClientPool::ConcordClientPool(std::istream &config_stream) {
  ConcordConfiguration config;
  concord::config_pool::ParseConfig(config_stream, config);
  uint16_t num_clients = config.getValue<std::uint16_t>(NUM_EXTERNAL_CLIENTS);
  for (int i = 0; i < num_clients; i++) {
    auto client = std::make_shared<external_client::ConcordClient>(config, i);
    clients_.push(std::move(client));
  }
  jobs_thread_pool_.start(num_clients);
}

ConcordClientPool::ConcordClientPool(std::string config_file_path) {
  std::ifstream config_file;
  config_file.exceptions(std::ifstream::failbit | std::fstream::badbit);
  config_file.open(config_file_path.data());
  ConcordClientPool((std::istream &)config_file);
}

ConcordClientPool::~ConcordClientPool() {
  jobs_thread_pool_.stop();
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  while (!clients_.empty()) {
    clients_.pop();
  }
}

void ConcordClientProcessingJob::execute() {
  processing_client_->SendRequest(request_, request_size_, flags_, timeout_ms_,
                                  reply_size_, out_reply_,
                                  out_actual_reply_size_, correlation_id_);
  clients_pool_.InsertClientToQueue(processing_client_);
}

void ConcordClientPool::InsertClientToQueue(
    std::shared_ptr<concord::external_client::ConcordClient> &client) {
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  clients_.push(client);
}
}  // namespace concord_client_pool
}  // namespace concord
