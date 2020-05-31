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
  if (internalError_) return SubmitResult::InternalError;
  std::shared_ptr<external_client::ConcordClient> client = nullptr;
  {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
    if (!clients_.empty()) {
      // start thread with client
      client = clients_.front();
      clients_.pop_front();
    } else {
      rejected_counter_.Increment();
      LOG4CPLUS_ERROR(logger_,
                      "Cannot allocate client for cid=" << correlation_id);
      return SubmitResult::Overloaded;
    }
  }
  client->generateClientSeqNum();
  LOG4CPLUS_INFO(
      logger_,
      "client_id=" << client->getClientId() << " allocated, insert reqSeqNum="
                   << client->getClientSeqNum() << " with cid="
                   << correlation_id << " with request size of=" << reply_size
                   << " and time out on ms=" << timeout_ms.count()
                   << " to the job pool");
  client->setStartRequestTime();
  std::unique_ptr<ConcordClientProcessingJob> job =
      std::make_unique<ConcordClientProcessingJob>(
          *this, move(client), request, request_size, flags, timeout_ms,
          reply_size, out_reply, out_actual_reply_size, correlation_id,
          client->getClientSeqNum());
  requests_counter_.Increment();
  clients_gauge_.Decrement();
  jobs_thread_pool_.add(job.release());
  return SubmitResult::Acknowledged;
}

SubmitResult ConcordClientPool::SendRequest(
    const bft::client::WriteConfig &config, bft::client::Msg &&request) {
  if (config.request.pre_execute)
    return SendRequest(request.data(), request.size(),
                       ClientMsgFlag::PRE_PROCESS_REQ, config.request.timeout,
                       config.request.max_reply_size, reply_->data(),
                       reinterpret_cast<uint32_t *>(reply_->size()),
                       config.request.correlation_id);
  else
    return SendRequest(request.data(), request.size(),
                       ClientMsgFlag::EMPTY_FLAGS_REQ, config.request.timeout,
                       config.request.max_reply_size, reply_->data(),
                       reinterpret_cast<uint32_t *>(reply_->size()),
                       config.request.correlation_id);
}

SubmitResult ConcordClientPool::SendRequest(
    const bft::client::ReadConfig &config, bft::client::Msg &&request) {
  return SendRequest(request.data(), request.size(),
                     ClientMsgFlag::READ_ONLY_REQ, config.request.timeout,
                     config.request.max_reply_size, reply_->data(),
                     reinterpret_cast<uint32_t *>(reply_->size()),
                     config.request.correlation_id);
}

ConcordClientPool::ConcordClientPool(std::istream &config_stream)
    : registry_(std::make_shared<prometheus::Registry>()),
      total_requests_counters_(prometheus::BuildCounter()
                                   .Name("total_external_clients_requests")
                                   .Help("counts requests from external client")
                                   .Register(*registry_)),
      rejected_requests_counters_(prometheus::BuildCounter()
                                      .Name("total_overloaded_requests")
                                      .Help("counts rejected requests")
                                      .Register(*registry_)),
      total_clients_gauges_(prometheus::BuildGauge()
                                .Name("total_used_external_clients")
                                .Help("counts used clients")
                                .Register(*registry_)),
      last_request_time_gauges_(prometheus::BuildGauge()
                                    .Name("average_time_for_requests")
                                    .Help("calculates the average request time")
                                    .Register(*registry_)),
      requests_counter_(total_requests_counters_.Add({{"item", "counter"}})),
      rejected_counter_(rejected_requests_counters_.Add({{"item", "counter"}})),
      clients_gauge_(total_clients_gauges_.Add({{"item", "client_updates"}})),
      last_request_time_gauge_(
          last_request_time_gauges_.Add({{"item", "time_updates"}})),
      logger_(
          log4cplus::Logger::getInstance("com.vmware.external_client_pool")) {
  ConcordConfiguration config;
  CreatePool(config_stream, config);
}

ConcordClientPool::ConcordClientPool(std::string config_file_path)
    : registry_(std::make_shared<prometheus::Registry>()),
      total_requests_counters_(prometheus::BuildCounter()
                                   .Name("total_external_clients_requests")
                                   .Help("counts requests from external client")
                                   .Register(*registry_)),
      rejected_requests_counters_(prometheus::BuildCounter()
                                      .Name("total_overloaded_requests")
                                      .Help("counts rejected requests")
                                      .Register(*registry_)),
      total_clients_gauges_(prometheus::BuildGauge()
                                .Name("total_used_external_clients")
                                .Help("counts used clients")
                                .Register(*registry_)),
      last_request_time_gauges_(prometheus::BuildGauge()
                                    .Name("average_time_for_requests")
                                    .Help("calculates the average request time")
                                    .Register(*registry_)),
      requests_counter_(total_requests_counters_.Add({{"item", "counter"}})),
      rejected_counter_(rejected_requests_counters_.Add({{"item", "counter"}})),
      clients_gauge_(total_clients_gauges_.Add({{"item", "client_updates"}})),
      last_request_time_gauge_(
          last_request_time_gauges_.Add({{"item", "time_updates"}})),
      logger_(
          log4cplus::Logger::getInstance("com.vmware.external_client_pool")) {
  std::ifstream config_file;
  config_file.exceptions(std::ifstream::failbit | std::fstream::badbit);
  config_file.open(config_file_path.data());
  ConcordConfiguration config;
  try {
    CreatePool(config_file, config);
  } catch (config::ConfigurationResourceNotFoundException e) {
    LOG4CPLUS_ERROR(logger_, "Could not parse the configuration file at path="
                                 << config_file_path);
    throw InternalError;
  } catch (std::invalid_argument &e) {
    LOG4CPLUS_ERROR(
        logger_, "Communication module="
                     << config.getValue<std::string>(
                            config_pool::ClientPoolConfig().COMM_PROTOCOL)
                     << " on file=" << config_file_path << " is not supported");
    throw InternalError;
  }
}

void ConcordClientPool::CreatePool(std::istream &config_stream,
                                   ConcordConfiguration &config) {
  auto pool_config = std::make_unique<config_pool::ClientPoolConfig>();
  Config_Initialize(config, *pool_config.get(), config_stream);
  Prometheus_Initialize(config, *pool_config.get());
  uint16_t num_clients =
      config.getValue<std::uint16_t>(pool_config->NUM_EXTERNAL_CLIENTS);
  clients_gauge_.Set(num_clients);
  LOG4CPLUS_INFO(logger_, "Creating pool of num_clients=" << num_clients);
  for (int i = 0; i < num_clients; i++) {
    LOG4CPLUS_DEBUG(logger_, "Creating client_id=" << i);
    clients_.push_back(std::make_shared<external_client::ConcordClient>(
        config, i, *pool_config.get()));
  }
  jobs_thread_pool_.start(num_clients);
}

void ConcordClientPool::Prometheus_Initialize(
    const config::ConcordConfiguration &config,
    const config_pool::ClientPoolConfig &pool_config) {
  std::string bind_port =
      ":" + config.getValue<std::string>(pool_config.PROMETHEUS_PORT);
  std::string bind_address =
      config.subscope(pool_config.PARTICIPANT_NODES, 0)
          .subscope(pool_config.PARTICIPANT_NODE, 0)
          .getValue<std::string>(pool_config.PROMETHEUS_HOST) +
      bind_port;
  exposer_ = std::make_shared<prometheus::Exposer>(bind_address, "/metrics", 1);
  exposer_->RegisterCollectable(registry_);
  LOG4CPLUS_INFO(logger_, "BFT-Client pool metrics will expose to "
                              << bind_address << "/metrics");
}

void ConcordClientPool::Config_Initialize(config::ConcordConfiguration &config,
                                          ClientPoolConfig &pool_config,
                                          std::istream &config_stream) {
  pool_config.ParseConfig(config_stream, config);
}

ConcordClientPool::~ConcordClientPool() {
  jobs_thread_pool_.stop(true);
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  clients_.clear();
  LOG4CPLUS_INFO(logger_, "Clients cleanup complete");
}

void ConcordClientProcessingJob::execute() {
  auto res = processing_client_->SendRequest(
      request_.c_str(), request_size_, flags_, timeout_ms_, reply_size_,
      out_reply_, out_actual_reply_size_, seq_num_, correlation_id_);
  result_ = res;
  if (res != 0) clients_pool_.turnInternalErrorOn();
  clients_pool_.InsertClientToQueue(processing_client_, seq_num_,
                                    correlation_id_);
}

void ConcordClientPool::InsertClientToQueue(
    std::shared_ptr<concord::external_client::ConcordClient> &client,
    const uint64_t seq_num, const std::string &correlation_id) {
  {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
    clients_.push_back(client);
  }
  LOG4CPLUS_INFO(logger_,
                 "reqSeqNum=" << seq_num << "with cid=" << correlation_id
                              << "has ended.returns client_id="
                              << client->getClientId() << " to the pool");
  std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
  last_request_time_gauge_.Set(
      std::chrono::duration_cast<std::chrono::milliseconds>(
          end - client->getStartRequestTime())
          .count());
  clients_gauge_.Increment();
}

PoolStatus ConcordClientPool::HealthStatus() {
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  for (auto &client : clients_)
    if (client->isRunning()) return PoolStatus::Serving;
  return PoolStatus::NotServing;
}

void ConcordClientPool::turnInternalErrorOn() { internalError_ = true; }

}  // namespace concord_client_pool
}  // namespace concord
