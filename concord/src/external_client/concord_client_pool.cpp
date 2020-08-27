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

#include <utility>

namespace concord::concord_client_pool {

using bftEngine::ClientMsgFlag;
using config::ConcordConfiguration;
using namespace config_pool;
using namespace bftEngine;

SubmitResult ConcordClientPool::SendRequest(
    std::vector<uint8_t> &&request, ClientMsgFlag flags,
    std::chrono::milliseconds timeout_ms, char *reply_buffer,
    std::uint32_t max_reply_size, uint64_t seq_num, std::string correlation_id,
    std::string span_context) {
  std::shared_ptr<external_client::ConcordClient> client;
  {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
    if (!clients_.empty()) {
      // start thread with client
      client = clients_.front();
      if (is_overloaded_) {
        std::chrono::steady_clock::time_point end =
            std::chrono::steady_clock::now();
        waiting_time_summary_.Observe(
            std::chrono::duration_cast<std::chrono::milliseconds>(
                end - client->getWaitingTime())
                .count());
        is_overloaded_ = false;
      }
      clients_.pop_front();
    } else {
      rejected_counter_.Increment();
      is_overloaded_ = true;
      LOG_ERROR(logger_, "Cannot allocate client for cid=" << correlation_id);
      return SubmitResult::Overloaded;
    }
  }

  if (seq_num > 0)
    client->setClientSeqNum(seq_num);
  else
    client->generateClientSeqNum();

  if (max_reply_size) client->setReplyBuffer(reply_buffer, max_reply_size);

  LOG_INFO(
      logger_,
      "client_id=" << client->getClientId() << " starts handling reqSeqNum="
                   << client->getClientSeqNum() << " cid=" << correlation_id
                   << "span_context exists=" << !span_context.empty()
                   << " flags=" << flags << " request_size=" << request.size()
                   << " timeout_ms=" << timeout_ms.count());

  client->setStartRequestTime();
  auto *job = new ConcordClientProcessingJob(
      *this, client, std::move(request), flags, timeout_ms, max_reply_size,
      correlation_id, client->getClientSeqNum(), span_context);
  requests_counter_.Increment();
  clients_gauge_.Decrement();
  jobs_thread_pool_.add(job);
  return SubmitResult::Acknowledged;
}

SubmitResult ConcordClientPool::SendRequest(
    const bft::client::WriteConfig &config, bft::client::Msg &&request) {
  LOG_INFO(logger_, "Write request generated with cid="
                        << config.request.correlation_id);
  auto request_flag = ClientMsgFlag::EMPTY_FLAGS_REQ;
  if (config.request.pre_execute) request_flag = ClientMsgFlag::PRE_PROCESS_REQ;
  return SendRequest(
      std::forward<std::vector<uint8_t>>(request), request_flag,
      config.request.timeout, nullptr, 0, config.request.sequence_number,
      config.request.correlation_id, config.request.span_context);
}

SubmitResult ConcordClientPool::SendRequest(
    const bft::client::ReadConfig &config, bft::client::Msg &&request) {
  LOG_INFO(logger_,
           "Read request generated with cid=" << config.request.correlation_id);
  return SendRequest(
      std::forward<std::vector<uint8_t>>(request), ClientMsgFlag::READ_ONLY_REQ,
      config.request.timeout, nullptr, 0, config.request.sequence_number,
      config.request.correlation_id, config.request.span_context);
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
                                .Name("total_available_external_clients")
                                .Help("counts available clients")
                                .Register(*registry_)),
      last_request_time_gauges_(prometheus::BuildGauge()
                                    .Name("last_request_time")
                                    .Help("calculates the last request time")
                                    .Register(*registry_)),
      waiting_time_summaries_(
          prometheus::BuildSummary()
              .Name("client_wait_time_after_overload")
              .Help("Observes the amount of time that client is waiting on "
                    "waiting to system to get back from overloaded wait time")
              .Register(*registry_)),
      requests_counter_(total_requests_counters_.Add({{"item", "counter"}})),
      rejected_counter_(rejected_requests_counters_.Add({{"item", "counter"}})),
      clients_gauge_(total_clients_gauges_.Add({{"item", "client_updates"}})),
      last_request_time_gauge_(
          last_request_time_gauges_.Add({{"item", "time_updates"}})),
      waiting_time_summary_(waiting_time_summaries_.Add(
          {{"item", "wait_time_updates"}},
          prometheus::Summary::Quantiles({{0.25, 0.1},
                                          {0.5, 0.1},
                                          {0.75, 0.1},
                                          {0.9, 0.1},
                                          {0.95, 0.1}}))),
      logger_(logging::getLogger("com.vmware.external_client_pool")) {
  ConcordConfiguration config;
  try {
    CreatePool(config_stream, config);
  } catch (config::ConfigurationResourceNotFoundException &e) {
    throw InternalError();
  } catch (std::invalid_argument &e) {
    LOG_ERROR(logger_, "Communication protocol="
                           << config.getValue<std::string>(
                                  config_pool::ClientPoolConfig().COMM_PROTOCOL)
                           << " is not supported");
    throw InternalError();
  } catch (config::InvalidConfigurationInputException &e) {
    throw InternalError();
  }
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
                                .Name("total_available_external_clients")
                                .Help("counts available clients")
                                .Register(*registry_)),
      last_request_time_gauges_(prometheus::BuildGauge()
                                    .Name("last_request_time")
                                    .Help("calculates the last request time")
                                    .Register(*registry_)),
      waiting_time_summaries_(
          prometheus::BuildSummary()
              .Name("client_wait_time_after_overload")
              .Help("Observes the amount of time that client is waiting on "
                    "waiting to system to get back from overloaded wait time")
              .Register(*registry_)),
      requests_counter_(total_requests_counters_.Add({{"item", "counter"}})),
      rejected_counter_(rejected_requests_counters_.Add({{"item", "counter"}})),
      clients_gauge_(total_clients_gauges_.Add({{"item", "client_updates"}})),
      last_request_time_gauge_(
          last_request_time_gauges_.Add({{"item", "time_updates"}})),
      waiting_time_summary_(waiting_time_summaries_.Add(
          {{"item", "wait_time_updates"}},
          prometheus::Summary::Quantiles({{0.25, 0.1},
                                          {0.5, 0.1},
                                          {0.75, 0.1},
                                          {0.9, 0.1},
                                          {0.95, 0.1}}))),
      logger_(logging::getLogger("com.vmware.external_client_pool")) {
  std::ifstream config_file;
  config_file.exceptions(std::ifstream::failbit | std::fstream::badbit);
  try {
    config_file.open(config_file_path.data());
  } catch (const std::ifstream::failure &e) {
    LOG_ERROR(logger_, "Could not find the configuration file at path="
                           << config_file_path);
    throw InternalError();
  }
  ConcordConfiguration config;
  try {
    CreatePool(config_file, config);
  } catch (config::ConfigurationResourceNotFoundException &e) {
    throw InternalError();
  } catch (std::invalid_argument &e) {
    LOG_ERROR(logger_, "Communication module="
                           << config.getValue<std::string>(
                                  config_pool::ClientPoolConfig().COMM_PROTOCOL)
                           << " configured in the file=" << config_file_path
                           << " is not supported");
    throw InternalError();
  } catch (config::InvalidConfigurationInputException &e) {
    throw InternalError();
  }
}

void ConcordClientPool::setUpClientParams(SimpleClientParams &client_params,
                                          const ConcordConfiguration &config,
                                          const ClientPoolConfig &pool_config) {
  client_params.clientInitialRetryTimeoutMilli =
      config.getValue<uint64_t>(pool_config.INITIAL_RETRY_TIMEOUT);
  client_params.clientMinRetryTimeoutMilli =
      config.getValue<uint64_t>(pool_config.MIN_RETRY_TIMEOUT);
  client_params.clientMaxRetryTimeoutMilli =
      config.getValue<uint64_t>(pool_config.MAX_RETRY_TIMEOUT);
  client_params.numberOfStandardDeviationsToTolerate =
      config.getValue<uint16_t>(pool_config.STANDARD_DEVIATIONS_TO_TOLERATE);
  client_params.samplesPerEvaluation =
      config.getValue<uint16_t>(pool_config.SAMPLES_PER_EVALUATION);
  client_params.samplesUntilReset =
      config.getValue<uint16_t>(pool_config.SAMPLES_UNTIL_RESET);
  client_params.clientSendsRequestToAllReplicasFirstThresh =
      config.getValue<uint16_t>(pool_config.FIRST_THRESH);
  client_params.clientSendsRequestToAllReplicasPeriodThresh =
      config.getValue<uint16_t>(pool_config.PERIODIC_THRESH);
  client_params.clientPeriodicResetThresh =
      config.getValue<uint16_t>(pool_config.RESET_THRESH);
  LOG_INFO(logger_,
           "clientInitialRetryTimeoutMilli="
               << client_params.clientInitialRetryTimeoutMilli
               << " clientMinRetryTimeoutMilli="
               << client_params.clientMinRetryTimeoutMilli
               << " clientMaxRetryTimeoutMilli="
               << client_params.clientMaxRetryTimeoutMilli
               << " numberOfStandardDeviationsToTolerate="
               << client_params.numberOfStandardDeviationsToTolerate
               << " samplesPerEvaluation=" << client_params.samplesPerEvaluation
               << " samplesUntilReset=" << client_params.samplesUntilReset
               << " clientSendsRequestToAllReplicasFirstThresh="
               << client_params.clientSendsRequestToAllReplicasFirstThresh
               << " clientSendsRequestToAllReplicasPeriodThresh="
               << client_params.clientSendsRequestToAllReplicasPeriodThresh
               << " clientPeriodicResetThresh="
               << client_params.clientPeriodicResetThresh);
}

void ConcordClientPool::CreatePool(std::istream &config_stream,
                                   ConcordConfiguration &config) {
  auto pool_config = std::make_unique<config_pool::ClientPoolConfig>();
  ConfigInit(config, *pool_config, config_stream);
  PrometheusInit(config, *pool_config);
  auto num_clients =
      config.getValue<std::uint16_t>(pool_config->NUM_EXTERNAL_CLIENTS);
  clients_gauge_.Set(num_clients);
  LOG_INFO(logger_, "Creating pool: num_clients=" << num_clients);
  auto f_val = config.getValue<uint16_t>(pool_config->F_VAL);
  auto c_val = config.getValue<uint16_t>(pool_config->C_VAL);
  auto clients_per_replica =
      config.getValue<uint16_t>(pool_config->CLIENT_PROXIES_PER_REPLICA);
  auto max_buf_size =
      stol(config.getValue<std::string>(pool_config->COMM_BUFF_LEN));
  const auto num_replicas = 3 * f_val + 2 * c_val + 1;
  const auto required_num_of_replicas = 2 * f_val + 1;
  external_client::ConcordClient::setStatics(required_num_of_replicas,
                                             num_replicas, max_buf_size);
  auto num_of_principals = num_replicas * clients_per_replica + num_replicas;
  bftEngine::SimpleClientParams clientParams;
  setUpClientParams(clientParams, config, *pool_config);
  for (int i = 0; i < num_clients; i++) {
    auto client_id = num_of_principals + i;
    LOG_DEBUG(logger_, "Creating client_id=" << client_id);
    clients_.push_back(std::make_shared<external_client::ConcordClient>(
        config, i, *pool_config, clientParams));
  }
  jobs_thread_pool_.start(num_clients);
}

void ConcordClientPool::PrometheusInit(
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
  LOG_INFO(logger_, "BFT-Client pool metrics will expose to " << bind_address
                                                              << "/metrics");
}

void ConcordClientPool::ConfigInit(config::ConcordConfiguration &config,
                                   ClientPoolConfig &pool_config,
                                   std::istream &config_stream) {
  pool_config.ParseConfig(config_stream, config);
}

ConcordClientPool::~ConcordClientPool() {
  jobs_thread_pool_.stop(true);
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  clients_.clear();
  LOG_INFO(logger_, "Clients cleanup complete");
}

void ConcordClientPool::SetDoneCallback(EXT_DONE_CALLBACK cb) {
  done_callback_ = std::move(cb);
}

void ConcordClientPool::Done(const uint64_t sn, const std::string cid,
                             uint32_t reply_size) {
  if (done_callback_) {
    done_callback_(sn, cid, reply_size);
  }
}

void ConcordClientProcessingJob::execute() {
  uint32_t reply_size = processing_client_->SendRequest(
      request_.data(), request_.size(), flags_, timeout_ms_, reply_size_,
      seq_num_, correlation_id_, span_context_);
  clients_pool_.InsertClientToQueue(processing_client_, seq_num_,
                                    correlation_id_, reply_size);
}

void ConcordClientPool::InsertClientToQueue(
    std::shared_ptr<concord::external_client::ConcordClient> &client,
    uint64_t seq_num, const std::string &correlation_id, uint32_t reply_size) {
  {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
    if (is_overloaded_) {
      client->setStartWaitingTime();
    }
    clients_.push_back(client);
  }
  Done(seq_num, correlation_id, reply_size);
  LOG_INFO(logger_, "client_id=" << client->getClientId()
                                 << " has completed handling reqSeqNum="
                                 << seq_num << " cid=" << correlation_id
                                 << " reply_size=" << reply_size);
  std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
  last_request_time_gauge_.Set(
      std::chrono::duration_cast<std::chrono::milliseconds>(
          end - client->getStartRequestTime())
          .count());
  clients_gauge_.Increment();
}

PoolStatus ConcordClientPool::HealthStatus() {
  std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
  for (auto &client : clients_) {
    if (client->isServing()) {
      LOG_DEBUG(logger_, "client=" << client->getClientId()
                                   << " is serving - pool is ready to serve");
      return PoolStatus::Serving;
    }
  }
  LOG_DEBUG(logger_,
            "All clients are not serving - pool is not ready to serve");
  return PoolStatus::NotServing;
}

}  // namespace concord::concord_client_pool
