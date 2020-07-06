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
    std::vector<char> &&request, ClientMsgFlag flags,
    std::chrono::milliseconds timeout_ms, std::uint32_t reply_size,
    char *extReplyBuffer, std::uint32_t extReplySize,
    std::string correlation_id) {
  std::shared_ptr<external_client::ConcordClient> client;
  {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
    if (!clients_.empty()) {
      // start thread with client
      client = clients_.front();
      clients_.pop_front();
    } else {
      rejected_counter_.Increment();
      LOG_ERROR(logger_, "Cannot allocate client for cid=" << correlation_id);
      return SubmitResult::Overloaded;
    }
  }
  client->generateClientSeqNum();
  if (extReplyBuffer)
    client->setExternalReplyBuffer(extReplyBuffer, extReplySize);
  LOG_INFO(logger_, "client_id=" << client->getClientId()
                                 << " starts handling reqSeqNum="
                                 << client->getClientSeqNum() << " cid="
                                 << correlation_id << " flags=" << flags
                                 << " request_size=" << request.size()
                                 << " timeout_ms=" << timeout_ms.count());
  client->setStartRequestTime();
  auto *job = new ConcordClientProcessingJob(
      *this, client, std::move(request), flags, timeout_ms, reply_size,
      correlation_id, client->getClientSeqNum());
  requests_counter_.Increment();
  clients_gauge_.Decrement();
  jobs_thread_pool_.add(job);
  return SubmitResult::Acknowledged;
}

SubmitResult ConcordClientPool::SendRequest(
    std::vector<char> &&request, ClientMsgFlag flags,
    std::chrono::milliseconds timeout_ms, std::uint32_t reply_size,
    const std::string correlation_id) {
  return SendRequest(std::forward<std::vector<char>>(request), flags,
                     timeout_ms, reply_size, nullptr, 0, correlation_id);
}

SubmitResult ConcordClientPool::SendRequest(
    const bft::client::WriteConfig &config, bft::client::Msg &&request) {
  LOG_INFO(logger_, "Write request generated with cid="
                        << config.request.correlation_id);
  auto request_flag = ClientMsgFlag::EMPTY_FLAGS_REQ;
  if (config.request.pre_execute) request_flag = ClientMsgFlag::PRE_PROCESS_REQ;
  return SendRequest(std::forward<std::vector<char>>(request), request_flag,
                     config.request.timeout, config.request.max_reply_size,
                     config.request.correlation_id);
}

SubmitResult ConcordClientPool::SendRequest(
    const bft::client::ReadConfig &config, bft::client::Msg &&request) {
  LOG_INFO(logger_,
           "Read request generated with cid=" << config.request.correlation_id);
  return SendRequest(std::forward<std::vector<char>>(request),
                     ClientMsgFlag::READ_ONLY_REQ, config.request.timeout,
                     config.request.max_reply_size,
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
      logger_(logging::getLogger("com.vmware.external_client_pool")) {
  std::ifstream config_file;
  config_file.exceptions(std::ifstream::failbit | std::fstream::badbit);
  config_file.open(config_file_path.data());
  ConcordConfiguration config;
  try {
    CreatePool(config_file, config);
  } catch (config::ConfigurationResourceNotFoundException &e) {
    LOG_ERROR(logger_, "Could not find the configuration file at path="
                           << config_file_path);
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
  auto max_buf_size =
      stol(config.getValue<std::string>(pool_config->COMM_BUFF_LEN));
  external_client::ConcordClient::setStatics(
      2 * f_val + 1, 3 * f_val + 2 * c_val + 1, max_buf_size);

  bftEngine::SimpleClientParams clientParams;
  setUpClientParams(clientParams, config, *pool_config);
  for (int i = 0; i < num_clients; i++) {
    LOG_DEBUG(logger_, "Creating client_id=" << i);
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
      seq_num_, correlation_id_);
  clients_pool_.InsertClientToQueue(processing_client_, seq_num_,
                                    correlation_id_, reply_size);
}

void ConcordClientPool::InsertClientToQueue(
    std::shared_ptr<concord::external_client::ConcordClient> &client,
    uint64_t seq_num, const std::string &correlation_id, uint32_t reply_size) {
  {
    std::unique_lock<std::mutex> clients_lock(clients_queue_lock_);
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
  for (auto &client : clients_)
    if (client->isServing()) return PoolStatus::Serving;
  return PoolStatus::NotServing;
}

}  // namespace concord::concord_client_pool
