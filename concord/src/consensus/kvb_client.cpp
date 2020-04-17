// Copyright 2018-2019 VMware, all rights reserved
//
// Layer between api_connection and concord::storage::IClient
//
// This is the end of the client side of Concord. Commands sent from here will
// end up at KVBCommandsHandler.

#include "kvb_client.hpp"
#include "SimpleClient.hpp"

#include <opentracing/tracer.h>
#include <boost/thread.hpp>

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ErrorResponse;
using google::protobuf::Duration;

using concord::time::TimePusher;
using std::chrono::steady_clock;
using namespace std::chrono_literals;

namespace concord {
namespace consensus {

void AddTracingContext(ConcordRequest &req, opentracing::Span &parent_span) {
  std::ostringstream req_context;
  parent_span.tracer().Inject(parent_span.context(), req_context);
  req.set_trace_context(req_context.str());
}

/**
 * Send a request to the replicas. Returns true if the response contains
 * something to forward (either a response message or an appropriate error
 * message). Returns false if the response is empty (for example, if parsing
 * failed).
 */
bool KVBClient::send_request_sync(ConcordRequest &req, uint8_t flags,
                                  std::chrono::milliseconds timeout,
                                  opentracing::Span &parent_span,
                                  ConcordResponse &resp,
                                  const std::string &correlation_id) {
  auto span = parent_span.tracer().StartSpan(
      "send_request_sync", {opentracing::ChildOf(&parent_span.context())});
  AddTracingContext(req, *span.get());

  if (!(flags & bftEngine::READ_ONLY_REQ) && timePusher_) {
    timePusher_->AddTimeToCommand(req);
  }

  std::string command;
  req.SerializeToString(&command);
  memset(m_outBuffer, 0, OUT_BUFFER_SIZE);

  uint32_t actualReplySize = 0;
  LOG4CPLUS_DEBUG(logger_, "Invoking command with flags: "
                               << flags << ", timeout: " << timeout.count());
  concordUtils::Status status = client_->invokeCommandSynch(
      command.c_str(), command.size(), flags, timeout, OUT_BUFFER_SIZE,
      m_outBuffer, &actualReplySize, correlation_id);

  if (status.isOK() && actualReplySize) {
    return resp.ParseFromArray(m_outBuffer, actualReplySize);
  } else {
    LOG4CPLUS_ERROR(logger_, "Error invoking command with flags: "
                                 << flags << " Status: " << status
                                 << " Reply size: " << actualReplySize);
    ErrorResponse *err = resp.add_error_response();
    err->set_description("Internal concord Error");
    return true;
  }
}

KVBClientPool::KVBClientPool(
    std::vector<KVBClient *> &clients, std::chrono::milliseconds timeout,
    std::shared_ptr<TimePusher> time_pusher,
    std::shared_ptr<concord::utils::IPrometheusRegistry> prometheus_registry)
    : logger_(
          log4cplus::Logger::getInstance("com.vmware.concord.KVBClientPool")),
      time_pusher_(time_pusher),
      client_count_{clients.size()},
      clients_(),
      timeout_(timeout),
      clients_mutex_(),
      clients_condition_(),
      shutdown_{false},
      kvbc_client_pool_requests_counters_(
          prometheus_registry->createCounterFamily(
              "concord_KVBClientPool_requests_counters_total",
              "counters of requests in KVBClientPool", {})),
      kvbc_client_pool_replies_counters_(
          prometheus_registry->createCounterFamily(
              "concord_KVBClientPool_replies_counters_total",
              "counters of replies in KVBClientPool", {})),
      kvbc_client_pool_received_requests_(prometheus_registry->createCounter(
          kvbc_client_pool_requests_counters_, {{"action", "received"}})),
      kvbc_client_pool_received_replies_(prometheus_registry->createCounter(
          kvbc_client_pool_replies_counters_, {{"action", "received"}})) {
  for (auto it = clients.begin(); it < clients.end(); it++) {
    clients_.push(*it);
  }
}

KVBClientPool::~KVBClientPool() {
  std::unique_lock<std::mutex> clients_lock(clients_mutex_);
  // stop new requests
  shutdown_ = true;

  while (client_count_ > 0) {
    // TODO: timeout
    clients_condition_.wait(clients_lock,
                            [this] { return !this->clients_.empty(); });

    LOG4CPLUS_DEBUG(logger_, "Stopping and deleting client");
    KVBClient *client = clients_.front();
    clients_.pop();
    delete client;
    client_count_--;
  }
  LOG4CPLUS_INFO(logger_, "Client cleanup complete");
}

bool KVBClientPool::send_request_sync(ConcordRequest &req, uint8_t flags,
                                      opentracing::Span &parent_span,
                                      ConcordResponse &resp,
                                      const std::string &correlation_id) {
  return send_request_sync(req, flags, timeout_, parent_span, resp,
                           correlation_id);
}

bool KVBClientPool::send_request_sync(ConcordRequest &req, uint8_t flags,
                                      std::chrono::milliseconds timeout,
                                      opentracing::Span &parent_span,
                                      ConcordResponse &resp,
                                      const std::string &correlation_id) {
  KVBClient *client;
  {
    std::unique_lock<std::mutex> clients_lock(clients_mutex_);

    auto predicate = [this] {
      // Only continue if either the node is shutting down or there is at lease
      // 1 available client in the pool
      return this->shutdown_ || !clients_.empty();
    };

    if (timeout > max_timeout_millis_) {
      LOG4CPLUS_WARN(logger_, "Timeout: " << timeout.count()
                                          << " > max_timeout_millis_: "
                                          << max_timeout_millis_.count());
      timeout = max_timeout_millis_;
    }
    if (timeout > 0ms) {
      if (!clients_condition_.wait_for(clients_lock, timeout, predicate)) {
        LOG4CPLUS_WARN(logger_, "Unable to claim a client in time.");
        ErrorResponse *err = resp.add_error_response();
        err->set_description("Internal concord Error");
        return true;
      }
    } else {
      // no timeout specified; wait unconditionally
      clients_condition_.wait(clients_lock, predicate);
    }

    if (shutdown_) {
      ErrorResponse *err = resp.add_error_response();
      err->set_description("Node is shutting down.");
      return true;
    }

    client = clients_.front();
    clients_.pop();
  }  // scope unlocks mutex

  kvbc_client_pool_received_requests_.Increment();
  bool result = client->send_request_sync(req, flags, timeout, parent_span,
                                          resp, correlation_id);
  kvbc_client_pool_received_replies_.Increment();

  {
    std::unique_lock<std::mutex> clients_lock(clients_mutex_);
    clients_.push(client);
  }  // scope unlocks mutex

  // one client returned to pool -> one thread needs to wake up
  clients_condition_.notify_one();

  return result;
}

void KVBClientPool::SetTimePusherPeriod(const Duration &period) {
  if (time_pusher_) {
    time_pusher_->SetPeriod(period);
  } else {
    LOG4CPLUS_WARN(logger_,
                   "Received request to reconfigure time pusher period to "
                   "client pool with no time pusher.");
  }
}

bool KVBClientPool::HasTimePusher() { return (bool)time_pusher_; }

}  // namespace consensus
}  // namespace concord
