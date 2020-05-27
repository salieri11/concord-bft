// Copyright 2018-2019 VMware, all rights reserved
//
// Layer between api_connection and IClient

#ifndef CONCORD_CONSENSUS_KVB_CLIENT_HPP_
#define CONCORD_CONSENSUS_KVB_CLIENT_HPP_

#include <log4cplus/loggingmacros.h>
#include <opentracing/span.h>
#include <prometheus/counter.h>
#include <prometheus/gauge.h>
#include <prometheus/histogram.h>
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <queue>
#include <vector>

#include "ClientImp.h"
#include "KVBCInterfaces.h"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "time/time_pusher.hpp"
#include "utils/concord_prometheus_metrics.hpp"

namespace concord {

// This breaks the circular dependency between TimePusher and
// KVBClient/KVBClientPool
namespace time {
class TimePusher;
}  // namespace time

namespace consensus {
using concord::kvbc::IClient;
class KVBClient {
 private:
  std::unique_ptr<IClient> client_;
  std::shared_ptr<concord::time::TimePusher> timePusher_;
  log4cplus::Logger logger_;
  static constexpr size_t OUT_BUFFER_SIZE = 512000;
  char m_outBuffer[OUT_BUFFER_SIZE];

 public:
  KVBClient(IClient *client,
            std::shared_ptr<concord::time::TimePusher> timePusher)
      : client_(client),
        timePusher_(timePusher),
        logger_(
            log4cplus::Logger::getInstance("com.vmware.concord.kvbclient")) {}

  ~KVBClient() { client_->stop(); }

  bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                         uint8_t flags, std::chrono::milliseconds timeout,
                         opentracing::Span &parent_span,
                         com::vmware::concord::ConcordResponse &resp,
                         const std::string &correlation_id = "");
};

class KVBClientPool {
 private:
  log4cplus::Logger logger_;
  std::shared_ptr<concord::time::TimePusher> time_pusher_;

  // Total number of clients under control of this pool.
  size_t client_count_;

  // Clients that are available for use (i.e. not already in use).
  std::queue<KVBClient *> clients_;

  // How long to wait for a client, and also how long to wait for the claimed
  // client to respond.
  std::chrono::milliseconds timeout_;

  // Mutex to grab before modifying clients_.
  std::mutex clients_mutex_;

  // Condition to wait on if clients_ is empty;
  std::condition_variable clients_condition_;

  // Flag signaling that the pool is shutting down. Once this flag is set,
  // clients_ should only be taken out of the pool to be destroyed, not to be
  // used for sending client requests.
  bool shutdown_;

  // Metrics
  prometheus::Family<prometheus::Counter> &kvbc_client_pool_requests_counters_;
  prometheus::Family<prometheus::Counter> &kvbc_client_pool_replies_counters_;
  prometheus::Family<prometheus::Gauge>
      &kvbc_client_pool_queued_requests_gauge_;
  prometheus::Family<prometheus::Histogram>
      &kvbc_client_pool_external_requests_dur_hist_;
  prometheus::Counter &kvbc_client_pool_received_requests_;
  prometheus::Counter &kvbc_client_pool_received_replies_;
  prometheus::Gauge &kvbc_client_pool_queued_requests_;
  prometheus::Histogram &kvbc_client_external_requests_dur_;

  std::chrono::milliseconds max_timeout_millis_{60 * 10 * 1000};

 public:
  // Constructor for KVBClientPool. clients should be a vector of pointers
  // to the pointers this KVBClientPool is to manage, and time_pusher should
  // be a shared_ptr to the TimePusher shared by these clients; this shared
  // pointer should point to the same thing as the shared TimePusher pointer
  // each client under management here was constucted with; we do not
  // currently define what the behavior will be if this precondition is not
  // met. This thing that this KVBClientPool and the KVBClients in manages
  // point to should be a valid TimePusher if the time service is enabled
  // and should be a null pointer if the time service is disabled.
  KVBClientPool(
      std::vector<KVBClient *> &clients, std::chrono::milliseconds timeout,
      std::shared_ptr<concord::time::TimePusher> time_pusher,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry);
  ~KVBClientPool();

  bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                         uint8_t flags, opentracing::Span &parent_span,
                         com::vmware::concord::ConcordResponse &resp,
                         const std::string &correlation_id = "");

  bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                         uint8_t flags, std::chrono::milliseconds timeout,
                         opentracing::Span &parent_span,
                         com::vmware::concord::ConcordResponse &resp,
                         const std::string &correlation_id = "");

  // Reconfigure the time period for the TimePusher (if any) managed by this
  // KVBClientPool and shared by its KVBClients. Calls to this function will be
  // ignored if this KVBClientPool was created without a TimePusher.
  void SetTimePusherPeriod(const google::protobuf::Duration &period);

  // Check whether this KVBClientPool was constructed with a TimePusher. It is
  // expected that this should be effectively equivalent to checking whether the
  // time service is enabled and the Concord node this is running on is a time
  // source if the preconditions of the KVBClientPool constructor were met when
  // this instance was constructed.
  bool HasTimePusher();
};

}  // namespace consensus
}  // namespace concord

#endif  // CONCORD_CONSENSUS_KVB_CLIENT_HPP_
