// Copyright 2019 VMware, all rights reserved

#include "time_pusher.hpp"

#include <log4cplus/loggingmacros.h>
#include <chrono>
#include <mutex>
#include <thread>

#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/kvb_client.hpp"
#include "time/time_reading.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::TimeRequest;
using com::vmware::concord::TimeSample;
using concord::time::TimePusher;

TimePusher::TimePusher(const concord::config::ConcordConfiguration &config,
                       const concord::config::ConcordConfiguration &nodeConfig,
                       concord::consensus::KVBClientPool &clientPool)
    : logger_(log4cplus::Logger::getInstance("concord.time.pusher")),
      clientPool_(clientPool),
      stop_(false),
      lastPublishTimeMs_(0) {
  // memoizing enable flag, to make checking faster later
  timeServiceEnabled_ = concord::time::IsTimeServiceEnabled(config);

  if (nodeConfig.hasValue<int>("time_pusher_period_ms")) {
    periodMilliseconds_ = nodeConfig.getValue<int>("time_pusher_period_ms");
  } else {
    periodMilliseconds_ = 0;
  }

  if (nodeConfig.hasValue<std::string>("time_source_id")) {
    timeSourceId_ = nodeConfig.getValue<std::string>("time_source_id");
  } else {
    timeSourceId_ = "";
  }

  if (timeServiceEnabled_) {
    signer_ = new TimeSigner(nodeConfig);
  }
}

TimePusher::~TimePusher() {
  if (signer_) {
    delete signer_;
  }
}

void TimePusher::Start() {
  if (!timeServiceEnabled_) {
    LOG4CPLUS_INFO(logger_, "Not starting thread: time service not enabled.");
    return;
  }

  if (timeSourceId_.empty()) {
    LOG4CPLUS_INFO(logger_,
                   "Not starting thread: no time_source_id configured.");
    return;
  }

  if (periodMilliseconds_ <= 0) {
    LOG4CPLUS_INFO(logger_, "Not starting thread: period is "
                                << periodMilliseconds_ << " ms (less than 1).");
    return;
  }

  std::lock_guard<std::mutex> lock(threadMutex_);
  if (pusherThread_.joinable()) {
    LOG4CPLUS_INFO(logger_, "Ignoring duplicate start request.");
    return;
  }

  pusherThread_ = std::thread(&TimePusher::ThreadFunction, this);
}

void TimePusher::Stop() {
  std::lock_guard<std::mutex> lock(threadMutex_);
  if (!pusherThread_.joinable()) {
    LOG4CPLUS_INFO(logger_, "Ignoring stop request - nothing to stop");
    return;
  }

  stop_ = true;
  pusherThread_.join();

  // allows the thread to be restarted, if we like
  stop_ = false;
}

bool TimePusher::IsTimeServiceEnabled() const { return timeServiceEnabled_; }

void TimePusher::AddTimeToCommand(ConcordRequest &command) {
  if (timeServiceEnabled_) {
    AddTimeToCommand(command, ReadTime());
  }
}

void TimePusher::AddTimeToCommand(ConcordRequest &command, uint64_t time) {
  assert(signer_);
  std::vector<uint8_t> signature = signer_->Sign(time);

  TimeRequest *tr = command.mutable_time_request();
  TimeSample *ts = tr->mutable_sample();
  ts->set_source(timeSourceId_);
  ts->set_time(time);
  ts->set_signature(signature.data(), signature.size());
  lastPublishTimeMs_ = time;
}

void TimePusher::ThreadFunction() {
  LOG4CPLUS_INFO(
      logger_, "Thread started with period " << periodMilliseconds_ << " ms.");
  ConcordRequest req;
  ConcordResponse resp;

  while (!stop_) {
    // Sleeping for a static amount of time, instead of taking into account how
    // recently the last publish time was, means we might wait up to
    // 2*periodMilliseconds_ before publishing, but it also prevents silly 1ms
    // sleeps.
    std::this_thread::sleep_for(std::chrono::milliseconds(periodMilliseconds_));

    uint64_t time = ReadTime();
    if (time < lastPublishTimeMs_ + periodMilliseconds_) {
      // Time was published by a transaction recently - no need to publish again
      // right now.
      continue;
    }

    try {
      AddTimeToCommand(req, time);
      clientPool_.send_request_sync(req, false /* not read-only */, resp);
      req.Clear();
      resp.Clear();
    } catch (...) {
      // We don't want this thread to die for any reason other than being shut
      // down, because we don't have anything monitoring to restart it if it
      // does. So we'll swallow all exceptions and just yell into the log about
      // any problems and wait for an admin to notice.
      LOG4CPLUS_ERROR(logger_, "Unable to send time update");
    }
  }
}
