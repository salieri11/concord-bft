// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License"). You may not use this product except in
// compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright notices and license terms.
// Your use of these subcomponents is subject to the terms and conditions of the sub-component's license,
// as noted in the LICENSE file.

#include "RequestsBatchingLogic.hpp"

namespace bftEngine::batchingLogic {

using namespace concordUtil;
using namespace std;
using namespace std::chrono;

RequestsBatchingLogic::RequestsBatchingLogic(InternalReplicaApi &replica,
                                             const ReplicaConfig &config,
                                             concordMetrics::Component &metrics,
                                             concordUtil::Timers &timers)
    : replica_(replica),
      metric_not_enough_client_requests_event_{metrics.RegisterCounter("notEnoughClientRequestsEvent")},
      batchingPolicy_((BatchingPolicy)config.batchingPolicy),
      batchingFactorCoefficient_(config.batchingFactorCoefficient),
      maxInitialBatchSize_(config.maxInitialBatchSize),
      batchFlushPeriodMs_(config.batchFlushPeriod),
      maxNumOfRequestsInBatch_(config.maxNumOfRequestsInBatch),
      maxBatchSizeInBytes_(config.maxBatchSizeInBytes),
      timers_(timers) {
  intervalTime_ = batchFlushPeriodMs_ / sleepFactorPercentage;
  auto flushTime = batchingPolicy_ == BATCH_BY_FILL_RATE ? intervalTime_ : batchFlushPeriodMs_;
  if (batchingPolicy_ == BATCH_BY_REQ_SIZE || batchingPolicy_ == BATCH_BY_REQ_NUM ||
      batchingPolicy_ == BATCH_BY_FILL_RATE)
    batchFlushTimer_ = timers_.add(
        milliseconds(flushTime), Timers::Timer::RECURRING, [this](Timers::Handle h) { onBatchFlushTimer(h); });
}

RequestsBatchingLogic::~RequestsBatchingLogic() {
  if (batchingPolicy_ == BATCH_BY_REQ_SIZE || batchingPolicy_ == BATCH_BY_REQ_NUM ||
      batchingPolicy_ == BATCH_BY_FILL_RATE)
    timers_.cancel(batchFlushTimer_);
}

void RequestsBatchingLogic::onBatchFlushTimer(Timers::Handle) {
  lock_guard<mutex> lock(batchProcessingLock_);
  if (replica_.isCurrentPrimary() && batchingPolicy_ == BATCH_BY_FILL_RATE) {
    bool send = true;
    if (std::chrono::steady_clock::now() - intervalStart_ < std::chrono::milliseconds(batchFlushPeriodMs_)) {
      if (currentIntervalCount_ >= lastIntervalCount_ ||
          lastIntervalCount_ - currentIntervalCount_ <=
              (double)lastIntervalCount_ / 100 * lastIntervalThreholdPercentage) {
        send = false;
      }
    } else {
      intervalStart_ = std::chrono::steady_clock::now();
    }
    lastIntervalCount_ = currentIntervalCount_;
    currentIntervalCount_ = 0;

    if (send) replica_.tryToSendPrePrepareMsg(false);
    timers_.reset(batchFlushTimer_, milliseconds(intervalTime_));
    return;
  } else if (replica_.isCurrentPrimary()) {
    LOG_DEBUG(GL, "Batching flush period expired" << KVLOG(batchFlushPeriodMs_));
    if (replica_.tryToSendPrePrepareMsg(false)) timers_.reset(batchFlushTimer_, milliseconds(batchFlushPeriodMs_));
  }
}

PrePrepareMsg *RequestsBatchingLogic::batchRequestsSelfAdjustedPolicy(SeqNum primaryLastUsedSeqNum,
                                                                      uint64_t requestsInQueue,
                                                                      SeqNum lastExecutedSeqNum) {
  if (requestsInQueue > maxNumberOfPendingRequestsInRecentHistory_)
    maxNumberOfPendingRequestsInRecentHistory_ = requestsInQueue;

  uint64_t minBatchSize = 1;
  uint64_t concurrentDiff = primaryLastUsedSeqNum + 1 - lastExecutedSeqNum;

  if (concurrentDiff >= 2) {
    minBatchSize = concurrentDiff * batchingFactor_;
    if (minBatchSize > maxInitialBatchSize_) minBatchSize = maxInitialBatchSize_;
  }

  if (requestsInQueue < minBatchSize) {
    LOG_INFO(GL, "Not enough client requests in the queue to fill the batch" << KVLOG(minBatchSize, requestsInQueue));
    metric_not_enough_client_requests_event_.Get().Inc();
    return nullptr;
  }

  // Update batching factor
  if (((primaryLastUsedSeqNum + 1) % kWorkWindowSize) == 0) {
    batchingFactor_ = maxNumberOfPendingRequestsInRecentHistory_ / batchingFactorCoefficient_;
    if (batchingFactor_ < 1) batchingFactor_ = 1;
    maxNumberOfPendingRequestsInRecentHistory_ = 0;
    LOG_DEBUG(GL, "PrePrepare batching factor updated" << KVLOG(batchingFactor_));
  }

  return replica_.buildPrePrepareMessage();
}

PrePrepareMsg *RequestsBatchingLogic::batchRequests() {
  const auto requestsInQueue = replica_.getRequestsInQueue();
  if (requestsInQueue == 0) return nullptr;

  PrePrepareMsg *prePrepareMsg = nullptr;
  switch (batchingPolicy_) {
    case BATCH_SELF_ADJUSTED:
      prePrepareMsg = batchRequestsSelfAdjustedPolicy(
          replica_.getPrimaryLastUsedSeqNum(), requestsInQueue, replica_.getLastExecutedSeqNum());
      break;
    case BATCH_BY_REQ_NUM: {
      lock_guard<mutex> lock(batchProcessingLock_);
      if (replica_.tryToSendPrePrepareMsgBatchByRequestsNum(maxNumOfRequestsInBatch_))
        timers_.reset(batchFlushTimer_, milliseconds(batchFlushPeriodMs_));
    } break;
    case BATCH_BY_REQ_SIZE: {
      lock_guard<mutex> lock(batchProcessingLock_);
      if (replica_.tryToSendPrePrepareMsgBatchByOverallSize(maxBatchSizeInBytes_))
        timers_.reset(batchFlushTimer_, milliseconds(batchFlushPeriodMs_));
    } break;
    case BATCH_BY_FILL_RATE: {
      lock_guard<mutex> lock(batchProcessingLock_);
      ++currentIntervalCount_;
      break;
    }
  }
  return prePrepareMsg;
}

}  // namespace bftEngine::batchingLogic
