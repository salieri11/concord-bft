// Concord
//
// Copyright (c) 2018 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").  You may not use this product except in
// compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright notices and license terms. Your use of
// these subcomponents is subject to the terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.

#include <cmath>

#include "messages/FullCommitProofMsg.hpp"
#include "Logger.hpp"
#include "messages/SignedShareMsgs.hpp"
#include "ControllerWithSimpleHistory.hpp"
#include <chrono>

using namespace std::chrono;

namespace bftEngine {
namespace impl {

static const uint32_t defaultTimeToStartSlowPathMilli = 150;  //  800;
static const uint32_t minTimeToStartSlowPathMilli = 20;       // 100;
static const uint32_t maxTimeToStartSlowPathMilli = 2000;
static const float maxUpdateInTimeToStartSlowPath = 0.20F;

ControllerWithSimpleHistory::ControllerWithSimpleHistory(
    uint16_t C, uint16_t F, ReplicaId replicaId, ViewNum initialView, SeqNum initialSeq)
    : onlyOptimisticFast(C == 0),
      c(C),
      f(F),
      numOfReplicas(3 * F + 2 * C + 1),
      myId(replicaId),
      recentActivity(1, nullptr),
      currentFirstPath{ControllerWithSimpleHistory_debugInitialFirstPath},
      currentView{initialView},
      isPrimary{((currentView % numOfReplicas) == myId)},
      currentTimeToStartSlowPathMilli{defaultTimeToStartSlowPathMilli} {
  if (isPrimary) {
    onBecomePrimary(initialView, initialSeq);
  }
}

// Resets `recentActivity`, a data structure that stores `EvaluationPeriod` num of requests.
// The starting index is initialized to the next sequence number,
// that is a multiplication of EvaluationPeriod plus one.
void ControllerWithSimpleHistory::onBecomePrimary(ViewNum v, SeqNum s) {
  Assert(isPrimary);

  SeqNum nextFirstRelevantSeqNum;
  if (s % EvaluationPeriod == 0)
    nextFirstRelevantSeqNum = s + 1;
  else
    nextFirstRelevantSeqNum = ((s / EvaluationPeriod) * EvaluationPeriod) + EvaluationPeriod + 1;

  recentActivity.resetAll(nextFirstRelevantSeqNum);

  currentTimeToStartSlowPathMilli = defaultTimeToStartSlowPathMilli;

  LOG_DEBUG_F(GL, "currentTimeToStartSlowPathMilli = %d", (int)currentTimeToStartSlowPathMilli);
}

CommitPath ControllerWithSimpleHistory::getCurrentFirstPath() { return currentFirstPath; }

uint32_t ControllerWithSimpleHistory::timeToStartSlowPathMilli() { return currentTimeToStartSlowPathMilli; }

uint32_t ControllerWithSimpleHistory::slowPathsTimerMilli() {
  const uint32_t minUsefulTimerRes = 10;  // TODO(GG): ??

  uint32_t retVal = (currentTimeToStartSlowPathMilli / 2);
  if (retVal < minUsefulTimerRes) retVal = minUsefulTimerRes;

  return retVal;
}

void ControllerWithSimpleHistory::onNewView(ViewNum v, SeqNum s) {
  Assert(v >= currentView);
  currentView = v;
  isPrimary = ((currentView % numOfReplicas) == myId);

  if (isPrimary) onBecomePrimary(v, s);
}

// Return true if currentFirstPath changed
bool ControllerWithSimpleHistory::onNewSeqNumberExecution(SeqNum n) {
  if (!isPrimary || !recentActivity.insideActiveWindow(n)) {
    return false;
  }

  SeqNoInfo& s = recentActivity.get(n);

  Time now = getMonotonicTime();
  auto duration = now - s.prePrepareTime;

  const auto MAX_DURATION_MICRO = microseconds(2 * 1000 * 1000);
  const auto MIN_DURATION_MICRO = microseconds(100);

  if (duration > MAX_DURATION_MICRO)
    s.durationMicro_ = MAX_DURATION_MICRO;
  else if (duration < MIN_DURATION_MICRO)
    s.durationMicro_ = MIN_DURATION_MICRO;

  // reset every 1000 rounds . TODO(GG): in config ?
  if (n % 1000 == 0) avgAndStdOfExecTime.reset();

  avgAndStdOfExecTime.add((double)s.durationMicro_.count());

  if (n == recentActivity.currentActiveWindow().second) {
    return onEndOfEvaluationPeriod();
  }
  return false;
}

// Return true if the currentFirstPath changed
bool ControllerWithSimpleHistory::onEndOfEvaluationPeriod() {
  const SeqNum minSeq = recentActivity.currentActiveWindow().first;
  const SeqNum maxSeq = recentActivity.currentActiveWindow().second;

  const CommitPath lastFirstPathVal = currentFirstPath;

  bool downgraded = false;
  bool currentFirstPathChanged = false;

  // Upgrade/Downgrade current path -
  // If SLOW_PATH, test for upgrade
  //    Iterate over the last X(64) executions and count How many times -
  //    - All replicas have participated in a transaction i.e. cyclesWithFullCooperation.
  //    - Between 3F+C and 3F+2C+1 replicas have participated i.e. cyclesWithPartialCooperation.
  //    If cyclesWithFullCooperation is greater than factorForFastOptimisticPath * EvaluationPeriod then upgrade to:
  //    OPTIMISTIC_FAST.
  //    Else - if C > 0 and cyclesWithFullCooperation + cyclesWithPartialCooperation is greater than:
  //    factorForFastPath * EvaluationPeriod, upgrade to FAST_WITH_THRESHOLD.
  // If FAST(can be optimistic or with threshold), test for downgrade
  //    Count the number of successful fast paths in the evaluation period.
  //    If successfulFastPaths is less than factor * EvaluationPeriod then:
  //    - If the current path is OPTIMISTIC_PATH and C > 0, downgrade to FAST_WITH_THRESHOLD.
  //    - Else downgrade to SLOW.
  if (currentFirstPath == CommitPath::SLOW) {
    size_t cyclesWithFullCooperation = 0;
    size_t cyclesWithPartialCooperation = 0;

    for (SeqNum i = minSeq; i <= maxSeq; i++) {
      SeqNoInfo& s = recentActivity.get(i);
      if (s.replicas.size() == (numOfReplicas - 1))
        cyclesWithFullCooperation++;
      else if (s.replicas.size() >= (3 * f + c))
        cyclesWithPartialCooperation++;
    }

    if (ControllerWithSimpleHistory_debugUpgradeEnabled) {
      const float factorForFastOptimisticPath =
          ControllerWithSimpleHistory_debugUpgradeFactorForFastOptimisticPath;                    // 0.95F;
      const float factorForFastPath = ControllerWithSimpleHistory_debugUpgradeFactorForFastPath;  //  0.85F;

      if (cyclesWithFullCooperation >= (factorForFastOptimisticPath * EvaluationPeriod)) {
        currentFirstPath = CommitPath::OPTIMISTIC_FAST;
        currentFirstPathChanged = true;
      } else if (!onlyOptimisticFast &&
                 (cyclesWithFullCooperation + cyclesWithPartialCooperation >= (factorForFastPath * EvaluationPeriod))) {
        currentFirstPath = CommitPath::FAST_WITH_THRESHOLD;
        currentFirstPathChanged = true;
      }
    }
  } else {
    Assert(currentFirstPath == CommitPath::OPTIMISTIC_FAST || currentFirstPath == CommitPath::FAST_WITH_THRESHOLD);

    size_t successfulFastPaths = 0;
    for (SeqNum i = minSeq; i <= maxSeq; i++) {
      SeqNoInfo& s = recentActivity.get(i);
      if (!s.switchToSlowPath) successfulFastPaths++;
    }

    const float factor = ControllerWithSimpleHistory_debugDowngradeFactor;  // 0.85F; //  0.0F;

    if (ControllerWithSimpleHistory_debugDowngradeEnabled && (successfulFastPaths < (factor * EvaluationPeriod))) {
      downgraded = true;

      // downgrade
      if (!onlyOptimisticFast && (currentFirstPath == CommitPath::OPTIMISTIC_FAST)) {
        currentFirstPath = CommitPath::FAST_WITH_THRESHOLD;
      } else {
        currentFirstPath = CommitPath::SLOW;
      }

      currentFirstPathChanged = true;
    }
  }

  recentActivity.resetAll(maxSeq + 1);

  if (lastFirstPathVal != currentFirstPath) LOG_INFO_F(GL, "currentFirstPath = %d", (int)currentFirstPath);

  // Adaptive tuning of the slow path duration threshold -
  //  - initialize the threshold to the `mean + 2*(standard deviation)` of the last executions.
  //  - Check that it's within bounds of min/max ranges.
  const double execAvg = avgAndStdOfExecTime.avg();
  const double execVar = avgAndStdOfExecTime.var();
  const double execStd = ((execVar) > 0 ? sqrt(execVar) : 0);

  LOG_DEBUG_F(GL, "Execution time of recent rounds (micro) - avg=%f std=%f", execAvg, execStd);

  if (!downgraded) {
    // compute and update currentTimeToStartSlowPathMilli
    const uint32_t relMin = (uint32_t)((1 - maxUpdateInTimeToStartSlowPath) * currentTimeToStartSlowPathMilli);
    const uint32_t relMax = (uint32_t)((1 + maxUpdateInTimeToStartSlowPath) * currentTimeToStartSlowPathMilli);

    uint32_t newSlowPathTimeMilli = (uint32_t)((execAvg + 2 * execStd) / 1000);

    if (newSlowPathTimeMilli < relMin)
      newSlowPathTimeMilli = relMin;
    else if (newSlowPathTimeMilli > relMax)
      newSlowPathTimeMilli = relMax;

    if (newSlowPathTimeMilli < minTimeToStartSlowPathMilli)
      newSlowPathTimeMilli = minTimeToStartSlowPathMilli;
    else if (newSlowPathTimeMilli > maxTimeToStartSlowPathMilli)
      newSlowPathTimeMilli = maxTimeToStartSlowPathMilli;

    currentTimeToStartSlowPathMilli = newSlowPathTimeMilli;

    LOG_DEBUG_F(GL, "currentTimeToStartSlowPathMilli = %d", (int)currentTimeToStartSlowPathMilli);
  }
  return currentFirstPathChanged;
}

void ControllerWithSimpleHistory::onSendingPrePrepare(SeqNum n, CommitPath commitPath) {
  if (!isPrimary || !recentActivity.insideActiveWindow(n)) return;

  SeqNoInfo& s = recentActivity.get(n);

  // if this is the time we send the preprepare
  if (s.prePrepareTime == MinTime) {
    s.prePrepareTime = getMonotonicTime();
  }
}

void ControllerWithSimpleHistory::onStartingSlowCommit(SeqNum n) {
  if (!isPrimary || !recentActivity.insideActiveWindow(n)) return;
  if (currentFirstPath == CommitPath::SLOW) return;

  SeqNoInfo& s = recentActivity.get(n);
  s.switchToSlowPath = true;
}

void ControllerWithSimpleHistory::onMessage(const PreparePartialMsg* m) {
  if (!isPrimary) return;
  if (currentFirstPath != CommitPath::SLOW) return;

  const SeqNum n = m->seqNumber();
  const ReplicaId id = m->senderId();

  if (!recentActivity.insideActiveWindow(n)) return;
  SeqNoInfo& s = recentActivity.get(n);
  if (s.switchToSlowPath) return;

  if (s.replicas.count(id) > 0) return;
  s.replicas.insert(id);
}

}  // namespace impl
}  // namespace bftEngine