// Concord
//
// Copyright (c) 2019 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License"). You may not use this product except in
// compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright notices and license terms. Your use of
// these subcomponents is subject to the terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#pragma once

#include "messages/PreProcessRequestMsg.hpp"
#include "messages/ClientPreProcessRequestMsg.hpp"
#include "MsgsCommunicator.hpp"
#include "MsgHandlersRegistrator.hpp"
#include "SimpleThreadPool.hpp"
#include "Replica.hpp"
#include "RequestProcessingState.hpp"
#include "sliver.hpp"
#include "SigManager.hpp"
#include "Metrics.hpp"
#include "Timers.hpp"

#include <mutex>

namespace preprocessor {

struct ClientRequestState {
  std::mutex mutex;  // Define a mutex per client to avoid contentions between clients
  RequestProcessingStateUniquePtr reqProcessingStatePtr;
};

typedef std::shared_ptr<ClientRequestState> ClientRequestStateSharedPtr;
typedef std::unordered_map<uint16_t, ClientRequestStateSharedPtr> OngoingReqMap;

//**************** Class PreProcessor ****************//

// This class is responsible for the coordination of pre-execution activities on both - primary and non-primary
// replica types. It handles client pre-execution requests, pre-processing requests, and replies.
// On primary replica - it collects pre-execution result hashes from other replicas and decides whether to continue
// or to fail request processing.

class PreProcessor {
 public:
  PreProcessor(std::shared_ptr<MsgsCommunicator> &msgsCommunicator,
               std::shared_ptr<IncomingMsgsStorage> &incomingMsgsStorage,
               std::shared_ptr<MsgHandlersRegistrator> &msgHandlersRegistrator,
               bftEngine::IRequestsHandler &requestsHandler,
               const InternalReplicaApi &replica);

  ~PreProcessor();

  static void addNewPreProcessor(std::shared_ptr<MsgsCommunicator> &msgsCommunicator,
                                 std::shared_ptr<IncomingMsgsStorage> &incomingMsgsStorage,
                                 std::shared_ptr<MsgHandlersRegistrator> &msgHandlersRegistrator,
                                 bftEngine::IRequestsHandler &requestsHandler,
                                 InternalReplicaApi &myReplica);

  static void setAggregator(std::shared_ptr<concordMetrics::Aggregator> aggregator);

  ReqId getOngoingReqIdForClient(uint16_t clientId);

 private:
  friend class AsyncPreProcessJob;

  uint16_t numOfRequiredReplies();

  template <typename T>
  void messageHandler(MessageBase *msg);

  template <typename T>
  void onMessage(T *msg);

  void registerRequest(ClientPreProcessReqMsgUniquePtr clientReqMsg,
                       PreProcessRequestMsgSharedPtr preProcessRequestMsg);
  void releaseClientPreProcessRequestSafe(uint16_t clientId);
  void releaseClientPreProcessRequest(const ClientRequestStateSharedPtr &clientEntry, uint16_t clientId);
  bool validateMessage(MessageBase *msg) const;
  void registerMsgHandlers();
  bool checkClientMsgCorrectness(const ClientPreProcessReqMsgUniquePtr &clientReqMsg, ReqId reqSeqNum) const;
  void handleClientPreProcessRequest(ClientPreProcessReqMsgUniquePtr clientReqMsg);
  void handleClientPreProcessRequestByPrimary(ClientPreProcessReqMsgUniquePtr clientReqMsg);
  void handleClientPreProcessRequestByNonPrimary(ClientPreProcessReqMsgUniquePtr msg);
  void sendMsg(char *msg, NodeIdType dest, uint16_t msgType, MsgSize msgSize);
  void sendPreProcessRequestToAllReplicas(PreProcessRequestMsgSharedPtr preProcessReqMsg);
  uint16_t getClientReplyBufferId(uint16_t clientId) const { return clientId - numOfReplicas_; }
  const char *getPreProcessResultBuffer(uint16_t clientId) const;
  void launchAsyncReqPreProcessingJob(PreProcessRequestMsgSharedPtr preProcessReqMsg, bool isPrimary, bool isRetry);
  uint32_t launchReqPreProcessing(uint16_t clientId, ReqId reqSeqNum, uint32_t reqLength, char *reqBuf);
  void handleReqPreProcessingJob(PreProcessRequestMsgSharedPtr preProcessReqMsg, bool isPrimary, bool isRetry);
  void handlePreProcessedReqByNonPrimary(uint16_t clientId,
                                         ReqId reqSeqNum,
                                         uint32_t resBufLen,
                                         const std::string &cid);
  void handlePreProcessedReqByPrimary(PreProcessRequestMsgSharedPtr preProcessReqMsg,
                                      uint16_t clientId,
                                      uint32_t resultBufLen);
  void handlePreProcessedReqPrimaryRetry(NodeIdType clientId, SeqNum reqSeqNum);
  void finalizePreProcessing(NodeIdType clientId);
  void cancelPreProcessing(NodeIdType clientId);
  PreProcessingResult getPreProcessingConsensusResult(uint16_t clientId);
  void handleReqPreProcessedByOneReplica(const std::string &cid,
                                         PreProcessingResult result,
                                         NodeIdType clientId,
                                         SeqNum reqSeqNum);
  void updateAggregatorAndDumpMetrics();
  void addTimers();
  void cancelTimers();
  void onRequestsStatusCheckTimer(concordUtil::Timers::Handle timer);
  void onPreProcessRequestMsgWaitTimer(concordUtil::Timers::Handle timer);

 private:
  static std::vector<std::shared_ptr<PreProcessor>> preProcessors_;  // The place holder for PreProcessor objects

  bftEngine::impl::SigManagerSharedPtr sigManager_;
  std::shared_ptr<MsgsCommunicator> msgsCommunicator_;
  std::shared_ptr<IncomingMsgsStorage> incomingMsgsStorage_;
  std::shared_ptr<MsgHandlersRegistrator> msgHandlersRegistrator_;
  bftEngine::IRequestsHandler &requestsHandler_;
  const InternalReplicaApi &myReplica_;
  const ReplicaId myReplicaId_;
  const uint32_t maxReplyMsgSize_;
  const std::set<ReplicaId> &idsOfPeerReplicas_;
  const uint16_t numOfReplicas_;
  const uint16_t numOfClients_;
  util::SimpleThreadPool threadPool_;
  // One-time allocated buffers (one per client) for the pre-execution results storage
  std::vector<concordUtils::Sliver> preProcessResultBuffers_;
  OngoingReqMap ongoingRequests_;  // clientId -> ClientRequestStateSharedPtr
  concordMetrics::Component metricsComponent_;
  std::chrono::seconds metricsLastDumpTime_;
  std::chrono::seconds metricsDumpIntervalInSec_;
  struct PreProcessingMetrics {
    concordMetrics::CounterHandle preProcReqReceived;
    concordMetrics::CounterHandle preProcReqInvalid;
    concordMetrics::CounterHandle preProcReqIgnored;
    concordMetrics::CounterHandle preProcConsensusNotReached;
    concordMetrics::CounterHandle preProcessRequestTimedout;
    concordMetrics::CounterHandle preProcReqSentForFurtherProcessing;
    concordMetrics::CounterHandle preProcPossiblePrimaryFaultDetected;
  } preProcessorMetrics_;
  concordUtil::Timers::Handle requestsStatusCheckTimer_;
  concordUtil::Timers::Handle preProcessReqMsgWaitTimer_;
  const uint64_t preExecReqStatusCheckTimeMilli_;
  const uint16_t preProcessReqWaitTimeMilli_;
};

//**************** Class AsyncPreProcessJob ****************//

// This class is used to send messages to other replicas in parallel

class AsyncPreProcessJob : public util::SimpleThreadPool::Job {
 public:
  AsyncPreProcessJob(PreProcessor &preProcessor,
                     PreProcessRequestMsgSharedPtr preProcessReqMsg,
                     bool isPrimary,
                     bool isRetry);
  virtual ~AsyncPreProcessJob() = default;

  void execute() override;
  void release() override;

 private:
  PreProcessor &preProcessor_;
  PreProcessRequestMsgSharedPtr preProcessReqMsg_;
  bool isPrimary_ = false;
  bool isRetry_ = false;
};

}  // namespace preprocessor