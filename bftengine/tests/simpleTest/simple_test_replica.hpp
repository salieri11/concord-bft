//
// Created by Igor Golikov on 2019-07-01.
//

#ifndef CONCORD_BFT_SIMPLE_TEST_REPLICA_HPP
#define CONCORD_BFT_SIMPLE_TEST_REPLICA_HPP

#include "commonDefs.h"
#include "CommFactory.hpp"
#include "Replica.hpp"
#include "ReplicaConfig.hpp"
#include "SimpleStateTransfer.hpp"
#include <thread>

using namespace bftEngine;
using namespace std;

#define test_assert(statement, message) \
{ if (!(statement)) { \
LOG_FATAL(logger, "assert fail with message: " << message); assert(false);}}

concordlogger::Logger replicaLogger =
    concordlogger::Logger::getLogger("simpletest.replica");

// The replica state machine.
class SimpleAppState : public RequestsHandler {
 private:
  uint64_t client_to_index(NodeNum clientId) {
    return clientId - numOfReplicas;
  }

  uint64_t get_last_state_value(NodeNum clientId) {
    auto index = client_to_index(clientId);
    return statePtr[index].lastValue;
  }

  uint64_t get_last_state_num(NodeNum clientId) {
    auto index = client_to_index(clientId);
    return statePtr[index].stateNum;
  }

  void set_last_state_value(NodeNum clientId, uint64_t value) {
    auto index = client_to_index(clientId);
    statePtr[index].lastValue = value;
  }

  void set_last_state_num(NodeNum clientId, uint64_t value) {
    auto index = client_to_index(clientId);
    statePtr[index].stateNum = value;
  }

 public:

  SimpleAppState(uint16_t numCl, uint16_t numRep) :
      statePtr{new SimpleAppState::State[numCl]},
      numOfClients{numCl},
      numOfReplicas{numRep} {}

  // Handler for the upcall from Concord-BFT.
  int execute(uint16_t clientId,
              uint64_t sequenceNum,
              bool readOnly,
              uint32_t requestSize,
              const char* request,
              uint32_t maxReplySize,
              char* outReply,
              uint32_t& outActualReplySize) override {
    if (readOnly) {
      // Our read-only request includes only a type, no argument.
      test_assert(requestSize == sizeof(uint64_t),
                  "requestSize =! " << sizeof(uint64_t));

      // We only support the READ operation in read-only mode.
      test_assert(*reinterpret_cast<const uint64_t*>(request) == READ_VAL_REQ,
                  "request is NOT " << READ_VAL_REQ);

      // Copy the latest register value to the reply buffer.
      test_assert(maxReplySize >= sizeof(uint64_t),
                  "maxReplySize < " << sizeof(uint64_t));
      uint64_t* pRet = reinterpret_cast<uint64_t*>(outReply);
      auto lastValue = get_last_state_value(clientId);
      *pRet = lastValue;
      outActualReplySize = sizeof(uint64_t);
    } else {
      // Our read-write request includes one eight-byte argument, in addition to
      // the request type.
      test_assert(requestSize == 2 * sizeof(uint64_t),
                  "requestSize != " << 2 * sizeof(uint64_t));

      // We only support the WRITE operation in read-write mode.
      const uint64_t* pReqId = reinterpret_cast<const uint64_t*>(request);
      test_assert(*pReqId == SET_VAL_REQ, "*preqId != " << SET_VAL_REQ);

      // The value to write is the second eight bytes of the request.
      const uint64_t* pReqVal = (pReqId + 1);

      // Modify the register state.
      set_last_state_value(clientId, *pReqVal);
      // Count the number of times we've modified it.
      auto stateNum = get_last_state_num(clientId);
      set_last_state_num(clientId, stateNum + 1);

      // Reply with the number of times we've modified the register.
      test_assert(maxReplySize >= sizeof(uint64_t),
                  "maxReplySize < " << sizeof(uint64_t));
      uint64_t* pRet = reinterpret_cast<uint64_t*>(outReply);
      *pRet = stateNum;
      outActualReplySize = sizeof(uint64_t);

      st->markUpdate(statePtr, sizeof(State) * numOfClients);
    }

    return 0;
  }

  struct State {
    // Number of modifications made.
    uint64_t stateNum = 0;
    // Register value.
    uint64_t lastValue = 0;
  };
  State *statePtr;

  uint16_t numOfClients;
  uint16_t numOfReplicas;

  concordlogger::Logger logger = concordlogger::Logger::getLogger
      ("simpletest.replica");

  bftEngine::SimpleInMemoryStateTransfer::ISimpleInMemoryStateTransfer* st = nullptr;
};

struct PersistencyTestInfo {
  bool replica2RestartNoVC = false;
  bool allReplicasRestartNoVC = false;
  bool replica2RestartVC = false;
  bool allReplicasRestartVC = false;
  bool primaryReplicaRestart = false;
};

class SimpleTestReplica {
 private:
  ICommunication *comm;
  bftEngine::Replica* replica = nullptr;
  ReplicaConfig *replicaConfig;
  std::thread *runnerThread = nullptr;

 public:
  PersistencyTestInfo pti;

  SimpleTestReplica(
      ICommunication *commObject,
      RequestsHandler &state,
      ReplicaConfig *rc,
      const PersistencyTestInfo &persistencyTestInfo,
      bftEngine::SimpleInMemoryStateTransfer::ISimpleInMemoryStateTransfer *inMemoryST) :
      comm{commObject}, pti{persistencyTestInfo}, replicaConfig{rc} {
    replica = Replica::createNewReplica(rc,  &state, inMemoryST, comm, nullptr);
  }

  ~SimpleTestReplica() {
    if(replica) {
      replica->stop();
      delete replica;
    }
    if(runnerThread) {
      runnerThread->join();
    }
  }

  uint16_t get_replica_id(){
    return replicaConfig->replicaId;
  }

  void start() {
    replica->start();
  }

  void stop() {
    replica->stop();
    LOG_INFO(replicaLogger, "replica " << replicaConfig->replicaId << " stopped");
  }

  bool isRunning() {
    return replica->isRunning();
  }

  void run() {
    while (replica->isRunning()) {
      if(pti.replica2RestartNoVC) {
        std::this_thread::sleep_for(std::chrono::seconds(10));
        if (replicaConfig->replicaId == 2) {
          replica->restartForDebug();
        }
      }
      else if (pti.replica2RestartVC) {
        std::this_thread::sleep_for(std::chrono::seconds(60));
        if (replicaConfig->replicaId == 2)
          replica->restartForDebug();
      }
      else if (pti.allReplicasRestartNoVC) {
        std::this_thread::sleep_for(std::chrono::seconds(3 *
            (2 + replicaConfig->replicaId) * (2 + replicaConfig->replicaId)));
        replica->restartForDebug();
      }
      else if (pti.allReplicasRestartVC) {
        std::this_thread::sleep_for(std::chrono::seconds(18 *
            (2 + replicaConfig->replicaId) * (2 + replicaConfig->replicaId)));
        replica->restartForDebug();
      }
      else if(pti.primaryReplicaRestart) {
        throw std::logic_error("not implemented");
      }
      else {
        std::this_thread::sleep_for(std::chrono::seconds(10));
      }
    }
  }

  void run_non_blocking() {
    runnerThread = new std::thread(std::bind(&SimpleTestReplica::run, this));
  }

};

#endif //CONCORD_BFT_SIMPLE_TEST_REPLICA_HPP
