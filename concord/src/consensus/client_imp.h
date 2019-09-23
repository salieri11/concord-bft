// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client definition.

#ifndef CONCORD_CONSENSUS_CLIENT_IMP_H_
#define CONCORD_CONSENSUS_CLIENT_IMP_H_

#include <boost/thread.hpp>
#include <chrono>
#include <map>
#include "ICommunication.hpp"
#include "SimpleClient.hpp"
#include "blockchain/db_interfaces.h"
#include "client_interface.h"
#include "communication.h"

namespace concord {
namespace consensus {

IClient *createClient(CommConfig &commConfig,
                      const ClientConsensusConfig &conf);

void releaseClient(IClient *r);

class ClientImp : public IClient {
 public:
  // IClient methods
  virtual concordUtils::Status start() override;
  virtual concordUtils::Status stop() override;

  virtual bool isRunning() override;

  virtual concordUtils::Status invokeCommandSynch(
      const char *request, uint32_t requestSize, bool isReadOnly,
      std::chrono::milliseconds timeout, uint32_t replySize, char *outReply,
      uint32_t *outActualReplySize) override;

 protected:
  // ctor & dtor
  ClientImp(CommConfig &commConfig, const ClientConsensusConfig &conf);
  virtual ~ClientImp();

  int m_status;

  friend IClient *createClient(CommConfig &commConfig,
                               const ClientConsensusConfig &conf);
  friend void releaseClient(IClient *r);

 private:
  bftEngine::SimpleClient *m_bftClient = nullptr;
  bftEngine::SeqNumberGeneratorForClientRequests *m_SeqNumGenerator = nullptr;
};

}  // namespace consensus
}  // namespace concord

#endif  // CONCORD_CONSENSUS_CLIENT_IMP_H_
