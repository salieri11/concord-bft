// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client definition.

#ifndef CONCORD_CONSENSUS_KVB_CLIENTIMP_H_
#define CONCORD_CONSENSUS_KVB_CLIENTIMP_H_

#include <boost/thread.hpp>
#include <map>
#include "BlockchainInterfaces.h"
#include "ICommunication.hpp"
#include "SimpleClient.hpp"

using namespace bftEngine;

namespace concord {
namespace consensus {

class ClientImp : public IClient {
 public:
  // IClient methods
  virtual Status start() override;
  virtual Status stop() override;

  virtual bool isRunning() override;

  virtual Status invokeCommandSynch(const char *request, uint32_t requestSize,
                                    bool isReadOnly, uint32_t replySize,
                                    char *outReply,
                                    uint32_t *outActualReplySize) override;

 protected:
  // ctor & dtor
  ClientImp(CommConfig &commConfig, const ClientConsensusConfig &conf);
  virtual ~ClientImp();

  int m_status;

  friend IClient *createClient(CommConfig &commConfig,
                               const ClientConsensusConfig &conf);
  friend void release(IClient *r);

 private:
  SimpleClient *m_bftClient = nullptr;
  SeqNumberGeneratorForClientRequests *m_SeqNumGenerator = nullptr;
};

}  // namespace consensus
}  // namespace concord

#endif  // CONCORD_CONSENSUS_KVB_CLIENTIMP_H_
