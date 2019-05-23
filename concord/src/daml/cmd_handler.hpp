#ifndef CONCORD_DAML_CMD_HANDLER_HPP_
#define CONCORD_DAML_CMD_HANDLER_HPP_

#include <log4cplus/loggingmacros.h>

#include "blocking_queue.h"
#include "consensus/blockchain_db_types.h"
#include "consensus/blockchain_interfaces.h"
#include "consensus/hash_defs.h"
#include "consensus/sliver.hpp"
#include "daml_commit.grpc.pb.h"
#include "daml_data.grpc.pb.h"
#include "daml_events.grpc.pb.h"

using namespace com::digitalasset;

namespace concord {
namespace daml {

concord::consensus::Sliver createSliver(char* content, const size_t size);
concord::consensus::Sliver createSliver(const std::string& content);

class KVBCCommandsHandler : public concord::consensus::ICommandsHandler {
 private:
  log4cplus::Logger logger_;
  concord::consensus::ILocalKeyValueStorageReadOnly* ro_storage_;
  concord::consensus::IBlocksAppender* blocks_appender_;
  BlockingPersistentQueue<kvbc::CommittedTx>& committedTxs_;

 public:
  KVBCCommandsHandler(concord::consensus::ILocalKeyValueStorageReadOnly* ros,
                      concord::consensus::IBlocksAppender* ba,
                      BlockingPersistentQueue<kvbc::CommittedTx>& committedTxs)
      : logger_(log4cplus::Logger::getInstance("com.vmware.concord.daml")),
        ro_storage_(ros),
        blocks_appender_(ba),
        committedTxs_(committedTxs) {}

  int execute(uint16_t clientId, uint64_t sequenceNum, bool readOnly,
              uint32_t requestSize, const char* request, uint32_t maxReplySize,
              char* outReply, uint32_t& outActualReplySize) override;

 private:
  bool executeKVBCRead(const kvbc::ReadTransactionRequest& readCmd,
                       const size_t maxReplySize, char* outReply,
                       uint32_t& outReplySize);
  concord::consensus::Key absContractIdToKey(std::string prefix,
                                             kvbc::AbsContractId coid);
  concord::consensus::Key relContractIdToKey(std::string prefix, int64_t txId,
                                             kvbc::RelContractId coid);
  bool contractIsActive(const concord::consensus::Key& coid);
  concord::consensus::KeyValuePair contractActiveKV(
      const concord::consensus::Key& coid);
  concord::consensus::KeyValuePair contractArchivedKV(
      const concord::consensus::Key& coid);
  bool executeKVBCCommit(const kvbc::CommitRequest& commitReq,
                         const size_t maxReplySize, char* outReply,
                         uint32_t& outReplySize);

  bool executeCommand(uint32_t requestSize, const char* request,
                      const size_t maxReplySize, char* outReply,
                      uint32_t& outReplySize);
  bool executeReadOnlyCommand(uint32_t requestSize, const char* request,
                              const size_t maxReplySize, char* outReply,
                              uint32_t& outReplySize);
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_CMD_HANDLER_HPP_
