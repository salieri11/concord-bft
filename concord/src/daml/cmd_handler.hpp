#ifndef CONCORD_DAML_CMD_HANDLER_HPP_
#define CONCORD_DAML_CMD_HANDLER_HPP_

#include <log4cplus/loggingmacros.h>

#include "blocking_queue.h"
#include "consensus/hash_defs.h"
#include "consensus/sliver.hpp"
#include "daml_commit.grpc.pb.h"
#include "daml_data.grpc.pb.h"
#include "daml_events.grpc.pb.h"
#include "storage/blockchain_db_types.h"
#include "storage/blockchain_interfaces.h"

namespace concord {
namespace daml {

concord::consensus::Sliver createSliver(char* content, const size_t size);
concord::consensus::Sliver createSliver(const std::string& content);

class KVBCCommandsHandler : public concord::storage::ICommandsHandler {
 private:
  log4cplus::Logger logger_;
  concord::storage::ILocalKeyValueStorageReadOnly* ro_storage_;
  concord::storage::IBlocksAppender* blocks_appender_;
  BlockingPersistentQueue<com::digitalasset::kvbc::CommittedTx>& committedTxs_;
  std::string execution_engine_addr_;

 public:
  KVBCCommandsHandler(
      concord::storage::ILocalKeyValueStorageReadOnly* ros,
      concord::storage::IBlocksAppender* ba,
      BlockingPersistentQueue<com::digitalasset::kvbc::CommittedTx>&
          committedTxs,
      std::string damle_addr)
      : logger_(log4cplus::Logger::getInstance("com.vmware.concord.daml")),
        ro_storage_(ros),
        blocks_appender_(ba),
        committedTxs_(committedTxs),
        execution_engine_addr_(damle_addr) {}

  int execute(uint16_t clientId, uint64_t sequenceNum, bool readOnly,
              uint32_t requestSize, const char* request, uint32_t maxReplySize,
              char* outReply, uint32_t& outActualReplySize) override;

 private:
  bool executeKVBCRead(
      const com::digitalasset::kvbc::ReadTransactionRequest& readCmd,
      const size_t maxReplySize, char* outReply, uint32_t& outReplySize);
  concord::storage::Key absContractIdToKey(
      std::string prefix, com::digitalasset::kvbc::AbsContractId coid);
  concord::storage::Key relContractIdToKey(
      std::string prefix, int64_t txId,
      com::digitalasset::kvbc::RelContractId coid);
  bool contractIsActive(const concord::storage::Key& coid);
  concord::storage::KeyValuePair contractActiveKV(
      const concord::storage::Key& coid);
  concord::storage::KeyValuePair contractArchivedKV(
      const concord::storage::Key& coid);
  bool executeKVBCCommit(
      const com::digitalasset::kvbc::CommitRequest& commitReq,
      const size_t maxReplySize, char* outReply, uint32_t& outReplySize);

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
