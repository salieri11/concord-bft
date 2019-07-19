#ifndef CONCORD_DAML_CMD_HANDLER_HPP_
#define CONCORD_DAML_CMD_HANDLER_HPP_

#include <google/protobuf/timestamp.pb.h>
#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>

#include "blocking_queue.h"
#include "consensus/hash_defs.h"
#include "consensus/sliver.hpp"
#include "daml_commit.grpc.pb.h"
#include "daml_data.grpc.pb.h"
#include "daml_events.grpc.pb.h"
#include "daml_validator.grpc.pb.h"
#include "storage/blockchain_db_types.h"
#include "storage/blockchain_interfaces.h"

namespace concord {
namespace daml {

concord::consensus::Sliver CreateSliver(char* content, const size_t size);
concord::consensus::Sliver CreateSliver(const std::string& content);

class KVBCValidatorClient {
 public:
  KVBCValidatorClient(std::shared_ptr<grpc::ChannelInterface> channel)
      : stub_(com::digitalasset::kvbc::ValidationService::NewStub(channel)) {}

  grpc::Status Validate(
      std::string entryId, std::string submission,
      google::protobuf::Timestamp& recordTime,
      const std::map<std::string, std::string>& input_log_entries,
      const std::map<std::string, std::string>& input_state_entries,
      com::digitalasset::kvbc::ValidateResponse* out);

 private:
  std::unique_ptr<com::digitalasset::kvbc::ValidationService::Stub> stub_;
};

class KVBCCommandsHandler : public concord::storage::ICommandsHandler {
 private:
  log4cplus::Logger logger_;
  concord::storage::ILocalKeyValueStorageReadOnly* ro_storage_;
  concord::storage::IBlocksAppender* blocks_appender_;
  BlockingPersistentQueue<com::digitalasset::kvbc::CommittedTx>& committed_txs_;
  std::unique_ptr<KVBCValidatorClient> validator_client_;

 public:
  KVBCCommandsHandler(
      concord::storage::ILocalKeyValueStorageReadOnly* ros,
      concord::storage::IBlocksAppender* ba,
      BlockingPersistentQueue<com::digitalasset::kvbc::CommittedTx>&
          committed_txs,
      std::unique_ptr<KVBCValidatorClient> validator)
      : logger_(log4cplus::Logger::getInstance("com.vmware.concord.daml")),
        ro_storage_(ros),
        blocks_appender_(ba),
        committed_txs_(committed_txs),
        validator_client_(std::move(validator)) {}

  int execute(uint16_t clientId, uint64_t sequenceNum, bool readOnly,
              uint32_t requestSize, const char* request, uint32_t maxReplySize,
              char* outReply, uint32_t& outActualReplySize) override;

 private:
  bool ExecuteKVBCRead(const com::digitalasset::kvbc::ReadCommand& readCmd,
                       const size_t maxReplySize, char* outReply,
                       uint32_t& outReplySize);
  bool ExecuteKVBCCommit(
      const com::digitalasset::kvbc::CommitRequest& commitReq,
      const size_t maxReplySize, char* outReply, uint32_t& outReplySize);

  bool ExecuteCommand(uint32_t requestSize, const char* request,
                      const size_t maxReplySize, char* outReply,
                      uint32_t& outReplySize);
  bool ExecuteReadOnlyCommand(uint32_t requestSize, const char* request,
                              const size_t maxReplySize, char* outReply,
                              uint32_t& outReplySize);
  std::map<string, string> GetFromStorage(
      const google::protobuf::RepeatedPtrField<com::digitalasset::kvbc::KVKey>&
          keys);
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_CMD_HANDLER_HPP_
