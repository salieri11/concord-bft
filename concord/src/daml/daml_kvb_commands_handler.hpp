// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
#define CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_

#include <grpcpp/grpcpp.h>

#include "blocking_queue.h"
#include "concord.pb.h"
#include "consensus/concord_commands_handler.hpp"
#include "consensus/hash_defs.h"
#include "consensus/sliver.hpp"
#include "daml_commit.grpc.pb.h"
#include "daml_data.grpc.pb.h"
#include "daml_events.grpc.pb.h"
#include "daml_validator.grpc.pb.h"
#include "daml_validator_client.hpp"

namespace concord {
namespace daml {

concord::consensus::Sliver CreateSliver(char* content, const size_t size);
concord::consensus::Sliver CreateSliver(const std::string& content);

class DamlKvbCommandsHandler
    : public concord::consensus::ConcordCommandsHandler {
 private:
  log4cplus::Logger logger_;
  BlockingPersistentQueue<com::digitalasset::kvbc::CommittedTx>& committed_txs_;
  std::unique_ptr<DamlValidatorClient> validator_client_;

 public:
  DamlKvbCommandsHandler(
      const concord::config::ConcordConfiguration& config,
      const concord::storage::ILocalKeyValueStorageReadOnly& ros,
      concord::storage::IBlocksAppender& ba,
      BlockingPersistentQueue<com::digitalasset::kvbc::CommittedTx>&
          committed_txs,
      std::unique_ptr<DamlValidatorClient> validator)
      : ConcordCommandsHandler(config, ros, ba),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord.daml")),
        committed_txs_(committed_txs),
        validator_client_(std::move(validator)) {}

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               uint64_t sequence_num, bool read_only,
               concord::time::TimeContract* time_contract,
               com::vmware::concord::ConcordResponse& response) override;
  void WriteEmptyBlock(uint64_t sequence_num,
                       concord::time::TimeContract* time_contract) override;

 private:
  bool ExecuteRead(const com::digitalasset::kvbc::ReadCommand& readCmd,
                   com::vmware::concord::ConcordResponse& concord_response);
  bool ExecuteCommit(const com::digitalasset::kvbc::CommitRequest& commitReq,
                     concord::time::TimeContract* time_contract,
                     com::vmware::concord::ConcordResponse& concord_response);

  bool ExecuteCommand(const com::vmware::concord::ConcordRequest& request,
                      concord::time::TimeContract* time_contract,
                      com::vmware::concord::ConcordResponse& response);
  bool ExecuteReadOnlyCommand(
      const com::vmware::concord::ConcordRequest& request,
      com::vmware::concord::ConcordResponse& response);
  std::map<string, string> GetFromStorage(
      const google::protobuf::RepeatedPtrField<com::digitalasset::kvbc::KVKey>&
          keys);
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
