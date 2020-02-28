// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
#define CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_

#include <grpcpp/grpcpp.h>
#include <opentracing/span.h>

#include "concord.pb.h"
#include "consensus/concord_commands_handler.hpp"
#include "daml_commit.grpc.pb.h"
#include "daml_validator.grpc.pb.h"
#include "daml_validator_client.hpp"
#include "hash_defs.h"
#include "sliver.hpp"
#include "thin_replica/subscription_buffer.hpp"

namespace concord {
namespace daml {

concordUtils::Sliver CreateSliver(char* content, const size_t size);
concordUtils::Sliver CreateSliver(const std::string& content);
concordUtils::Sliver CreateDamlKvbKey(const std::string& content);

class DamlKvbCommandsHandler
    : public concord::consensus::ConcordCommandsHandler {
 private:
  log4cplus::Logger logger_;
  std::unique_ptr<IDamlValidatorClient> validator_client_;
  prometheus::Counter& write_ops_;
  prometheus::Counter& read_ops_;
  concordUtils::Key cid_key_ =
      concordUtils::Key(new char[1]{storage::kKvbKeyCorrelationId}, 1);

 public:
  DamlKvbCommandsHandler(
      const concord::config::ConcordConfiguration& config,
      const concord::config::ConcordConfiguration& node_config,
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly& ros,
      concord::storage::blockchain::IBlocksAppender& ba,
      concord::thin_replica::SubBufferList& subscriber_list,
      std::unique_ptr<IDamlValidatorClient> validator,
      std::shared_ptr<concord::utils::IPrometheusRegistry> prometheus_registry)
      : ConcordCommandsHandler(config, node_config, ros, ba, subscriber_list,
                               prometheus_registry),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord.daml")),
        validator_client_(std::move(validator)),
        write_ops_{prometheus_registry->createCounter(
            command_handler_counters_, {{"layer", "DamlKvbCommandsHandler"},
                                        {"operation", "daml_writes"}})},
        read_ops_{prometheus_registry->createCounter(
            command_handler_counters_, {{"layer", "DamlKvbCommandsHandler"},
                                        {"operation", "daml_reads"}})} {}

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               uint8_t flags, concord::time::TimeContract* time_contract,
               opentracing::Span& parent_span,
               com::vmware::concord::ConcordResponse& response) override;
  void WriteEmptyBlock(concord::time::TimeContract* time_contract) override;

 private:
  bool ExecuteRead(const com::digitalasset::kvbc::ReadCommand& readCmd,
                   com::vmware::concord::ConcordResponse& concord_response);

  bool ExecuteCommit(const com::digitalasset::kvbc::CommitRequest& commitReq,
                     uint8_t flags, concord::time::TimeContract* time_contract,
                     opentracing::Span& parent_span,
                     com::vmware::concord::ConcordResponse& concord_response);

  bool ExecuteCommand(const com::vmware::concord::ConcordRequest& request,
                      uint8_t flags, concord::time::TimeContract* time_contract,
                      opentracing::Span& parent_span,
                      com::vmware::concord::ConcordResponse& response);
  bool ExecuteReadOnlyCommand(
      const com::vmware::concord::ConcordRequest& request,
      com::vmware::concord::ConcordResponse& response);
  std::map<string, string> GetFromStorage(
      const google::protobuf::RepeatedPtrField<std::string>& keys);

  std::optional<std::string> GetCorrelationId(
      const com::vmware::concord::DamlRequest& daml_request) const;

  void RecordTransaction(
      const string& entryId, const concordUtils::SetOfKeyValuePairs& updates,
      concordUtils::BlockId current_block_id, const string& correlation_id,
      com::vmware::concord::ConcordResponse& concord_response);

  bool CommitPreExecutionResult(
      concordUtils::BlockId current_block_id, const string& entryId,
      string& correlation_id,
      com::vmware::concord::ConcordResponse& concord_response);

  void BuildPreExecutionResult(
      const concordUtils::SetOfKeyValuePairs& updates,
      concordUtils::BlockId current_block_id, string& correlation_id,
      const com::digitalasset::kvbc::Result& result,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool RunDamlExecution(
      const com::digitalasset::kvbc::CommitRequest& commit_req,
      const string& entryId, google::protobuf::Timestamp& record_time,
      opentracing::Span& parent_span,
      com::digitalasset::kvbc::ValidateResponse& response,
      com::digitalasset::kvbc::ValidatePendingSubmissionResponse& response2);

  void GetUpdatesFromExecutionResult(
      const com::digitalasset::kvbc::Result& result, const string& entryId,
      concordUtils::SetOfKeyValuePairs& updates) const;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
