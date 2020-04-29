// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
#define CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_

#include <grpcpp/grpcpp.h>
#include <opentracing/span.h>
#include <time/time_contract.hpp>

#include "concord.pb.h"
#include "consensus/concord_commands_handler.hpp"
#include "daml_commit.grpc.pb.h"
#include "daml_validator.grpc.pb.h"
#include "daml_validator_client.hpp"
#include "db_interfaces.h"
#include "sliver.hpp"
#include "storage/kvb_key_types.h"
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
  bool enable_pipelined_commits_;
  prometheus::Counter& write_ops_;
  prometheus::Counter& read_ops_;

 public:
  static const char* kFeaturePipelinedCommitExecution;

  DamlKvbCommandsHandler(
      const concord::config::ConcordConfiguration& config,
      const concord::config::ConcordConfiguration& node_config,
      const concord::kvbc::ILocalKeyValueStorageReadOnly& ros,
      concord::kvbc::IBlocksAppender& ba,
      concord::thin_replica::SubBufferList& subscriber_list,
      std::unique_ptr<IDamlValidatorClient> validator,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry,
      concord::time::TimeContract* time_contract = nullptr)
      : ConcordCommandsHandler(config, node_config, ros, ba, subscriber_list,
                               prometheus_registry, time_contract),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord.daml")),
        validator_client_(std::move(validator)),
        enable_pipelined_commits_(
            IsPipelinedCommitExecutionEnabled(node_config)),
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

  bool DoCommitSingle(
      const com::digitalasset::kvbc::CommitRequest& commit_request,
      const kvbc::BlockId current_block_id,
      const google::protobuf::Timestamp& record_time,
      opentracing::Span& parent_span,
      std::optional<google::protobuf::Timestamp>* max_record_time,
      std::vector<std::string>& read_set, kvbc::SetOfKeyValuePairs& updates,
      kvbc::SetOfKeyValuePairs& updates_on_timeout,
      kvbc::SetOfKeyValuePairs& updates_on_conflict);

  bool DoCommitPipelined(const std::string& submission,
                         const google::protobuf::Timestamp& record_time,
                         const std::string& participant_id,
                         const std::string& correlation_id,
                         opentracing::Span& parent_span,
                         std::vector<std::string>& read_set,
                         kvbc::SetOfKeyValuePairs& updates);

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
      const kvbc::SetOfKeyValuePairs& updates, kvbc::BlockId current_block_id,
      const string& correlation_id, opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response);

  bool CommitPreExecutionResult(
      kvbc::BlockId current_block_id, google::protobuf::Timestamp& record_time,
      std::string& correlation_id, opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response);

  void BuildPreExecutionResult(
      const kvbc::SetOfKeyValuePairs& updates,
      const kvbc::SetOfKeyValuePairs& updates_on_timeout,
      const kvbc::SetOfKeyValuePairs& updates_on_conflict,
      kvbc::BlockId current_block_id,
      const std::optional<google::protobuf::Timestamp>& record_time,
      string& correlation_id,
      const std::vector<std::string>& validation_read_set,
      opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool RunDamlExecution(
      const com::digitalasset::kvbc::CommitRequest& commit_req,
      const string& entry_id, const google::protobuf::Timestamp& record_time,
      opentracing::Span& parent_span,
      com::digitalasset::kvbc::ValidateResponse& response,
      com::digitalasset::kvbc::ValidatePendingSubmissionResponse& response2);

  void GetUpdatesFromExecutionResult(
      const com::digitalasset::kvbc::Result& result, const string& entryId,
      kvbc::SetOfKeyValuePairs& updates,
      kvbc::SetOfKeyValuePairs& timeout_updates,
      kvbc::SetOfKeyValuePairs& conflict_updates) const;

  void BuildWriteSetsFromUpdates(
      const kvbc::SetOfKeyValuePairs& updates,
      const kvbc::SetOfKeyValuePairs& timeout_updates,
      const kvbc::SetOfKeyValuePairs& conflict_updates,
      com::vmware::concord::PreExecutionResult* result) const;

  std::optional<google::protobuf::Timestamp> GetPreExecutionMaxRecordTime();

  google::protobuf::Timestamp RecordTimeForTimeContract(
      concord::time::TimeContract* time_contract);

  static bool IsPipelinedCommitExecutionEnabled(
      const config::ConcordConfiguration& config);

  void GetUpdatesFromRawUpdates(
      const google::protobuf::RepeatedPtrField<
          ::com::digitalasset::kvbc::ProtectedKeyValuePair>& raw_updates,
      kvbc::SetOfKeyValuePairs& updates) const;
};

using DamlKvbReadFunc = std::function<std::map<std::string, std::string>(
    const google::protobuf::RepeatedPtrField<std::string>&)>;

// Enables DamlValidatorClient to read from storage while collecting updates to
// be written.
class WriteCollectingStorageOperations : public KeyValueStorageOperations {
 public:
  struct ValueWithThinReplicaIds {
    ValueWithThinReplicaIds() = default;
    ValueWithThinReplicaIds(const std::string& in_value,
                            const std::vector<std::string>& in_thin_replica_ids)
        : value(in_value), thin_replica_ids(in_thin_replica_ids) {}

    std::string value;
    std::vector<std::string> thin_replica_ids;
  };

  WriteCollectingStorageOperations(DamlKvbReadFunc kvb_read)
      : kvb_read_(kvb_read) {}

  std::map<std::string, std::string> Read(
      const google::protobuf::RepeatedPtrField<std::string>& keys) override;

  void Write(const std::string& key, const std::string& value,
             const std::vector<std::string>& thin_replica_ids) override;

  const std::unordered_map<std::string, ValueWithThinReplicaIds>&
  get_updates() {
    return updates_;
  }

 private:
  DamlKvbReadFunc kvb_read_;
  std::unordered_map<std::string, ValueWithThinReplicaIds> updates_;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
