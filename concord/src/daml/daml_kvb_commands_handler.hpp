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
concordUtils::Sliver CreateDamlKvbValue(const std::string& value,
                                        const std::vector<string>& trid_list);
void AccessControlListToThinReplicaIds(
    const com::digitalasset::kvbc::AccessControlList& access_control_list,
    std::vector<string>& thin_replica_ids);

bool CheckIfWithinTimeBounds(
    const com::digitalasset::kvbc::PreExecutionOutput& pre_execution_output,
    const google::protobuf::Timestamp& record_time);

class DamlKvbCommandsHandler
    : public concord::consensus::ConcordCommandsHandler {
 public:
  // The key used to communicate record time to participants for pre-executed
  // submissions.
  static std::string kTimeUpdateKey;

  DamlKvbCommandsHandler(
      const concord::config::ConcordConfiguration& config,
      const concord::config::ConcordConfiguration& node_config,
      const concord::kvbc::ILocalKeyValueStorageReadOnly& ros,
      concord::kvbc::IBlocksAppender& blocks_appender,
      concord::kvbc::IBlocksDeleter& blocks_deleter,
      bftEngine::IStateTransfer& state_transfer,
      concord::thin_replica::SubBufferList& subscriber_list,
      std::unique_ptr<concord::reconfiguration::IReconfigurationDispatcher>
          reconfiguration_sm,
      std::unique_ptr<IDamlValidatorClient> validator,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry,
      concord::time::TimeContract* time_contract = nullptr)
      : ConcordCommandsHandler(config, node_config, ros, blocks_appender,
                               blocks_deleter, state_transfer, subscriber_list,
                               std::move(reconfiguration_sm),
                               prometheus_registry, time_contract),
        logger_(logging::getLogger("concord.daml")),
        determinism_logger_(logging::getLogger("concord.daml.determinism")),
        validator_client_(std::move(validator)),
        daml_exec_eng_dur_{prometheus_registry->createHistogram(
            command_handler_histograms_,
            {{"layer", "DamlKvbCommandsHandler"},
             {"duration", "daml_external_validation_duration_ms"}},
            {10, 20, 40, 80, 160, 320, 480, 640, 960, 1280})},
        daml_hdlr_exec_dur_{prometheus_registry->createHistogram(
            command_handler_histograms_,
            {{"layer", "DamlKvbCommandsHandler"},
             {"duration", "daml_handler_duration_ms"}},
            {10, 20, 40, 80, 160, 320, 480, 640, 960, 1280})},

        write_ops_{prometheus_registry->createCounter(
            command_handler_counters_, {{"layer", "DamlKvbCommandsHandler"},
                                        {"operation", "daml_writes"}})},
        read_ops_{prometheus_registry->createCounter(
            command_handler_counters_, {{"layer", "DamlKvbCommandsHandler"},
                                        {"operation", "daml_reads"}})},
        failed_ops_{prometheus_registry->createCounter(
            command_handler_counters_, {{"layer", "DamlKvbCommandsHandler"},
                                        {"operation", "daml_failures"}})},
        execution_time_{prometheus_registry->createCounter(
            command_handler_counters_, {{"layer", "DamlKvbCommandsHandler"},
                                        {"operation", "daml_execution_time"}})},
        post_execution_success_{prometheus_registry->createCounter(
            command_handler_counters_,
            {{"layer", "DamlKvbCommandsHandler"},
             {"operation", "post_execution_successes"}})},
        post_execution_timeout_{prometheus_registry->createCounter(
            command_handler_counters_,
            {{"layer", "DamlKvbCommandsHandler"},
             {"operation", "post_execution_timeouts"}})},
        read_keys_count_{prometheus_registry->createSummary(
            command_handler_summaries_,
            {{"layer", "DamlKvbCommandsHandler"},
             {"item", "daml_keys_read_count"}},
            {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
        written_keys_count_{prometheus_registry->createSummary(
            command_handler_summaries_,
            {{"layer", "DamlKvbCommandsHandler"},
             {"item", "daml_keys_written_count"}},
            {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})},
        daml_kv_size_summary_{prometheus_registry->createSummary(
            command_handler_summaries_,
            {{"item", "daml_kv_size"}, {"layer", "DamlKvbCommandsHandler"}},
            {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})} {}

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               const concord::consensus::ConcordRequestContext& request_context,
               const uint8_t flags, concord::time::TimeContract* time_contract,
               opentracing::Span& parent_span,
               com::vmware::concord::ConcordResponse& response) override;
  void WriteEmptyBlock(concord::time::TimeContract* time_contract,
                       const opentracing::Span& parent_span) override;

  bool PostExecute(
      const com::vmware::concord::PreExecutionResult& pre_execution_result,
      concord::time::TimeContract* time, const opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response);

  std::map<std::string, ValueFingerprintPair> ReadKeys(
      const google::protobuf::RepeatedPtrField<string>& keys);

 private:
  bool ExecuteRead(const com::digitalasset::kvbc::ReadCommand& read_cmd,
                   com::vmware::concord::ConcordResponse& concord_response);

  bool ExecuteCommit(const com::digitalasset::kvbc::CommitRequest& commit_req,
                     uint8_t flags, concord::time::TimeContract* time_contract,
                     const opentracing::Span& parent_span,
                     com::vmware::concord::ConcordResponse& concord_response);
  bool PreExecute(const com::digitalasset::kvbc::CommitRequest& commit_request,
                  const opentracing::Span& parent_span,
                  com::vmware::concord::ConcordResponse& concord_response);

  bool GenerateWriteSetForPreExecution(
      const com::digitalasset::kvbc::PreExecutionOutput& pre_execution_output,
      const google::protobuf::Timestamp& record_time,
      kvbc::SetOfKeyValuePairs& write_set) const;

  void WriteSetToRawUpdates(
      const com::digitalasset::kvbc::WriteSet& input_write_set,
      kvbc::SetOfKeyValuePairs& updates) const;

  bool DoCommitPipelined(const std::string& submission,
                         const google::protobuf::Timestamp& record_time,
                         const std::string& participant_id,
                         const std::string& correlation_id,
                         const opentracing::Span& parent_span,
                         std::vector<std::string>& read_set,
                         kvbc::SetOfKeyValuePairs& updates);

  bool ExecuteCommand(const com::vmware::concord::ConcordRequest& request,
                      uint8_t flags, concord::time::TimeContract* time_contract,
                      const opentracing::Span& parent_span,
                      com::vmware::concord::ConcordResponse& response);
  bool ExecuteReadOnlyCommand(
      const com::vmware::concord::ConcordRequest& request,
      com::vmware::concord::ConcordResponse& response);
  std::map<string, std::pair<string, kvbc::BlockId>> GetFromStorage(
      const google::protobuf::RepeatedPtrField<std::string>& keys);

  void RecordTransaction(
      const kvbc::SetOfKeyValuePairs& updates,
      const kvbc::BlockId current_block_id, const string& correlation_id,
      const opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response);

  google::protobuf::Timestamp RecordTimeForTimeContract(
      concord::time::TimeContract* time_contract);

  logging::Logger logger_;
  logging::Logger determinism_logger_;
  std::unique_ptr<IDamlValidatorClient> validator_client_;
  prometheus::Histogram& daml_exec_eng_dur_;
  prometheus::Histogram& daml_hdlr_exec_dur_;
  prometheus::Counter& write_ops_;
  prometheus::Counter& read_ops_;
  prometheus::Counter& failed_ops_;
  prometheus::Counter& execution_time_;
  prometheus::Counter& post_execution_success_;
  prometheus::Counter& post_execution_timeout_;
  prometheus::Summary& read_keys_count_;
  prometheus::Summary& written_keys_count_;
  prometheus::Summary& daml_kv_size_summary_;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
