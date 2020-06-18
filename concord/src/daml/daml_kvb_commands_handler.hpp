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
  logging::Logger logger_;
  logging::Logger dtrmnsm_logger_;
  std::unique_ptr<IDamlValidatorClient> validator_client_;
  prometheus::Histogram& daml_exec_eng_dur_;
  prometheus::Histogram& daml_hdlr_exec_dur_;
  prometheus::Counter& write_ops_;
  prometheus::Counter& read_ops_;
  prometheus::Counter& failed_ops_;
  prometheus::Counter& execution_time_;
  prometheus::Summary& daml_kv_size_summary_;

 public:
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
        logger_(logging::getLogger("concord.daml")),
        dtrmnsm_logger_(logging::getLogger("concord.daml.determinism")),
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
        daml_kv_size_summary_{prometheus_registry->createSummary(
            command_handler_summaries_,
            {{"item", "daml_kv_size"}, {"layer", "DamlKvbCommandsHandler"}},
            {{0.25, 0.1}, {0.5, 0.1}, {0.75, 0.1}, {0.9, 0.1}})} {}

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

  bool DoCommitPipelined(const std::string& submission,
                         const google::protobuf::Timestamp& record_time,
                         const std::string& participant_id,
                         const std::string& correlation_id,
                         opentracing::Span& parent_span,
                         std::vector<std::string>& read_set,
                         kvbc::SetOfKeyValuePairs& updates,
                         bool isPreExecution);

  bool ExecuteCommand(const com::vmware::concord::ConcordRequest& request,
                      uint8_t flags, concord::time::TimeContract* time_contract,
                      opentracing::Span& parent_span,
                      com::vmware::concord::ConcordResponse& response);
  bool ExecuteReadOnlyCommand(
      const com::vmware::concord::ConcordRequest& request,
      com::vmware::concord::ConcordResponse& response);
  std::map<string, string> GetFromStorage(
      const google::protobuf::RepeatedPtrField<std::string>& keys);

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
      const string& correlation_id,
      const std::vector<std::string>& validation_read_set,
      opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response) const;

  void BuildWriteSetsFromUpdates(
      const kvbc::SetOfKeyValuePairs& updates,
      const kvbc::SetOfKeyValuePairs& timeout_updates,
      const kvbc::SetOfKeyValuePairs& conflict_updates,
      com::vmware::concord::PreExecutionResult* result) const;

  std::optional<google::protobuf::Timestamp> GetPreExecutionMaxRecordTime();

  google::protobuf::Timestamp RecordTimeForTimeContract(
      concord::time::TimeContract* time_contract);
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_KVB_COMMANDS_HANDLER_HPP_
