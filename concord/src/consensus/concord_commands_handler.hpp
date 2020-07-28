// Copyright 2019 VMware, all rights reserved
//
// Shim between generic KVB and Concord-specific commands handlers.

#ifndef CONSENSUS_CONCORD_COMMANDS_HANDLER_HPP_
#define CONSENSUS_CONCORD_COMMANDS_HANDLER_HPP_

#include <opentracing/span.h>
#include <prometheus/counter.h>
#include <utils/concord_prometheus_metrics.hpp>
#include "KVBCInterfaces.h"
#include "Logger.hpp"
#include "OpenTracing.hpp"
#include "concord.pb.h"
#include "db_interfaces.h"
#include "kv_types.hpp"
#include "pruning/kvb_pruning_sm.hpp"
#include "reconfiguration/concord_control_handler.hpp"
#include "reconfiguration/reconfiguration_sm.hpp"
#include "storage/concord_block_metadata.h"
#include "thin_replica/subscription_buffer.hpp"
#include "time/time_contract.hpp"
#include "time/time_reading.hpp"

namespace concord {
namespace consensus {

struct ConcordRequestContext {
  uint16_t client_id;
  uint64_t sequence_num;
  uint32_t max_response_size;
};

class ConcordCommandsHandler : public concord::kvbc::ICommandsHandler,
                               public concord::kvbc::IBlocksAppender {
 private:
  logging::Logger logger_;
  uint64_t executing_bft_sequence_num_;
  concord::thin_replica::SubBufferList &subscriber_list_;
  std::shared_ptr<reconfiguration::ConcordControlHandler>
      concord_control_handlers_;

  void PublishUpdatesToThinReplicaServer(kvbc::BlockId block_id,
                                         kvbc::SetOfKeyValuePairs &updates);

  uint32_t SerializeResponse(const opentracing::Span &parent_span,
                             uint32_t max_response_size,
                             com::vmware::concord::ConcordResponse &response,
                             char *response_buffer) const;
  void UpdateTime(const opentracing::Span &parent_span,
                  const com::vmware::concord::ConcordRequest &request,
                  bool read_only, uint16_t client_id);
  void UpdateResponseWithTime(
      const opentracing::Span &parent_span,
      const com::vmware::concord::TimeRequest &time_request,
      com::vmware::concord::ConcordResponse &response);
  void AddTimeUpdateBlock(const opentracing::Span &parent_span,
                          com::vmware::concord::ConcordResponse &response,
                          int execute_result, bool read_only);

  uint16_t replica_id_;

 protected:
  const concord::kvbc::ILocalKeyValueStorageReadOnly &storage_;
  concord::storage::ConcordBlockMetadata metadata_storage_;
  prometheus::Family<prometheus::Counter> &command_handler_counters_;
  prometheus::Family<prometheus::Histogram> &command_handler_histograms_;
  prometheus::Family<prometheus::Summary> &command_handler_summaries_;
  prometheus::Counter &written_blocks_;
  prometheus::Summary &internal_kv_size_summary_;
  const kvbc::Key cid_key_ = kvbc::Key(
      new decltype(storage::kKvbKeyCorrelationId)[1]{
          storage::kKvbKeyCorrelationId},
      1);
  std::shared_ptr<bftEngine::ControlStateManager> controlStateManager_;

 public:
  concord::kvbc::IBlocksAppender &appender_;
  std::unique_ptr<concord::time::TimeContract> time_;
  std::unique_ptr<concord::pruning::KVBPruningSM> pruning_sm_;
  std::unique_ptr<concord::reconfiguration::ReconfigurationSM>
      reconfiguration_sm_;

 public:
  ConcordCommandsHandler(
      const concord::config::ConcordConfiguration &config,
      const concord::config::ConcordConfiguration &node_config,
      const concord::kvbc::ILocalKeyValueStorageReadOnly &storage,
      concord::kvbc::IBlocksAppender &appender,
      concord::thin_replica::SubBufferList &subscriber_list,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry,
      concord::time::TimeContract *time_contract = nullptr);
  virtual ~ConcordCommandsHandler() = default;

  // Callback from the replica via ICommandsHandler.
  int execute(uint16_t client_id, uint64_t sequence_num, uint8_t flags,
              uint32_t request_size, const char *request,
              uint32_t max_reply_size, char *out_reply,
              uint32_t &out_reply_size,
              uint32_t &out_replica_specific_info_size,
              concordUtils::SpanWrapper &parent_span) override;

  // Parses the request buffer in case it contains a pre-execution response
  static bool parseFromPreExecutionResponse(
      const char *request_buffer, uint32_t request_size,
      com::vmware::concord::ConcordRequest &request);

  // Our concord::storage::blockchain::IBlocksAppender implementation, where we
  // can add lower-level data like time contract status, before forwarding to
  // the true appender.
  concordUtils::Status addBlock(const kvbc::SetOfKeyValuePairs &updates,
                                kvbc::BlockId &out_block_id,
                                const concordUtils::SpanWrapper &parent_span =
                                    concordUtils::SpanWrapper{}) override;

  // Checks the pre-executed result for read/write conflicts
  bool HasPreExecutionConflicts(const com::vmware::concord::PreExecutionResult
                                    &pre_execution_result) const;

  // Functions the subclass must implement are below here.

  // The up-call to execute a command. This base class's execute function calls
  // this Execute function after decoding the request buffer.
  //
  // `timeContract` will only be a valid pointer if time server is enabled. It
  // will be nullptr otherwise.
  //
  // The subclass should fill out any fields in `response` that it wants to
  // return to the client.
  virtual bool Execute(
      const com::vmware::concord::ConcordRequest &request,
      const concord::consensus::ConcordRequestContext &request_context,
      uint8_t flags, concord::time::TimeContract *time_contract,
      opentracing::Span &parent_span,
      com::vmware::concord::ConcordResponse &response) = 0;

  // In some cases, commands may arrive that require writing a KVB block to
  // store state that is not controlled by the subclass. This callback gives the
  // subclass a chance to add its own data to that block (for example, an
  // "empty" smart-contract-level block).
  virtual void WriteEmptyBlock(concord::time::TimeContract *time_contract,
                               const opentracing::Span &parent_span) = 0;

  void setControlStateManager(std::shared_ptr<bftEngine::ControlStateManager>
                                  controlStateManager) override;

  virtual std::shared_ptr<bftEngine::ControlHandlers> getControlHandlers()
      override;

  static std::string SerializeFingerprint(const kvbc::BlockId fingerprint);
  static kvbc::BlockId DeserializeFingerprint(const std::string &data);
};

}  // namespace consensus
}  // namespace concord

#endif  // CONSENSUS_CONCORD_COMMANDS_HANDLER_HPP_
