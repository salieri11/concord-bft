// Copyright 2019 VMware, all rights reserved
//
// Shim between generic KVB and Concord-specific commands handlers.

#ifndef CONSENSUS_CONCORD_COMMANDS_HANDLER_HPP_
#define CONSENSUS_CONCORD_COMMANDS_HANDLER_HPP_

#include "KVBCInterfaces.h"
#include "blockchain/db_interfaces.h"
#include "concord.pb.h"
#include "consensus/timing_stat.h"
#include "pruning/kvb_pruning_sm.hpp"
#include "storage/concord_block_metadata.h"
#include "thin_replica/subscription_buffer.hpp"
#include "time/time_contract.hpp"
#include "time/time_reading.hpp"

#include <log4cplus/logger.h>
#include <opentracing/span.h>
#include <chrono>

namespace concord {
namespace consensus {

class ConcordCommandsHandler
    : public concord::kvbc::ICommandsHandler,
      public concord::storage::blockchain::IBlocksAppender {
 private:
  log4cplus::Logger logger_;
  concord::storage::ConcordBlockMetadata metadata_storage_;
  uint64_t executing_bft_sequence_num_;
  std::chrono::steady_clock::duration timing_log_period_;
  std::chrono::steady_clock::time_point timing_log_last_;
  concord::thin_replica::SubBufferList &subscriber_list_;

  void log_timing();

 protected:
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly &storage_;
  bool timing_enabled_;
  concordMetrics::Component metrics_;

 public:
  concord::storage::blockchain::IBlocksAppender &appender_;
  std::unique_ptr<concord::time::TimeContract> time_;
  concord::pruning::KVBPruningSM pruning_;

  com::vmware::concord::ConcordRequest request_;
  com::vmware::concord::ConcordResponse response_;

 public:
  ConcordCommandsHandler(
      const concord::config::ConcordConfiguration &config,
      const concord::config::ConcordConfiguration &node_config,
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly
          &storage,
      concord::storage::blockchain::IBlocksAppender &appender,
      concord::thin_replica::SubBufferList &subscriber_list);
  virtual ~ConcordCommandsHandler() {}

  // Callback from the replica via ICommandsHandler.
  int execute(uint16_t client_id, uint64_t sequence_num, bool read_only,
              uint32_t request_size, const char *request,
              uint32_t max_reply_size, char *out_reply,
              uint32_t &out_reply_size) override;

  // Our concord::storage::blockchain::IBlocksAppender implementation, where we
  // can add lower-level data like time contract status, before forwarding to
  // the true appender.
  concordUtils::Status addBlock(
      const concord::storage::SetOfKeyValuePairs &updates,
      concordUtils::BlockId &out_block_id) override;

  // Functions the subclass must implement are below here.

  // The up-call to execute a command. This base class's execute function calls
  // this Execute function after decoding the request buffer.
  //
  // `timeContract` will only be a valid pointer if time server is enabled. It
  // will be nullptr otherwise.
  //
  // The subclass should fill out any fields in `response` that it wants to
  // return to the client.
  virtual bool Execute(const com::vmware::concord::ConcordRequest &request,
                       bool read_only,
                       concord::time::TimeContract *time_contract,
                       opentracing::Span &parent_span,
                       com::vmware::concord::ConcordResponse &response) = 0;

  // In some cases, commands may arrive that require writing a KVB block to
  // store state that is not controlled by the subclass. This callback gives the
  // subclass a chance to add its own data to that block (for example, an
  // "empty" smart-contract-level block).
  virtual void WriteEmptyBlock(concord::time::TimeContract *time_contract) = 0;

  // After stats have been logged, this function will be called to allow the
  // subclass to clear any stats before beginning to count for the next
  // interval.
  virtual void ClearStats(){};
};

}  // namespace consensus
}  // namespace concord

#endif  // CONSENSUS_CONCORD_COMMANDS_HANDLER_HPP_
