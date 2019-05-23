// Copyright 2018-2019 VMware, all rights reserved
//

#ifndef CONCORD_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_H_
#define CONCORD_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_H_

#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>

#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "hlf/concord_hlf_handler.hpp"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"

namespace concord {
namespace hlf {

class KvbCommandsHandlerForHlf : public Blockchain::ICommandsHandler {
 private:
  log4cplus::Logger logger_;
  concord::hlf::HlfHandler* hlf_handler_ = nullptr;
  const concord::config::ConcordConfiguration& config_;
  concord::config::ConcordConfiguration& node_config_;

  Blockchain::ILocalKeyValueStorageReadOnly* ptr_ro_storage_ = nullptr;
  Blockchain::IBlocksAppender* ptr_block_appender_ = nullptr;

 public:
  KvbCommandsHandlerForHlf(
      HlfHandler* hlf_handler,
      const concord::config::ConcordConfiguration& config,
      concord::config::ConcordConfiguration& node_config,
      Blockchain::ILocalKeyValueStorageReadOnly* ptr_ro_storage,
      Blockchain::IBlocksAppender* ptr_block_appender);

  ~KvbCommandsHandlerForHlf();

  int execute(uint16_t client_id, uint64_t sequence_num, bool read_only,
              uint32_t request_size, const char* request,
              uint32_t max_reply_size, char* out_reply,
              uint32_t& out_actual_reply_size) override;

 private:
  bool ExecuteCommand(
      uint32_t request_size, const char* request, uint64_t sequence_num,
      const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage,
      Blockchain::IBlocksAppender& block_appender, const size_t max_reply_size,
      char* out_reply, uint32_t& out_reply_size) const;

  bool ExecuteReadOnlyCommand(
      uint32_t request_size, const char* request,
      const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage,
      const size_t max_reply_size, char* out_reply,
      uint32_t& out_reply_size) const;

  // HLF extent
  bool HandleHlfRequest(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfRequestReadOnly(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInstallChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInstantiateChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfUpgradeChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInvokeChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfQueryChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::blockchain::hlf::KvbStorageForHlf* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;
};

}  // namespace hlf
}  // namespace concord

#endif  // CONCORDF_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_HPP
