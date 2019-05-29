// Copyright 2018-2019 VMware, all rights reserved
//

#ifndef CONCORD_HLF_KVB_COMMANDS_HANDLER_H_
#define CONCORD_HLF_KVB_COMMANDS_HANDLER_H_

#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "hlf/chaincode_invoker.hpp"
#include "hlf/kvb_storage.hpp"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"
#include "storage/blockchain_interfaces.h"

namespace concord {
namespace hlf {

class HlfKvbCommandsHandler : public concord::storage::ICommandsHandler {
 private:
  log4cplus::Logger logger_;
  concord::hlf::ChaincodeInvoker* chaincode_invoker_ = nullptr;
  const concord::config::ConcordConfiguration& config_;
  concord::config::ConcordConfiguration& node_config_;

  concord::storage::ILocalKeyValueStorageReadOnly* ptr_ro_storage_ = nullptr;
  concord::storage::IBlocksAppender* ptr_block_appender_ = nullptr;

 public:
  HlfKvbCommandsHandler(
      ChaincodeInvoker* chaincode_invoker,
      const concord::config::ConcordConfiguration& config,
      concord::config::ConcordConfiguration& node_config,
      concord::storage::ILocalKeyValueStorageReadOnly* ptr_ro_storage,
      concord::storage::IBlocksAppender* ptr_block_appender);

  ~HlfKvbCommandsHandler();

  int execute(uint16_t client_id, uint64_t sequence_num, bool read_only,
              uint32_t request_size, const char* request,
              uint32_t max_reply_size, char* out_reply,
              uint32_t& out_actual_reply_size) override;

 private:
  bool ExecuteCommand(
      uint32_t request_size, const char* request, uint64_t sequence_num,
      const concord::storage::ILocalKeyValueStorageReadOnly& ro_storage,
      concord::storage::IBlocksAppender& block_appender,
      const size_t max_reply_size, char* out_reply,
      uint32_t& out_reply_size) const;

  bool ExecuteReadOnlyCommand(
      uint32_t request_size, const char* request,
      const concord::storage::ILocalKeyValueStorageReadOnly& ro_storage,
      const size_t max_reply_size, char* out_reply,
      uint32_t& out_reply_size) const;

  // HLF extent

  // This function is used to update the write transaction and block to storage
  concord::consensus::Status StorageUpdate(
      const com::vmware::concord::HlfRequest& hlf_request,
      HlfKvbStorage* kvb_hlf_storage) const;

  bool HandleHlfRequest(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfRequestReadOnly(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInstallChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInstantiateChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfUpgradeChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInvokeChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfQueryChaincode(
      com::vmware::concord::ConcordRequest& concord_request,
      com::vmware::concord::ConcordResponse& concord_response) const;
};

}  // namespace hlf
}  // namespace concord

#endif  // CONCORDF_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_HPP
