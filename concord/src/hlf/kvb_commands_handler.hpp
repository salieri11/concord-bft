// Copyright 2018-2019 VMware, all rights reserved
//

#ifndef CONCORD_HLF_KVB_COMMANDS_HANDLER_H_
#define CONCORD_HLF_KVB_COMMANDS_HANDLER_H_

#include <opentracing/span.h>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <iostream>
#include "Logger.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/concord_commands_handler.hpp"
#include "db_interfaces.h"
#include "hlf/chaincode_invoker.hpp"
#include "hlf/kvb_storage.hpp"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"
#include "thin_replica/subscription_buffer.hpp"
#include "time/time_contract.hpp"

namespace concord {
namespace hlf {

class HlfKvbCommandsHandler
    : public concord::consensus::ConcordCommandsHandler {
 private:
  logging::Logger logger_;
  concord::hlf::ChaincodeInvoker* chaincode_invoker_ = nullptr;

  concord::config::ConcordConfiguration& node_config_;

 public:
  HlfKvbCommandsHandler(
      ChaincodeInvoker* chaincode_invoker,
      const concord::config::ConcordConfiguration& config,
      concord::config::ConcordConfiguration& node_config,
      const concord::kvbc::ILocalKeyValueStorageReadOnly& ro_storage,
      concord::kvbc::IBlocksAppender& block_appender,
      concord::kvbc::IBlocksDeleter& block_deleter,
      bftEngine::IStateTransfer& state_transfer,
      concord::thin_replica::SubBufferList& subscriber_list,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry);

  ~HlfKvbCommandsHandler();

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               const concord::consensus::ConcordRequestContext& request_context,
               uint8_t flags, concord::time::TimeContract* time_contract,
               opentracing::Span& parent_span,
               com::vmware::concord::ConcordResponse& response) override;
  void WriteEmptyBlock(concord::time::TimeContract* time_contract,
                       const opentracing::Span& parent_span) override;

 private:
  bool ExecuteCommand(const com::vmware::concord::ConcordRequest& request,
                      concord::time::TimeContract* time_contract,
                      com::vmware::concord::ConcordResponse& response);

  bool ExecuteReadOnlyCommand(
      const com::vmware::concord::ConcordRequest& request,
      concord::time::TimeContract* time_contract,
      com::vmware::concord::ConcordResponse& response) const;

  // HLF extent

  // This function is used to update the write transaction and block to storage
  concordUtils::Status StorageUpdate(
      const com::vmware::concord::HlfRequest& hlf_request,
      HlfKvbStorage* kvb_hlf_storage) const;

  // This function will write the chaincode raw bytes to a temporary chaincode
  // file of the local file system, and then use the peer command tool to
  // send HLF chaincode install request to HLF peer, after then,
  // remove the temporary chaincode file
  concordUtils::Status HandleIntermediateChaincodeFile(
      const com::vmware::concord::HlfRequest& hlf_request,
      std::string& error_msg) const;

  // The chaincode path must be under the GOPATH
  const std::string GetChaincodePath() const;

  bool HandleHlfRequest(
      const com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfPing(
      const com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfRequestReadOnly(
      const com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInstallChaincode(
      const com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfUpgradeChaincode(
      const com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfInvokeChaincode(
      const com::vmware::concord::ConcordRequest& concord_request,
      concord::hlf::HlfKvbStorage* kvb_hlf_storage,
      com::vmware::concord::ConcordResponse& concord_response) const;

  bool HandleHlfQueryChaincode(
      const com::vmware::concord::ConcordRequest& concord_request,
      com::vmware::concord::ConcordResponse& concord_response) const;
};

}  // namespace hlf
}  // namespace concord

#endif  // CONCORDF_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_HPP
