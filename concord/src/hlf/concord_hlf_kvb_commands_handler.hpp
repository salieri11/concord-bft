// Copyright 2018-2019 VMware, all rights reserved
//

#ifndef CONCORD_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_HPP
#define CONCORD_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_HPP

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

class KVBCommandsHandlerForHlf : public Blockchain::ICommandsHandler {
 private:
  log4cplus::Logger logger;
  concord::hlf::HlfHandler *hlfHandler_ = nullptr;
  const concord::config::ConcordConfiguration &config_;
  concord::config::ConcordConfiguration &nodeConfiguration;

  Blockchain::ILocalKeyValueStorageReadOnly *m_ptrRoStorage = nullptr;
  Blockchain::IBlocksAppender *m_ptrBlockAppender = nullptr;

 public:
  KVBCommandsHandlerForHlf(HlfHandler *hlfHandler,
                           const concord::config::ConcordConfiguration &config,
                           concord::config::ConcordConfiguration &nodeConfig,
                           Blockchain::ILocalKeyValueStorageReadOnly *roStorage,
                           Blockchain::IBlocksAppender *appendder);

  ~KVBCommandsHandlerForHlf();

  int execute(uint16_t clientId, uint64_t sequenceNum, bool readOnly,
              uint32_t requestSize, const char *request, uint32_t maxReplySize,
              char *outReply, uint32_t &outActualReplySize) override;

 private:
  bool executeCommand(
      uint32_t requestSize, const char *request, uint64_t sequenceNum,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      Blockchain::IBlocksAppender &blockAppender, const size_t maxReplySize,
      char *outReply, uint32_t &outReplySize) const;

  bool executeReadOnlyCommand(
      uint32_t requestSize, const char *request,
      const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
      const size_t maxReplySize, char *outReply, uint32_t &outReplySize) const;

  // HLF extent
  bool handle_hlf_request(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  bool handle_hlf_request_read_only(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  bool handle_hlf_install_chaincode(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  bool handle_hlf_instantiate_chaincode(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  bool handle_hlf_upgrade_chaincode(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  bool handle_hlf_invoke_chaincode(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;

  bool handle_hlf_query_chaincode(
      com::vmware::concord::ConcordRequest &athreq,
      concord::blockchain::hlf::KVBHlfStorage *kvbHlfStorage,
      com::vmware::concord::ConcordResponse &athresp) const;
};

}  // namespace hlf
}  // namespace concord

#endif  // CONCORDF_CONSENSUS_HLF_KVB_COMMANDS_HANDLER_HPP
