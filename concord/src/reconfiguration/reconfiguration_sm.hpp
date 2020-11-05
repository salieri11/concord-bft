// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_

#include "reconfiguration_sm_open.hpp"

#include "concord.cmf.hpp"

namespace concord {
namespace reconfiguration {

class ReconfigurationSM : public ReconfigurationSMOpen {
 public:
  ReconfigurationSM(const kvbc::ILocalKeyValueStorageReadOnly&,
                    kvbc::IBlocksAppender&, kvbc::IBlocksDeleter&,
                    bftEngine::IStateTransfer&,
                    const config::ConcordConfiguration& config,
                    const config::ConcordConfiguration& node_config,
                    concord::time::TimeContract*);
  ReconfigurationSM() = default;
  // Use WedgeCommand handler from parent class but implement the others
  bool handle(const concord::messages::GetVersionCommand&) override;
  bool handle(const concord::messages::DownloadCommand&) override;
  bool handle(const concord::messages::UpgradeCommand&) override;
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
