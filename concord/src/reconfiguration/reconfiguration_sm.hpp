// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_

#include "reconfiguration_sm_open.hpp"

#include "concord.pb.h"

namespace concord {
namespace reconfiguration {

class ReconfigurationSM : public ReconfigurationSMOpen {
 public:
  // Use WedgeCommand handler from parent class but implement the others
  bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::GetVersionCommand&)
      override;
  bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::DownloadCommand&)
      override;
  bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::UpgradeCommand&)
      override;
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
