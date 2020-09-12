// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_

#include "reconfiguration_sm_open.hpp"

namespace concord {
namespace reconfiguration {

class ReconfigurationSM : public ReconfigurationSMOpen {
 public:
  ReconfigurationSM(
      const config::ConcordConfiguration& config,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
      : ReconfigurationSMOpen(config, prometheus_registry) {}
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
