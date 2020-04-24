// Copyright 2018 VMware, all rights reserved
//
// Aggregator for status events. Feeded by various modules

#ifndef COMMON_STATUS_AGGREGATOR_HPP
#define COMMON_STATUS_AGGREGATOR_HPP

#include <functional>
#include <memory>
#include <string>
#include <vector>
#include "communication/StatusInfo.h"
#include "utils/concord_utils.hpp"

namespace concord {
namespace common {

struct UiPeerInfo {
  std::string hostname;
  std::string address;
  std::int64_t millisSinceLastMessage;
  std::int32_t millisSinceLastMessageThreshold;
  std::string state;
};

class StatusAggregator {
 public:
  StatusAggregator();

  std::vector<UiPeerInfo> get_peers_info();

  /**
   * this function returns actual method that will be called by low level
   */
  bft::communication::UPDATE_CONNECTIVITY_FN get_update_connectivity_fn();

 private:
  class Impl;
  std::shared_ptr<Impl> _pImpl;
};

}  // namespace common
}  // namespace concord

#endif  // COMMON_STATUS_AGGREGATOR_HPP
