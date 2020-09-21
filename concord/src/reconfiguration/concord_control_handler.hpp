// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_CONSENSUS_CONCORD_CONTROL_HANDLERS_HPP_
#define CONCORD_CONCORD_SRC_CONSENSUS_CONCORD_CONTROL_HANDLERS_HPP_

#include "bftengine/Replica.hpp"

namespace concord {
namespace reconfiguration {
class ConcordControlHandler : public bftEngine::ControlHandlers {
  bool onNoutOfNCheckpoint = false;
  bool onNMinusFOutOfNCheckpoint = false;

 public:
  void onSuperStableCheckpoint() override { onNoutOfNCheckpoint = true; };

  void onStableCheckpoint() override { onNMinusFOutOfNCheckpoint = true; }
  virtual ~ConcordControlHandler() {}

  bool isOnNOutOfNCheckpoint() { return onNoutOfNCheckpoint; }
  bool isOnStableCheckpoint() { return onNMinusFOutOfNCheckpoint; }
};
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_CONCORD_SRC_CONSENSUS_CONCORD_CONTROL_HANDLERS_HPP_
