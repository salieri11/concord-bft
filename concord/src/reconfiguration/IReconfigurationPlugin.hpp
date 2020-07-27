// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_

#include <opentracing/tracer.h>
#include <string>
#include "bftengine/ControlStateManager.hpp"
#include "concord.pb.h"

namespace concord {
namespace reconfiguration {

struct PluginReply {
  bool succ;
  std::string data;
};

class IReconfigurationPlugin {
  com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId_;

 public:
  explicit IReconfigurationPlugin(
      com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId)
      : pluginId_(pluginId) {}
  virtual ~IReconfigurationPlugin() {}
  com::vmware::concord::ReconfigurationSmRequest::PluginId GetPluginId() const {
    return pluginId_;
  }
  virtual PluginReply Handle(const std::string& command, uint64_t sequence_num,
                             bool readOnly, opentracing::Span& parent_span,
                             std::shared_ptr<bftEngine::ControlStateManager>
                                 control_state_manager = nullptr) = 0;
};
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
