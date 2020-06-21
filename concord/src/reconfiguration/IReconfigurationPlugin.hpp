// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_

#include <opentracing/tracer.h>
#include <string>
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
  virtual PluginReply Handle(const std::string& command, bool readOnly,
                             opentracing::Span& parent_span) = 0;
};
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
