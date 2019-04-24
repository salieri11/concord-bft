// Copyright 2019 VMware, all rights reserved

#include "time_reading.hpp"

#include <log4cplus/loggingmacros.h>
#include <chrono>

#include "concord.pb.h"
#include "config/configuration_manager.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::TimeUpdate;
using concord::config::ConcordConfiguration;
using std::chrono::system_clock;

namespace concord {
namespace time {

// Return true if the time service is enabled, or false if it is disabled.
bool IsTimeServiceEnabled(const ConcordConfiguration &config) {
  return config.getValue<bool>("FEATURE_time_service");
}

// Read milliseconds since the UNIX Epoch, according to the system clock.
//
// Eventually this should take a Config object, and use it to decide how to read
// the time.
uint64_t ReadTime() {
  system_clock::time_point now = system_clock::now();
  system_clock::duration since_epoch = now.time_since_epoch();
  return std::chrono::duration_cast<std::chrono::milliseconds>(since_epoch)
      .count();
}

// Add a time reading to a command, so the time contract will be updated when
// the command is executed by a replica.
void AddTimeToCommand(const ConcordConfiguration &nodeConfig,
                      ConcordRequest &command) {
  if (nodeConfig.hasValue<std::string>("time_source_id")) {
    TimeUpdate *tu = command.mutable_time_update();
    tu->set_source(nodeConfig.getValue<std::string>("time_source_id"));
    tu->set_time(ReadTime());
  }
}

std::pair<std::string, uint64_t> GetTimeFromCommand(
    const ConcordRequest &command) {
  if (!command.has_time_update()) {
    return std::make_pair("", 0);
  }

  TimeUpdate tu = command.time_update();
  return std::make_pair(tu.source(), tu.time());
}

}  // namespace time
}  // namespace concord
