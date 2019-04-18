// Copyright 2019 VMware, all rights reserved
//
// Utilities for reading the current time at this host, and adding that reading
// to a concord command.

#ifndef TIME_TIME_READING_HPP

#include "concord.pb.h"
#include "config/configuration_manager.hpp"

namespace concord {
namespace time {

bool IsTimeServiceEnabled(const concord::config::ConcordConfiguration &config);

uint64_t ReadTime();

void AddTimeToCommand(const concord::config::ConcordConfiguration &nodeConfig,
                      com::vmware::concord::ConcordRequest &command);

std::pair<std::string, uint64_t> GetTimeFromCommand(
    const com::vmware::concord::ConcordRequest &command);

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_READING_HPP
