// Copyright 2019 VMware, all rights reserved

#include "time_contract.hpp"

#include <algorithm>
#include <unordered_map>
#include <vector>

#include "concord_storage.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/kvb/sliver.hpp"

using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;
using concord::config::ParameterSelection;

namespace concord {
namespace time {

// Add a sample to the time contract.
uint64_t TimeContract::Update(const std::string &source, uint64_t time) {
  LoadLatestSamples();

  auto old_sample = samples_->find(source);
  if (old_sample != samples_->end()) {
    old_sample->second = std::max(time, old_sample->second);
    StoreLatestSamples();
  } else {
    LOG4CPLUS_WARN(logger_,
                   "Ignoring sample from uknown source \"" << source << "\"");
  }

  return SummarizeTime();
}

// Get the current time at the latest block (including any updates that have
// been applied since this TimeContract was instantiated).
uint64_t TimeContract::GetTime() {
  LoadLatestSamples();

  return SummarizeTime();
}

// Combine samples into a single defintion of "now". Samples must have been
// loaded before this function is called.
//
// TODO: refuse to give a summary if there are not enough samples to guarantee
// monotonicity
uint64_t TimeContract::SummarizeTime() {
  assert(samples_);

  if (samples_->empty()) {
    return 0;
  }

  std::vector<uint64_t> times;
  for (auto s : *samples_) {
    times.push_back(s.second);
  }

  // middle is either the actual median, or the high side of it for even counts
  // - remember zero indexing!
  //  odd: 1 2 3 4 5 ... 5 / 2 = 2
  //  even: 1 2 3 4 5 6 ... 6 / 2 = 3
  int middle = times.size() / 2;

  // only need to sort the first "half" to find out where the median is
  std::partial_sort(times.begin(), times.begin() + middle + 1, times.end());

  uint64_t result;
  if (times.size() % 2 == 0) {
    return (*(times.begin() + middle) + *(times.begin() + (middle - 1))) / 2;
  } else {
    return *(times.begin() + middle);
  }

  return result;
}

// Find node[*].time_source_id fields in the config.
static bool TimeSourceIdSelector(const ConcordConfiguration &config,
                                 const ConfigurationPath &path, void *state) {
  // isScope: the parameter is inside "node" scope
  // useInstance: we don't care about the template
  return path.isScope && path.useInstance &&
         path.subpath->name == "time_source_id";
}

// Load samples from storage, if they haven't been already.
//
// An exception is thrown if data was found in the time key in storage, but that
// data could not be parsed.
void TimeContract::LoadLatestSamples() {
  if (samples_) {
    // we already loaded the samples; don't load them again, or we could
    // overwrite updates that have been made
    return;
  }

  samples_ = new std::unordered_map<std::string, uint64_t>();

  concord::consensus::Sliver raw_time = storage_.get_time();
  if (raw_time.length() > 0) {
    com::vmware::concord::kvb::Time time_storage;
    if (time_storage.ParseFromArray(raw_time.data(), raw_time.length())) {
      if (time_storage.version() == kTimeStorageVersion) {
        LOG4CPLUS_DEBUG(logger_, "Loading " << time_storage.sample_size()
                                            << " time samples");
        for (int i = 0; i < time_storage.sample_size(); i++) {
          samples_->emplace(time_storage.sample(i).source(),
                            time_storage.sample(i).time());
        }
      } else {
        LOG4CPLUS_ERROR(logger_, "Unknown time storage version: "
                                     << time_storage.version());
        throw TimeException("Unknown time storage version");
      }
    } else {
      LOG4CPLUS_ERROR(logger_, "Unable to parse time storage");
      throw TimeException("Unable to parse time storage");
    }
  } else {
    // This const_cast is ugly. We don't actually change the config values, but
    // the iterator registers itself with the config object, so the reference
    // can't be const.
    ParameterSelection time_source_ids(
        const_cast<ConcordConfiguration &>(config_), TimeSourceIdSelector,
        nullptr);
    for (auto id : time_source_ids) {
      LOG4CPLUS_DEBUG(logger_,
                      "source id: " << config_.getValue<std::string>(id));
      samples_->emplace(config_.getValue<std::string>(id), 0);
    }

    LOG4CPLUS_INFO(logger_, "Initializing time contract with "
                                << samples_->size() << " sources");
  }
}

// Write the map to storage.
void TimeContract::StoreLatestSamples() {
  com::vmware::concord::kvb::Time proto;
  proto.set_version(kTimeStorageVersion);

  for (auto s : *samples_) {
    auto sample = proto.add_sample();

    sample->set_source(s.first);
    sample->set_time(s.second);
  }

  size_t storage_size = proto.ByteSize();
  concord::consensus::Sliver time_storage(new uint8_t[storage_size],
                                          storage_size);
  proto.SerializeToArray(time_storage.data(), storage_size);

  storage_.set_time(time_storage);
}

}  // namespace time
}  // namespace concord
