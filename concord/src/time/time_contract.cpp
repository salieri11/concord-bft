// Copyright 2019 VMware, all rights reserved
//
// Time Contract is a state machine run by each replica to combine timestamps
// from other replicas into a non-decreasing time that has some resilience to
// false readings.
//
// Readings are submitted as (source, time) pairs, where source is a string
// identifier of the submitter, and time is an integer number of units since
// some starting point (milliseconds since the UNIX epoch is suggested).

#include "time_contract.hpp"

#include <algorithm>
#include <vector>

#include "blockchain/kvb_storage.hpp"
#include "concord_storage.pb.h"
#include "consensus/kvb/sliver.hpp"

namespace concord {
namespace time {

// Add a sample to the time contract.
uint64_t TimeContract::Update(const std::string &source, uint64_t time) {
  LoadLatestSamples();

  auto old_sample = samples_->find(source);
  if (old_sample != samples_->end()) {
    (*samples_)[source] = std::max(time, old_sample->second);
  } else {
    samples_->emplace(source, time);
  }

  StoreLatestSamples();

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

  samples_ = new std::map<std::string, uint64_t>();

  Blockchain::Sliver raw_time = storage_.get_time();
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
  Blockchain::Sliver time_storage(new uint8_t[storage_size], storage_size);
  proto.SerializeToArray(time_storage.data(), storage_size);

  storage_.set_time(time_storage);
}

}  // namespace time
}  // namespace concord
