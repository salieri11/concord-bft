// Copyright 2019 VMware, all rights reserved

#include "time_contract.hpp"

#include <algorithm>
#include <unordered_map>
#include <vector>

#include "concord_storage.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/sliver.hpp"

using std::vector;

using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;
using concord::config::ParameterSelection;

namespace concord {
namespace time {

// Note we expect GetSignableUpdateData's implementation to produce the same
// series of bytes given the same time source ID and time, independent of the
// platform it is compiled for (note this includes independence of the system's
// endianness) and run on to the greatest extent possible, as if this function
// does not yield the same result on every node in the Concord cluster, the
// nodes will not be able to accept each other's time samples.
//
// At the time of this writing, the implementation of GetSignableUpdateData
// gives a concatenation of the bytes of the string source (in order, excluding
// any null terminator character) and the bytes of the time value (in
// little-endian order).
vector<uint8_t> TimeContract::GetSignableUpdateData(const string &source,
                                                    uint64_t time) {
  // Note std::string is defined to be std::basic_string<char>, that is, it is
  // generally treated as and operated on like an array or list of chars. Some
  // online references on std::string that were consulted in writing this
  // function document that std::string does not itself consider or handle
  // multi-char or variable-length character encodings; it just thinks of itself
  // as a list of char-type integers. Therefore, this function does not itself
  // have to consider the underlying character encoding, though this function
  // does (at this time) make the assumption that the same encoding (or at least
  // compatible encodings such as ASCII and UTF-8 using only ASCII characters)
  // is used on all nodes and time sources in this concord cluster.

  // Note that, another conceivable way in which the same string could have
  // different bytes on different machines is a case where the same character
  // encoding is used on every machine, but char has a different width on
  // different machines. This is probably unlikely, given that chars are often
  // assumed to be equivalent to bytes in C++, but, at the time of this writing,
  // the implementer of this function could not definitively verify that C++
  // does not permit char to be wider than a byte like it permits other integer
  // to be of greater width. Since this case is believed to be unlikely, we have
  // chosen simply not to support systems with unusually wide chars at this
  // time.
  static_assert(sizeof(char) == sizeof(uint8_t),
                "Current Concord time service implementation does not support "
                "platforms where char is not 1 byte.");

  vector<uint8_t> signable_data(source.length() + sizeof(uint64_t));

  const uint8_t *source_first_byte =
      reinterpret_cast<const uint8_t *>(source.data());
  const uint8_t *source_last_byte = source_first_byte + source.length();
  const uint8_t *time_first_byte = reinterpret_cast<const uint8_t *>(&time);
  const uint8_t *time_last_byte = time_first_byte + sizeof(time);
  uint8_t *current_data = signable_data.data();

  std::copy(source_first_byte, source_last_byte, current_data);
  current_data += source.length();
#ifdef BOOST_LITTLE_ENDIAN
  std::copy(time_first_byte, time_last_byte, current_data);
#else   // BOOST_LITTLE_ENDIAN not defined in this case
  std::reverse_copy(time_first_byte, time_last_byte, current_data);
#endif  // if BOOST_LITTLE_ENDIAN defined/ else

  return signable_data;
}

// Add a sample to the time contract.
uint64_t TimeContract::Update(const string &source, uint64_t time,
                              const vector<uint8_t> &signature) {
  LoadLatestSamples();

  auto old_sample = samples_->find(source);
  if (old_sample != samples_->end()) {
    if (verifier_.Verify(source, time, signature)) {
      if (time > old_sample->second.time) {
        old_sample->second.time = time;
        old_sample->second.signature = signature;
        StoreLatestSamples();
      }
    } else {
      LOG4CPLUS_WARN(logger_,
                     "Ignoring time sample with invalid signature claiming to "
                     "be from source \""
                         << source << "\".");
    }
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

  vector<uint64_t> times;
  for (auto s : *samples_) {
    times.push_back(s.second.time);
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

// Get the list of samples.
const unordered_map<string, TimeContract::SampleBody>
    &TimeContract::GetSamples() {
  LoadLatestSamples();
  return *samples_;
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
//
// Any entries in the storage containing invalid signatures will be ignored
// (with the special exception of entries for a recognized source containing
// both a 0 time and an empty signature, which may be used to indicate no sample
// is yet available for the given source. If the sample for a recognized source
// is rejected, that source's sample for this TimeContract will be initialized
// to the default of time 0. Any sample for an unrecognized source will always
// be completely ignored.
void TimeContract::LoadLatestSamples() {
  if (samples_) {
    // we already loaded the samples; don't load them again, or we could
    // overwrite updates that have been made
    return;
  }

  samples_ = new unordered_map<string, SampleBody>();

  concord::consensus::Sliver raw_time = storage_.get_time();
  if (raw_time.length() > 0) {
    com::vmware::concord::kvb::Time time_storage;
    if (time_storage.ParseFromArray(raw_time.data(), raw_time.length())) {
      if (time_storage.version() == kTimeStorageVersion) {
        LOG4CPLUS_DEBUG(logger_, "Loading " << time_storage.sample_size()
                                            << " time samples");
        for (int i = 0; i < time_storage.sample_size(); i++) {
          const com::vmware::concord::kvb::Time::Sample &sample =
              time_storage.sample(i);

          vector<uint8_t> signature(sample.signature().begin(),
                                    sample.signature().end());

          // Note time samples with time 0 are accepted from storage with a
          // blank signature as that may simply indicate that no valid time
          // sample was received from the given source before the time storage
          // we are reading was written.
          if (((sample.time() == 0) && (signature.size() == 0) &&
               (verifier_.HasTimeSource(sample.source()))) ||
              verifier_.Verify(sample.source(), sample.time(), signature)) {
            samples_->emplace(sample.source(), SampleBody());
            samples_->at(sample.source()).time = sample.time();
            samples_->at(sample.source()).signature = signature;
          } else {
            LOG4CPLUS_ERROR(logger_,
                            "Time storage contained invalid signature for "
                            "sample claimed to be from source: "
                                << sample.source() << ".");
            throw TimeException(
                "Cannot load time storage: found time update recorded with "
                "invalid signature.");
          }
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
      LOG4CPLUS_DEBUG(logger_, "source id: " << config_.getValue<string>(id));
      samples_->emplace(config_.getValue<string>(id), SampleBody());
      samples_->at(config_.getValue<string>(id)).time = 0;
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
    sample->set_time(s.second.time);
    sample->set_signature(s.second.signature.data(), s.second.signature.size());
  }

  size_t storage_size = proto.ByteSize();
  concord::consensus::Sliver time_storage(new uint8_t[storage_size],
                                          storage_size);
  proto.SerializeToArray(time_storage.data(), storage_size);

  storage_.set_time(time_storage);
}

}  // namespace time
}  // namespace concord
