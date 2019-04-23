// Copyright 2019 VMware, all rights reserved
//
// Time Contract is a state machine run by each replica to combine timestamps
// from other replicas into a non-decreasing time that has some resilience to
// false readings.
//
// Readings are submitted as (source, time) pairs, where source is a string
// identifier of the submitter, and time is an integer number of units since
// some starting point (units are unspecified, but milliseconds since the UNIX
// epoch is suggested). See TimeContract::Update.
//
// Aggregation can be done via any statistical means that give the guarantees
// discussed above. The current implementation is to choose the median of the
// most recent readings. See TimeContract::SummarizeTime.

#ifndef TIME_TIME_CONTRACT_HPP

#include <unordered_map>
#include <utility>

#include "blockchain/kvb_storage.hpp"

namespace concord {
namespace time {

class TimeException : public std::exception {
 public:
  explicit TimeException(const std::string& what) : msg_(what) {}

  const char* what() const noexcept override { return msg_.c_str(); }

 private:
  std::string msg_;
};

const int64_t kTimeStorageVersion = 1;

class TimeContract {
 public:
  explicit TimeContract(concord::blockchain::KVBStorage& storage)
      : logger_(log4cplus::Logger::getInstance("concord.time")),
        storage_(storage),
        samples_(nullptr) {}

  ~TimeContract() {
    if (samples_) {
      delete samples_;
    }
  }

  uint64_t Update(const std::string& source, uint64_t time);
  uint64_t GetTime();
  void StoreLatestSamples();

 private:
  log4cplus::Logger logger_;
  concord::blockchain::KVBStorage& storage_;
  std::unordered_map<std::string, uint64_t>* samples_;

  void LoadLatestSamples();
  uint64_t SummarizeTime();
};

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_CONTRACT_HPP
