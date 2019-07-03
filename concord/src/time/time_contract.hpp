// Copyright 2019 VMware, all rights reserved
//
// Time Contract is a state machine run by each replica to combine timestamps
// from other replicas into a non-decreasing time that has some resilience to
// false readings.
//
// Readings are submitted as (source, time, signature) triples, where source is
// a string identifier of the submitter, time is an integer number of units
// since some starting point (units are unspecified, but milliseconds since the
// UNIX epoch is suggested) (See TimeContract::Update), and signature is a
// digital signature proving that this time sample actually originates from the
// named source (based on a public key given for that time source in this
// Concord cluster's configuration).
//
// Aggregation can be done via any statistical means that give the guarantees
// discussed above. The current implementation is to choose the median of the
// most recent readings. See TimeContract::SummarizeTime.

#ifndef TIME_TIME_CONTRACT_HPP

#include <unordered_map>
#include <utility>
#include <vector>

#include "config/configuration_manager.hpp"
#include "ethereum/eth_kvb_storage.hpp"
#include "time/time_signing.hpp"

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
  // Given the logical content of a time update, computes the data that we
  // expect a signature over to validate that time update.
  static std::vector<uint8_t> GetSignableUpdateData(const std::string& source,
                                                    uint64_t time);

  // Constructor for TimeContract. Arguments:
  //   - storage: KVB Storage backend to persist the state of the time contract
  //   to.
  //   - config: ConcordConfiguration for this Concord cluster; note that the
  //   set of valid time sources and the public keys used to verify each
  //   source's updates will be taken from this configuration. An std::invalid
  //   argument may be thrown if a time source is found in this configuration
  //   without a corresponding public key or the configuration otherwise differs
  //   from the Time Service's expectations.
  explicit TimeContract(concord::ethereum::EthKvbStorage& storage,
                        const concord::config::ConcordConfiguration& config)
      : logger_(log4cplus::Logger::getInstance("concord.time")),
        storage_(storage),
        config_(config),
        verifier_(config),
        samples_(nullptr) {}

  ~TimeContract() {
    if (samples_) {
      delete samples_;
    }
  }

  // Update the latest time reading from a given source. Any invalid update is
  // simply ignored, though the time contract may also choose to log
  // particularly suspicious updates (ignoring these updates enables the time
  // contract to automatically filter out both  updates duplicated or re-ordered
  // by the network (or attempted replay attackers) and any unconvincing
  // forgeries of updates). Arguments:
  //   - source: String identifier for the time source submitting this update.
  //   - time: Time value for this update.
  //   - signature: Signature proving this update comes from the claimed source.
  //   The signature should be a signature over the data returned by
  //   TimeContract::GetSignableUpdateData(source, time), signed with the named
  //   source's private key.
  // Returns the current time reading after making this update, which may or may
  // not have increased since before the update.
  // Throws a TimeException if this operation causes the TimeContract to load
  // state from its persistent storage but the data loaded is corrupted or
  // otherwise invalid.
  uint64_t Update(const std::string& source, uint64_t time,
                  const std::vector<uint8_t>& signature);

  // Get the current time as specified by this TimeContract based on what
  // samples it currently has. Throws a TimeException if this operation causes
  // the TimeContract to load state from its persistent storage but the data
  // loaded is corrupted or otherwise invalid.
  uint64_t GetTime();

  void StoreLatestSamples();

  // Struct containing the data stored for the latest time sample known from
  // each source. SampleBody specifically excludes the name of the source the
  // sample is for because it is intended for use in data structures and things
  // where time samples are looked up by source name.
  struct SampleBody {
    // Time for this sample.
    uint64_t time;

    // Cryptographic signature proving this sample was published by the time
    // source it is recorded for.
    std::vector<uint8_t> signature;
  };

  // Gets a const reference to the current set of time samples this time
  // contract has. The samples are returned in a map from time source names to
  // TimeContract::SampleBody structs containing the data for the latest
  // verified sample known from that source.
  // Throws a TimeException if this operation causes the TimeContract to load
  // state from its persistent storage but the data loaded is corrupted or
  // otherwise invalid.
  const std::unordered_map<std::string, SampleBody>& GetSamples();

 private:
  log4cplus::Logger logger_;
  concord::ethereum::EthKvbStorage& storage_;
  const concord::config::ConcordConfiguration& config_;
  concord::time::TimeVerifier verifier_;
  std::unordered_map<std::string, SampleBody>* samples_;

  void LoadLatestSamples();
  uint64_t SummarizeTime();
};

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_CONTRACT_HPP
