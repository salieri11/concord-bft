// Copyright 2019 VMware, all rights reserved
//
// Time Contract is a state machine run by each replica to combine timestamps
// from other replicas into a non-decreasing time that has some resilience to
// false readings.
//
// Optionally, cryptographic signatures can be used to protect against untrusted
// entities forging updates from legitimate sources and against Byzantine-faulty
// sources forging updates from other, non-faulty, sources. This optional
// feature is referred to as "time signing".
//
// One important note is that reconfiguration of a cluster after it has been
// deployed to enable or disable time signing is NOT currently supported. That
// is, if any node in a cluster has run at all before, we explicitly leave the
// behavior resulting from changing whether time signing is enabled after that
// point undefined (even if one stops every node in the cluster before making
// this change). This reconfiguration could be non-trivial because whether
// signatures are persisted to the record of time samples in the database is
// dependent on whether time signing is enabled.
//
// Readings are submitted as (source, time, signature) triples (if time signing
// is enabled) or (source, time) pairs (if time signing is disabled), where
// source is a string identifier of the submitter, time is a timestamp
// represetnted as google::protobuf::Timestamp, and signature (if present) is a
// digital signature proving that this time sample actually originates from the
// named source (based on a public key given for that time source in this
// Concord cluster's configuration).
//
// Aggregation can be done via any statistical means that give the guarantees
// discussed above. The current implementation is to choose the median of the
// most recent readings. See TimeContract::SummarizeTime.

#ifndef TIME_TIME_CONTRACT_HPP
#define TIME_TIME_CONTRACT_HPP

#include <google/protobuf/timestamp.pb.h>
#include <unordered_map>
#include <utility>
#include <vector>

#include "blockchain/db_interfaces.h"
#include "config/configuration_manager.hpp"
#include "time_exception.hpp"
#include "time_signing.hpp"

namespace concord {
namespace time {

const int64_t kTimeStorageVersion = 1;
const uint8_t kTimeKey = 0x20;

class TimeContract {
 public:
  // Constructor for TimeContract. Arguments:
  //   - storage: KVB Storage backend to persist the state of the time contract
  //   to.
  //   - config: ConcordConfiguration for this Concord cluster; note that the
  //   set of valid time sources, whether time signing is enabled, and (if time
  //   signing is enabled) the public keys used to verify each source's updates
  //   will be taken from this configuration. An std::invalid argument may be
  //   thrown if time signing is enabled and a time source is found in this
  //   configuration without a corresponding public key or if the configuration
  //   otherwise differs from the Time Service's expectations.
  explicit TimeContract(
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly&
          storage,
      const concord::config::ConcordConfiguration& config)
      : logger_(log4cplus::Logger::getInstance("concord.time")),
        storage_(storage),
        config_(config),
        verifier_(),
        samples_(nullptr),
        changed_(false),
        time_key_(new uint8_t[1]{kTimeKey}, 1) {
    if (config.hasValue<bool>("time_signing_enable") &&
        config.getValue<bool>("time_signing_enable")) {
      verifier_.reset(new concord::time::TimeVerifier(config));
    }
  }

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
  //   - signature: Pointer to a signature proving this update comes from the
  //   claimed source. If time signing is enabled, the signature should be a
  //   signature over the data returned by
  //   TimeContract::GetSignableUpdateData(source, time), signed with the named
  //   source's private key. Note this pointer will be completely ignored if
  //   time signing is not enabled; however, if time signing is enabled, the
  //   update will be ignored unless a valid signature is provided.
  // Returns the current time reading after making this update, which may or may
  // not have increased since before the update.
  // Throws a TimeException if this operation causes the TimeContract to load
  // state from its persistent storage but the data loaded is corrupted or
  // otherwise invalid.
  google::protobuf::Timestamp Update(
      const std::string& source, const google::protobuf::Timestamp& time,
      const std::vector<uint8_t>* signature = nullptr);

  // Get the current time as specified by this TimeContract based on what
  // samples it currently has. Throws a TimeException if this operation causes
  // the TimeContract to load state from its persistent storage but the data
  // loaded is corrupted or otherwise invalid.
  google::protobuf::Timestamp GetTime();

  // Has the contract been updated since being loaded or since last
  // serialization?
  bool Changed() { return changed_; }

  bool SigningEnabled() { return (bool)verifier_; }

  // Produce a key-value pair that encodes the state of the time contract for
  // KVB.
  pair<concordUtils::Sliver, concordUtils::Sliver> Serialize();

  // Clear all cached data.
  void Reset() {
    if (samples_) {
      delete samples_;
    }
    changed_ = false;
  }

  // Struct containing the data stored for the latest time sample known from
  // each source. SampleBody specifically excludes the name of the source the
  // sample is for because it is intended for use in data structures and things
  // where time samples are looked up by source name.
  struct SampleBody {
    // Time for this sample.
    google::protobuf::Timestamp time;

    // Optional pointer to a cryptographic signature. If time signing is
    // enabled, this signature should prove this sample was published by the
    // time source it is recorded for. If time signing is not enabled, this
    // pointer should point to null.
    std::unique_ptr<std::vector<uint8_t>> signature;
  };

  // Gets a const reference to the current set of time samples this time
  // contract has. The samples are returned in a map from time source names to
  // TimeContract::SampleBody structs containing the data for the latest
  // verified sample known from that source. Throws a TimeException if this
  // operation causes the TimeContract to load state from its persistent storage
  // but the data loaded is corrupted or otherwise invalid.
  const std::unordered_map<std::string, SampleBody>& GetSamples();

 private:
  log4cplus::Logger logger_;
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly& storage_;
  const concord::config::ConcordConfiguration& config_;
  std::unique_ptr<concord::time::TimeVerifier> verifier_;
  std::unordered_map<std::string, SampleBody>* samples_;
  bool changed_;
  const concordUtils::Sliver time_key_;

  void LoadLatestSamples();
  google::protobuf::Timestamp SummarizeTime();
};

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_CONTRACT_HPP
