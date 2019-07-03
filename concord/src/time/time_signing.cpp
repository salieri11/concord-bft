// Copyright 2019 VMware, all rights reserved

#include "time_signing.hpp"
#include "time_contract.hpp"

using std::invalid_argument;
using std::string;
using std::unique_ptr;
using std::vector;

using bftEngine::impl::RSASigner;
using bftEngine::impl::RSAVerifier;
using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;
using concord::time::TimeContract;

namespace concord {
namespace time {

TimeSigner::TimeSigner(const ConcordConfiguration& node_config) {
  if (!node_config.hasValue<string>("time_source_id")) {
    throw invalid_argument(
        "Cannot construct TimeSigner for given node configuration: "
        "time_source_id not found.");
  }

  ConfigurationPath private_key_path("replica", size_t{0});
  private_key_path.subpath.reset(new ConfigurationPath("private_key"));
  if (!node_config.hasValue<string>(private_key_path)) {
    throw invalid_argument(
        "Cannot construct TimeSigner for given node configuration: private_key "
        "not found for this time source.");
  }

  sourceID_ = node_config.getValue<string>("time_source_id");
  private_key_ = node_config.getValue<string>(private_key_path);
  signer_.reset(new RSASigner(private_key_.c_str()));
}

TimeSigner::TimeSigner(const TimeSigner& original)
    : sourceID_(original.sourceID_),
      signer_(new RSASigner(original.private_key_.c_str())),
      private_key_(original.private_key_.c_str()) {}

TimeSigner::~TimeSigner() {}

TimeSigner& TimeSigner::operator=(const TimeSigner& original) {
  sourceID_ = original.sourceID_;
  signer_.reset(new RSASigner(original.private_key_.c_str()));
  private_key_ = original.private_key_;
  return *this;
}

vector<uint8_t> TimeSigner::Sign(uint64_t time) const {
  vector<uint8_t> data_to_sign =
      TimeContract::GetSignableUpdateData(sourceID_, time);
  vector<uint8_t> signature(signer_->signatureLength(), 0);
  size_t signature_size = 0;
  bool sig_made = signer_->sign(
      reinterpret_cast<const char*>(data_to_sign.data()), data_to_sign.size(),
      reinterpret_cast<char*>(signature.data()), signature.size(),
      signature_size);

  if (!sig_made) {
    throw TimeException(
        "TimeSigner unexpectedly failed to sign a time update for source \"" +
        sourceID_ + "\".");
  } else if (signature_size < signature.size()) {
    signature.resize(signature_size);
  }
  return signature;
}

TimeVerifier::TimeVerifier(const ConcordConfiguration& config)
    : verifiers_(), public_keys_() {
  if (!config.scopeIsInstantiated("node")) {
    throw invalid_argument(
        "Cannot construct TimeVerifier for given configuration: cannot find "
        "instantiated node scope.");
  }
  for (size_t i = 0; i < config.scopeSize("node"); ++i) {
    const ConcordConfiguration& node_config = config.subscope("node", i);
    if (node_config.hasValue<string>("time_source_id")) {
      string source_id = node_config.getValue<string>("time_source_id");
      ConfigurationPath public_key_path("replica", size_t{0});
      public_key_path.subpath.reset(new ConfigurationPath("public_key"));
      if (!node_config.hasValue<string>(public_key_path)) {
        throw invalid_argument(
            "Cannot construct TimeVerifier for given configuration: cannot "
            "find public key for time source \"" +
            source_id + "\"");
      }

      std::string public_key = node_config.getValue<string>(public_key_path);

      verifiers_.emplace(source_id, unique_ptr<RSAVerifier>(
                                        new RSAVerifier(public_key.c_str())));
      public_keys_.emplace(source_id, public_key);
    }
  }
}

TimeVerifier::TimeVerifier(const TimeVerifier& original)
    : verifiers_(), public_keys_() {
  for (auto time_source : original.public_keys_) {
    verifiers_.emplace(
        time_source.first,
        unique_ptr<RSAVerifier>(new RSAVerifier(time_source.second.c_str())));
    public_keys_.emplace(time_source.first, time_source.second);
  }
}

TimeVerifier::~TimeVerifier() {}

TimeVerifier& TimeVerifier::operator=(const TimeVerifier& original) {
  verifiers_.clear();
  public_keys_.clear();
  for (auto time_source : original.public_keys_) {
    verifiers_.emplace(
        time_source.first,
        unique_ptr<RSAVerifier>(new RSAVerifier(time_source.second.c_str())));
    public_keys_.emplace(time_source.first, time_source.second);
  }
  return *this;
}

bool TimeVerifier::HasTimeSource(const string& source) const {
  return (verifiers_.count(source) > 0);
}

bool TimeVerifier::Verify(const string& source, uint64_t time,
                          const vector<uint8_t>& signature) {
  if (verifiers_.count(source) < 1) {
    return false;
  }

  vector<uint8_t> expected_signed_data =
      TimeContract::GetSignableUpdateData(source, time);
  return verifiers_.at(source)->verify(
      reinterpret_cast<char*>(expected_signed_data.data()),
      expected_signed_data.size(),
      reinterpret_cast<const char*>(signature.data()), signature.size());
}

}  // namespace time
}  // namespace concord
