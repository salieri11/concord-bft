// Copyright 2019-2020 VMware, all rights reserved

#include "rsa_pruning_signer.hpp"
#include "pruning_exception.hpp"
#include "pruning_serialization.hpp"

#include "config/configuration_manager.hpp"

#include <string>

namespace concord {
namespace pruning {

using namespace detail;

namespace {

std::string PrivateKey(
    const concord::config::ConcordConfiguration& node_config) {
  auto& replica_scope = node_config.subscope("replica", 0);
  return replica_scope.getValue<std::string>("private_key");
}

}  // anonymous namespace

RSAPruningSigner::RSAPruningSigner(
    const concord::config::ConcordConfiguration& node_config)
    : signer_{PrivateKey(node_config).c_str()} {}

void RSAPruningSigner::Sign(
    com::vmware::concord::LatestPrunableBlock& block) const {
  if (!block.has_replica() || !block.has_block_id()) {
    throw PruningRuntimeException{
        "RSAPruningSigner failed to sign a malformed LatestPrunable message"};
  }

  std::string ser;
  ser << block.replica() << block.block_id();

  auto signature = GetSignatureBuffer();
  size_t actual_sign_len{0};
  const auto res = signer_.sign(ser.c_str(), ser.length(), signature.data(),
                                signer_.signatureLength(), actual_sign_len);
  if (!res) {
    throw PruningRuntimeException{
        "RSAPruningSigner failed to sign a LatestPrunableBlock message"};
  } else if (actual_sign_len < signature.length()) {
    signature.resize(actual_sign_len);
  }

  block.set_signature(std::move(signature));
}

std::string RSAPruningSigner::GetSignatureBuffer() const {
  const auto sign_len = signer_.signatureLength();
  return std::string(sign_len, '\0');
}

}  // namespace pruning
}  // namespace concord
