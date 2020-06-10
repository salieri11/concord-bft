// Copyright 2018 VMware, all rights reserved
//
// Ethereum Hashing.

#include "concord_eth_hash.hpp"

#include <keccak.h>
#include "Logger.hpp"

#include "evm.h"

namespace concord {
namespace utils {
namespace eth_hash {

evm_uint256be keccak_hash(const std::vector<uint8_t> &data) {
  return keccak_hash(&data[0], data.size());
}

evm_uint256be keccak_hash(const uint8_t *data, size_t size) {
  static_assert(sizeof(evm_uint256be) == CryptoPP::Keccak_256::DIGESTSIZE,
                "hash is not the same size as uint256");

  CryptoPP::Keccak_256 keccak;
  evm_uint256be hash;
  keccak.CalculateDigest(hash.bytes, data, size);
  return hash;
}

}  // namespace eth_hash
}  // namespace utils
}  // namespace concord
