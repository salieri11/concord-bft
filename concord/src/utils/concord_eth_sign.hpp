// Copyright 2018 VMware, all rights reserved
//
// Ethereum Signature verification.

#ifndef UTILS_CONCORD_ETH_SIGN_HPP
#define UTILS_CONCORD_ETH_SIGN_HPP

#include <secp256k1.h>
#include "Logger.hpp"

#include "evm.h"

namespace concord {
namespace utils {

class EthSign {
 public:
  EthSign();
  ~EthSign();

  std::vector<uint8_t> sign(const evm_uint256be hash,
                            const evm_uint256be key) const;

  evm_address ecrecover(const evm_uint256be hash, const uint8_t version,
                        const evm_uint256be r, const evm_uint256be s) const;

 private:
  logging::Logger logger;
  secp256k1_context *ctx;
};

}  // namespace utils
}  // namespace concord

#endif  // UTILS_CONCORD_ETH_SIGN_HPP
