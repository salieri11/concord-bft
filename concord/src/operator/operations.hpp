// Concord
//
// Copyright (c) 2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#pragma once

#include <chrono>
#include <optional>
#include <string>

#include "Logger.hpp"
#include "bftclient/bft_client.h"
#include "concord.pb.h"
#include "config.h"
#include "utils/openssl_crypto_utils.hpp"

namespace concord::op {
class Operations {
  /*
   * Here we will implement the actual operator actions.
   */
 public:
  Operations(const Config& config, bft::client::Client& client);
  std::optional<com::vmware::concord::ReconfigurationSmResponse>
  initiateMockCommand(std::chrono::milliseconds timeout);

 private:
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPrivateKey>
      priv_key_;
  bft::client::Client& client_;
  logging::Logger logger_ = logging::getLogger("operator.operations");
};
}  // namespace concord::op