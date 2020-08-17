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

#include <string>
#include "bftclient/config.h"
#include "config/communication.hpp"
#include "config/configuration_manager.hpp"

// `operator` is a reserved keyword
namespace concord::op {

struct Config {
  static Config parse(const char* path);

  bft::client::ClientConfig client_config;
  concord::config::CommConfig comm_config;
};

};  // namespace concord::op
