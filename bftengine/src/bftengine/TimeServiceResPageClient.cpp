// Concord
//
// Copyright (c) 2018-2021 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").  You may not use this product except in
// compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright notices and license terms. Your use of
// these subcomponents is subject to the terms and conditions of the sub-component's license, as noted in the LICENSE
// file.

#include "TimeServiceResPageClient.hpp"

namespace {
constexpr auto RESERVED_PAGE_ID = uint32_t{0};
}

namespace bftEngine {

TimeServiceResPageClient::TimeServiceResPageClient() {
  ConcordAssert(res_pages_ != nullptr &&
                "Reserved pages must be initialized before instantiating TimeServiceResPageClient");
  load();
}

void TimeServiceResPageClient::setLastTimestamp(ConsensusTime timestamp) {
  auto raw_value = timestamp.count();
  saveReservedPage(RESERVED_PAGE_ID, sizeof(ConsensusTime::rep), reinterpret_cast<char*>(&raw_value));
  last_timestamp_ = timestamp;
}

void TimeServiceResPageClient::load() {
  auto raw_value = ConsensusTime::rep{0};
  if (loadReservedPage(RESERVED_PAGE_ID, sizeOfReservedPage(), reinterpret_cast<char*>(&raw_value))) {
    last_timestamp_ = ConsensusTime{raw_value};
  } else {
    setLastTimestamp(last_timestamp_);
  }
}
}  // namespace bftEngine
