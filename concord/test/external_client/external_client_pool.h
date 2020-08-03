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

#include <future>
#include <string>

#include "Logger.hpp"
#include "concord_client_pool.hpp"

namespace com::vmware::concord {
class ConcordRequest;
}
class ExternalClient {
  static constexpr std::chrono::milliseconds kDefaultRqTimeout = 10000ms;
  static constexpr int kDefaultBufSize = 64 * 1024;
  static constexpr int kRetryCount = 5;
  const std::string kErrorResponse{1, '\0'};

  const int udp_server_port_;
  unsigned int corr_id_ =
      0;  // corr_id_ always starts from 0, for ease of debugging
  concord::concord_client_pool::ConcordClientPool client_pool_;
  int server_fd_ = 0;
  logging::Logger logger_;

  void ServerLoop();
  com::vmware::concord::ConcordRequest ReadConcordRequest(
      struct sockaddr &src_addr, socklen_t &src_addr_size, uint8_t *recv_buf,
      const int recv_buf_size, bftEngine::ClientMsgFlag &msg_flags,
      uint64_t &seq_num);
  void SendError(const struct sockaddr *src_addr,
                 const socklen_t src_addr_size);
  bft::client::Msg CreateBftClientMsg(
      com::vmware::concord::ConcordRequest &conc_request);
  std::promise<uint8_t> SetResponseCallback();

 public:
  ExternalClient(std::string config_path, int internal_udp_port);
  int Start();
};