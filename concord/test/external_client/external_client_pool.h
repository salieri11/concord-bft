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
#include <mutex>
#include <string>
#include <unordered_map>

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
  // corr_id_ always starts from 0, for ease of debugging
  unsigned int corr_id_ = 0;
  concord::concord_client_pool::ConcordClientPool client_pool_;
  int server_fd_ = 0;
  logging::Logger logger_;
  std::mutex map_mutex_;
  std::unordered_map<unsigned int, std::promise<uint8_t> &> cid_to_pr_map_;

  void ServerLoop();
  auto WorkerHandleRequest(
      unsigned int corr_id,
      const com::vmware::concord::ConcordRequest &&conc_request,
      struct sockaddr src_addr, socklen_t src_addr_size,
      bftEngine::ClientMsgFlag rq_flags, uint64_t seq_num);
  com::vmware::concord::ConcordRequest ReadConcordRequest(
      struct sockaddr &src_addr, socklen_t &src_addr_size,
      bftEngine::ClientMsgFlag &msg_flags, uint64_t &seq_num);
  void SendError(const struct sockaddr *src_addr,
                 const socklen_t src_addr_size);
  bft::client::Msg CreateBftClientMsg(
      const com::vmware::concord::ConcordRequest &conc_request);
  std::promise<uint8_t> RegisterResponseCallback(unsigned int corr_id);
  void ResponseCallback(const uint64_t &seq_num, const std::string cid,
                        uint8_t reply_size);

 public:
  ExternalClient(std::string config_path, int internal_udp_port);
  int Start();
};