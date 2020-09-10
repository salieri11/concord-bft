#include "external_client_pool.h"

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <sstream>

#include "endianness.hpp"
#include "grpc_services.hpp"
#include "thread_pool.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::SkvbcRequest;

/*
 * This code works together with ExternalBftClient python class
 * (hermes/util/skvbc/concord_external_client.py).
 *
 * Server's main thread binds to a UDP port and waits for request payload.
 * It process requests concurrently, using a ThreadPool (see
 * concord-bft/util/include/thread_pool.hpp), by passing each request to get
 * processed by a worker thread. After passing the request to the thread pool,
 * main thread goes back to wait for requests.
 *
 * Each worker starts at WorkerHandleRequest. It serializes a ConcordRequest and
 * passes it to the external client pool for execution. The wrapper uses a
 * simple UDP protocol to transfer messages: | MSG FLAGS (8bits) | SEQUENCE
 * NUMBER (64 bits) | MSG PAYLOAD (variable size) MSG FLAGS are handled by
 * worker thread in ReadConcordRequest() method. The response is sent back by
 * the worker thread, and is either -1 (on error) or the actual response
 * received from the replica.
 *
 * Concurrency is handled by using a thread-safe unordered map cid_to_pr_map_,
 * which map corr_id to a promise.A callback ResponseCallback is set into client
 * pool when server starts.
 * Each client registers itself to the callback by inserting corr_id ->
 * promise pair into the map. At callback RegisterResponseCallback, the right
 * promise is fetched from map according to the given corr_id and the promise
 * value is set to invoke the waiting thread.
 *
 * This class is used for testing purposes - to execute ApolloBftTests suite
 * from Hermes.
 */

ExternalClient::ExternalClient(std::string config_path, int internal_udp_port)
    : client_pool_{config_path}, udp_server_port_{internal_udp_port} {
  logging::initLogger("/concord/config-public/log4cplus.properties");
  logger_ = logging::Logger::getInstance("external_client_pool_wrapper");
}

std::string BufToHex(const uint8_t *buf, const int len) {
  std::ostringstream res;
  if (len) res << "0x";
  for (int i = 0; i < len; i++) {
    res << std::hex << std::setfill('0') << std::setw(2) << (int)buf[i];
  }
  return res.str();
}

std::string BufToHex(const char *buf, const int len) {
  std::ostringstream res;
  if (len) res << "0x";
  for (int i = 0; i < len; i++) {
    res << std::hex << std::setfill('0') << std::setw(2) << (int)buf[i];
  }
  return res.str();
}

ConcordRequest ExternalClient::ReadConcordRequest(
    struct sockaddr &src_addr, socklen_t &src_addr_size,
    bftEngine::ClientMsgFlag &msg_flags, uint64_t &seq_num) {
  ConcordRequest conc_request;
  ssize_t recv_size = 0;
  uint8_t recv_buf[kDefaultBufSize]{};

  src_addr_size = sizeof(src_addr);
  recv_size = recvfrom(server_fd_, recv_buf, sizeof(recv_buf), 0, &src_addr,
                       &src_addr_size);
  LOG_INFO(logger_, "Got new request. Assigning corr id: "
                        << corr_id_ << " Size: " << recv_size
                        << " Data: " << BufToHex(recv_buf, recv_size));

  if (recv_size < 1 + 8) {  // 1 byte flags, 8 bytes seq num
    // message is too short
    LOG_ERROR(logger_,
              "Message is too short. Expected message size to be > 8.");
    std::exit(195);
  }

  msg_flags = static_cast<bftEngine::ClientMsgFlag>(recv_buf[0]);
  seq_num = concordUtils::fromBigEndianBuffer<uint64_t>(&recv_buf[1]);
  LOG_DEBUG(logger_, "Details about request with corr id: "
                         << corr_id_ << " flags: " << msg_flags
                         << " seq_num: " << seq_num);

  SkvbcRequest *skvbc_request =
      conc_request.mutable_tee_request()->mutable_skvbc_request();
  skvbc_request->set_request_content(
      recv_buf + 1 + 8, recv_size - 1 - 8);  // 1 byte flags, 8 bytes seq num

  return conc_request;
}

bft::client::Msg ExternalClient::CreateBftClientMsg(
    const ConcordRequest &conc_request) {
  bft::client::Msg request(conc_request.ByteSizeLong());
  if (!conc_request.SerializeToArray(request.data(), request.size())) {
    LOG_ERROR(logger_, "Error serializing bft client msg");
    request.clear();
  }

  return request;
}

void ExternalClient::SendError(const struct sockaddr *src_addr,
                               const socklen_t src_addr_size) {
  sendto(server_fd_, kErrorResponse.data(), kErrorResponse.size(), 0, src_addr,
         src_addr_size);
}

void ExternalClient::ResponseCallback(const uint64_t &seq_num,
                                      const std::string cid,
                                      uint8_t reply_size) {
  std::lock_guard<std::mutex> lock(map_mutex_);
  auto iter = cid_to_pr_map_.find(std::stoul(cid));

  if (iter != cid_to_pr_map_.end()) {
    iter->second.set_value(reply_size);
    cid_to_pr_map_.erase(iter);
  } else {
    std::ostringstream ss;
    ss << "cid " << std::stoul(cid) << " is not found in cid_to_pr_map_!";
    throw std::out_of_range(ss.str());
  }
}

std::promise<uint8_t> ExternalClient::RegisterResponseCallback(
    unsigned int corr_id) {
  std::promise<uint8_t> reply_size_pr;
  std::lock_guard<std::mutex> lock(map_mutex_);

  cid_to_pr_map_.emplace(corr_id, reply_size_pr);

  return reply_size_pr;
}

auto ExternalClient::WorkerHandleRequest(unsigned int corr_id,
                                         const ConcordRequest &&conc_request,
                                         struct sockaddr src_addr,
                                         socklen_t src_addr_size,
                                         bftEngine::ClientMsgFlag rq_flags,
                                         uint64_t seq_num) {
  concord::concord_client_pool::SubmitResult rq_result;
  com::vmware::concord::ConcordResponse response;
  std::string reply(kDefaultBufSize, '\0');
  // point to an error response by default
  const std::string *resp_buf{&kErrorResponse};

  for (int i = 0; i < kRetryCount; i++) {
    std::promise<uint8_t> reply_size_pr = RegisterResponseCallback(corr_id);
    bft::client::Msg request = CreateBftClientMsg(conc_request);

    if (!request.size()) {
      LOG_ERROR(logger_, "Serialization error, sending error "
                             << i + 1 << " of " << kRetryCount);
      // no need to retry - we have some severe error
      break;
    }

    rq_result = client_pool_.SendRequest(
        std::move(request), rq_flags, kDefaultRqTimeout, reply.data(),
        reply.size(), seq_num, std::to_string(corr_id));

    if (rq_result != concord::concord_client_pool::SubmitResult::Acknowledged) {
      LOG_ERROR(logger_, "Request not acknowledged. Retry " << i + 1 << " of "
                                                            << kRetryCount);
      continue;  // retry
    }

    auto reply_size_fut = reply_size_pr.get_future();
    if (reply_size_fut.wait_for(kDefaultRqTimeout * 1.5) ==
        std::future_status::timeout) {
      LOG_ERROR(logger_, "Timeout. Retry " << i + 1 << " of " << kRetryCount);
      continue;  // retry
    }

    uint8_t reply_size = reply_size_fut.get();
    if (reply_size == 0) {
      LOG_ERROR(logger_, "Zero buf. Retry " << i + 1 << " of " << kRetryCount);
      continue;  // retry
    }

    if (!response.ParseFromArray(reply.data(), reply_size)) {
      LOG_ERROR(logger_, "Bad reply. Retry " << i + 1 << " of " << kRetryCount);
      continue;  // retry
    }

    // response recvied and processed - set resp_buf to actual response
    resp_buf = response.mutable_tee_response()
                   ->mutable_skvbc_response()
                   ->mutable_response_content();
    assert(resp_buf);

    break;
  }

  LOG_INFO(logger_, "Sending reply with length "
                        << resp_buf->length() << " Data: "
                        << BufToHex(resp_buf->data(), resp_buf->length()));
  sendto(server_fd_, resp_buf->data(), resp_buf->length(), 0, &src_addr,
         src_addr_size);
}

void ExternalClient::ServerLoop() {
  concord::util::ThreadPool thread_pool{};
  bftEngine::ClientMsgFlag rq_flags;
  uint64_t seq_num;
  struct sockaddr src_addr;
  socklen_t src_addr_size;

  LOG_INFO(logger_, "Starting receive loop (requests handled concurrently)");
  memset(&src_addr, 0, sizeof(src_addr));
  while (1) {
    auto conc_request =
        ReadConcordRequest(src_addr, src_addr_size, rq_flags, seq_num);
    thread_pool.async([=]() {
      return this->WorkerHandleRequest(corr_id_++, std::move(conc_request),
                                       src_addr, src_addr_size, rq_flags,
                                       seq_num);
    });
  }
}

int ExternalClient::Start() {
  if ((server_fd_ = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
    perror("socket");
    return 1;
  }

  int optval = 1;
  setsockopt(server_fd_, SOL_SOCKET, SO_REUSEADDR, (const void *)&optval,
             sizeof(int));

  struct sockaddr_in bind_addr;
  memset(&bind_addr, 0, sizeof(struct sockaddr_in));
  bind_addr.sin_family = AF_INET;
  bind_addr.sin_port = htons(udp_server_port_);
  bind_addr.sin_addr.s_addr = INADDR_ANY;

  if (bind(server_fd_, (struct sockaddr *)&bind_addr, sizeof(bind_addr)) ==
      -1) {
    perror("bind");
    return 1;
  }

  using namespace std::placeholders;
  client_pool_.SetDoneCallback(
      std::bind(&ExternalClient::ResponseCallback, this, _1, _2, _3));
  ServerLoop();

  return 0;
}