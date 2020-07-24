#include "external_client_pool.h"

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "endianness.hpp"
#include "grpc_services.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::SkvbcRequest;

/*
 * This code works together with ExternalBftClient python class
 * (hermes/util/skvbc/concord_external_client.py) It binds to a UDP port and
 * waits for request payload. Then it serializes a ConcordRequest and passes it
 * to the external client pool for execution.
 *
 * The wrapper uses a simple UDP protocol to transfer messages:
 * | MSG FLAGS (8bits) | SEQUENCE NUMBER (64 bits) | MSG PAYLOAD (variable size)
 * |
 *
 * MSG FLAGS are handled in ReadConcordRequest() method.
 *
 * The response is either -1 (on error) or the actual response received from the
 * replica.
 *
 * This class use use for testing purposes - to execute ApolloBftTests suite
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
    struct sockaddr &src_addr, socklen_t &src_addr_size, uint8_t *recv_buf,
    const int recv_buf_size, bftEngine::ClientMsgFlag &msg_flags,
    uint64_t &seq_num) {
  ConcordRequest conc_request;
  ssize_t recv_size = 0;
  src_addr_size = sizeof(src_addr);
  recv_size = recvfrom(server_fd_, recv_buf, recv_buf_size, 0, &src_addr,
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
    ConcordRequest &conc_request) {
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

std::promise<uint8_t> ExternalClient::SetResponseCallback() {
  std::promise<uint8_t> reply_size_pr;
  EXT_DONE_CALLBACK cb = [&reply_size_pr](const uint64_t &seq_num,
                                          const std::string cid,
                                          uint8_t reply_size) -> void {
    reply_size_pr.set_value(reply_size);
  };

  // recv_buf[0] contains the msg type
  client_pool_.SetDoneCallback(cb);

  return reply_size_pr;
}

void ExternalClient::ServerLoop() {
  unsigned char resp_buf[kDefaultBufSize];
  concord::concord_client_pool::SubmitResult rq_result;

  struct sockaddr src_addr;
  socklen_t src_addr_size = 0;
  memset(&src_addr, 0, sizeof(src_addr));

  LOG_INFO(logger_, "Starting receive loop");

  uint8_t recv_buf[kDefaultBufSize];
  while (1) {
    bftEngine::ClientMsgFlag rq_flags;
    uint64_t seq_num = 0;
    ConcordRequest conc_request = ReadConcordRequest(
        src_addr, src_addr_size, recv_buf, sizeof(recv_buf), rq_flags, seq_num);

    std::string reply(kDefaultBufSize, '\0');

    // This will be send after the loop. It will point either to the actual
    // response or to generic error response
    const std::string *resp_buf = &kErrorResponse;
    com::vmware::concord::ConcordResponse response;
    for (int i = 0; i < kRetryCount; i++) {
      std::promise<uint8_t> reply_size_pr = SetResponseCallback();
      bft::client::Msg request = CreateBftClientMsg(conc_request);
      if (!request.size()) {
        LOG_ERROR(logger_, "Serialization error. Retry " << i + 1 << " of "
                                                         << kRetryCount);
        SendError(&src_addr, src_addr_size);
        continue;
      }

      rq_result = client_pool_.SendRequest(
          std::move(request), rq_flags, kDefaultRqTimeout, reply.data(),
          reply.size(), std::to_string(corr_id_++), seq_num);

      if (rq_result !=
          concord::concord_client_pool::SubmitResult::Acknowledged) {
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
        LOG_ERROR(logger_,
                  "Zero buf. Retry " << i + 1 << " of " << kRetryCount);
        continue;  // retry
      }

      response.ParseFromArray(reply.data(),
                              reply_size);  // TODO ERROR HANDLING
      resp_buf = response
                     .mutable_tee_response()  // TODO ERROR HANDLING
                     ->mutable_skvbc_response()
                     ->mutable_response_content();
      break;
    }

    LOG_INFO(logger_, "Sending reply with length "
                          << resp_buf->length() << " Data: "
                          << BufToHex(resp_buf->data(), resp_buf->length()));
    sendto(server_fd_, resp_buf->data(), resp_buf->length(), 0, &src_addr,
           src_addr_size);
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

  ServerLoop();

  return 0;
}