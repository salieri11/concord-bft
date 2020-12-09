// Copyright 2020 VMware, all rights reserved

#ifndef THIN_REPLICA_IMPL_HPP_
#define THIN_REPLICA_IMPL_HPP_

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/status.h>
#include <openssl/bio.h>
#include <openssl/pem.h>
#include <openssl/x509.h>
#include <opentracing/tracer.h>
#include <storage/kvb_key_types.h>
#include <chrono>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utils/concord_prometheus_metrics.hpp>
#include <utils/open_tracing_utils.hpp>
#include "Logger.hpp"

#include "db_interfaces.h"
#include "kv_types.hpp"
#include "storage/kvb_app_filter.h"

#include "thin_replica.grpc.pb.h"
#include "thin_replica/subscription_buffer.hpp"

namespace concord {
namespace thin_replica {

class ThinReplicaImpl {
 private:
  class StreamClosed : public std::runtime_error {
   public:
    StreamClosed(const std::string& msg) : std::runtime_error(msg){};
  };
  class StreamCancelled : public std::runtime_error {
   public:
    StreamCancelled(const std::string& msg) : std::runtime_error(msg){};
  };

  using KvbAppFilterPtr = std::shared_ptr<storage::KvbAppFilter>;
  static constexpr size_t kSubUpdateBufferSize{1000u};

 public:
  ThinReplicaImpl(
      const bool is_insecure_trs, const std::string& tls_trs_cert_path,
      const concord::kvbc::ILocalKeyValueStorageReadOnly* rostorage,
      SubBufferList& subscriber_list,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
      : logger_(logging::getLogger("concord.thin_replica")),
        is_insecure_trs_(is_insecure_trs),
        tls_trs_cert_path_(tls_trs_cert_path),
        rostorage_(rostorage),
        subscriber_list_(subscriber_list),
        prometheus_registry_(prometheus_registry),
        metric_stats_(prometheus_registry_->createGaugeFamily(
            "concord_trs_stats",
            "Count stats available to the TRS for multiple clients "
            "differentiated by the label",
            {{"layer", "ThinReplicaServer"}})) {}

  ThinReplicaImpl(const ThinReplicaImpl&) = delete;
  ThinReplicaImpl(ThinReplicaImpl&&) = delete;
  ThinReplicaImpl& operator=(const ThinReplicaImpl&) = delete;
  ThinReplicaImpl& operator=(ThinReplicaImpl&&) = delete;

  template <typename ServerContextT, typename ServerWriterT>
  grpc::Status ReadState(
      ServerContextT* context,
      const com::vmware::concord::thin_replica::ReadStateRequest* request,
      ServerWriterT* stream) {
    auto [status, kvb_filter] = CreateKvbFilter(
        context, request, true /* Keep correlation id while filtering */);
    if (!status.ok()) {
      return status;
    }

    LOG_DEBUG(logger_, "ReadState");

    // TODO: Determine oldest block available (pruning)
    kvbc::BlockId start = 1;
    kvbc::BlockId end = rostorage_->getLastBlock();

    if (end == 0) {
      std::string msg{"No blocks available"};
      LOG_WARN(logger_, msg);
      return grpc::Status(grpc::StatusCode::FAILED_PRECONDITION, msg);
    }

    try {
      ReadFromKvbAndSendData(logger_, context, stream, start, end, kvb_filter);
    } catch (StreamCancelled& error) {
      return grpc::Status(grpc::StatusCode::CANCELLED, error.what());
    } catch (std::exception& error) {
      LOG_ERROR(logger_, "Failed to read and send state: " << error.what());
      return grpc::Status(grpc::StatusCode::UNKNOWN,
                          "Failed to read and send state");
    }
    return grpc::Status::OK;
  }

  template <typename ServerContextT>
  grpc::Status ReadStateHash(
      ServerContextT* context,
      const com::vmware::concord::thin_replica::ReadStateHashRequest* request,
      com::vmware::concord::thin_replica::Hash* hash) {
    auto [status, kvb_filter] = CreateKvbFilter(
        context, request, false /* Don't keep correlation id */);
    if (!status.ok()) {
      return status;
    }

    LOG_DEBUG(logger_, "ReadStateHash");

    // TODO: Determine oldest block available (pruning)
    kvbc::BlockId block_id_start = 1;
    kvbc::BlockId block_id_end = request->block_id();

    if (block_id_end < block_id_start) {
      std::string msg{"Invalid block range"};
      msg += " [" + std::to_string(block_id_start) + "," +
             std::to_string(block_id_end) + "]";
      LOG_WARN(logger_, msg);
      return grpc::Status(grpc::StatusCode::FAILED_PRECONDITION, msg);
    }

    concord::storage::KvbStateHash kvb_hash;
    try {
      kvb_hash = kvb_filter->ReadBlockRangeHash(block_id_start, block_id_end);
    } catch (std::exception& error) {
      LOG_ERROR(logger_, error.what());
      std::stringstream msg;
      msg << "Reading StateHash for block " << block_id_end << " failed";
      return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
    }

    hash->set_block_id(block_id_end);
    hash->set_hash(&kvb_hash, sizeof kvb_hash);

    return grpc::Status::OK;
  }

  template <typename ServerContextT>
  grpc::Status AckUpdate(
      ServerContextT* context,
      const com::vmware::concord::thin_replica::BlockId* block_id,
      google::protobuf::Empty* empty) {
    return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "AckUpdate");
  }

  template <typename ServerContextT, typename ServerWriterT, typename DataT>
  grpc::Status SubscribeToUpdates(
      ServerContextT* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest* request,
      ServerWriterT* stream) {
    bool read_cid = false;
    std::string stream_type;
    // If DataT == Data we want to read the correlation is when filtering the
    // updates in order to update the correlation id in the returned Update
    // item. If DataT = Hash we won't read the correlation id while filtering in
    // order to adjust the hashes values to the updates.
    if constexpr (std::is_same<DataT,
                               com::vmware::concord::thin_replica::Data>()) {
      read_cid = true;
      stream_type = "data";
    } else if constexpr (std::is_same<
                             DataT,
                             com::vmware::concord::thin_replica::Hash>()) {
      stream_type = "hash";
    }

    auto [kvb_status, kvb_filter] = CreateKvbFilter(context, request, read_cid);
    if (!kvb_status.ok()) {
      return kvb_status;
    }

    // Setup metrics for this client connection
    assert(prometheus_registry_);
    auto& metric_queue_size = prometheus_registry_->createGauge(
        metric_stats_, {{"operation", "queue_size"},
                        {"stream", stream_type},
                        {"client", GetClientId(context)}});
    metric_queue_size.Set(0);
    auto& metric_last_sent_block_id = prometheus_registry_->createGauge(
        metric_stats_, {{"operation", "last_sent_block_id"},
                        {"stream", stream_type},
                        {"client", GetClientId(context)}});
    metric_last_sent_block_id.Set(0);

    auto [subscribe_status, live_updates] = SubscribeToLiveUpdates(request);
    if (!subscribe_status.ok()) {
      return subscribe_status;
    }

    try {
      SyncAndSend<ServerContextT, ServerWriterT, DataT>(
          context, request->block_id(), live_updates, stream, kvb_filter);
    } catch (StreamCancelled& error) {
      subscriber_list_.RemoveBuffer(live_updates);
      live_updates->RemoveAllUpdates();
      return grpc::Status(grpc::StatusCode::CANCELLED, error.what());
    } catch (std::exception& error) {
      LOG_ERROR(logger_, error.what());
      subscriber_list_.RemoveBuffer(live_updates);
      live_updates->RemoveAllUpdates();

      std::stringstream msg;
      msg << "Couldn't transition from block id " << request->block_id()
          << " to new blocks";
      return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
    }

    // Read, filter, and send live updates
    std::string correlation_id;
    SubUpdate update;
    while (!context->IsCancelled()) {
      metric_queue_size.Set(live_updates->Size());
      try {
        live_updates->Pop(update);
      } catch (concord::thin_replica::ConsumerTooSlow& error) {
        LOG_WARN(logger_, "Closing subscription: " << error.what());
        break;
      }
      auto filtered_update = kvb_filter->FilterUpdate(update);
      auto& kv = filtered_update.second;
      correlation_id = ExtractCid(kv);
      auto span = concord::utils::ExtractSpan(kv, "thin_replica_server",
                                              logger_, correlation_id);
      try {
        if constexpr (std::is_same<
                          DataT, com::vmware::concord::thin_replica::Data>()) {
          SendData(stream, filtered_update, correlation_id, span);
        } else if constexpr (std::is_same<
                                 DataT,
                                 com::vmware::concord::thin_replica::Hash>()) {
          SendHash(stream, update.first,
                   kvb_filter->HashUpdate(filtered_update));
        }
      } catch (std::exception& error) {
        LOG_INFO(logger_, "Subscription stream closed: " << error.what());
        break;
      }
      metric_last_sent_block_id.Set(update.first);
    }

    subscriber_list_.RemoveBuffer(live_updates);
    live_updates->RemoveAllUpdates();
    if (context->IsCancelled()) {
      return grpc::Status::CANCELLED;
    }
    return grpc::Status::OK;
  }

  template <typename ServerContextT>
  grpc::Status Unsubscribe(ServerContextT* context,
                           const google::protobuf::Empty* request,
                           google::protobuf::Empty* response) {
    // Note: In order to unsubscribe in a separate gRPC call, we need to connect
    // the sub buffer with the thin replica client id.
    return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "Unsubscribe");
  }

  // Parses the value of the OU field i.e., the client id from the subject
  // string
  std::string ParseClientIdFromSubject(const std::string& subject_str) {
    std::string delim = "OU = ";
    size_t start = subject_str.find(delim) + delim.length();
    size_t end = subject_str.find(",", start);
    std::string raw_str = subject_str.substr(start, end - start);
    size_t fstart = 0;
    size_t fend = raw_str.length();
    // remove surrounding whitespaces and newlines
    if (raw_str.find_first_not_of(" ") != std::string::npos)
      fstart = raw_str.find_first_not_of(" ");
    if (raw_str.find_last_not_of(" ") != std::string::npos)
      fend = raw_str.find_last_not_of(" ");
    raw_str.erase(std::remove(raw_str.begin(), raw_str.end(), '\n'),
                  raw_str.end());
    return raw_str.substr(fstart, fend - fstart + 1);
  }

  template <typename ServerContextT>
  std::string GetClientIdFromClientCert(ServerContextT* context) {
    std::string client_id;
    // get certificate from the client
    for (auto& temp : context->auth_context()->FindPropertyValues(
             GRPC_X509_PEM_CERT_PROPERTY_NAME)) {
      std::string cert_str = temp.data();
      const char* data = (const char*)cert_str.c_str();

      // encode the certificate received in string format to x509 certificate
      BIO* bio;
      X509* certificate;

      // A BIO is an I/O stream abstraction used by openssl
      bio = BIO_new(BIO_s_mem());  // returns a new BIO
      if (!bio) {
        throw std::runtime_error(
            "Failed to read certificate received from client for client "
            "authorization - BIO_new() failed!");
      }
      // attempts to write a null terminated string to BIO
      BIO_puts(bio, data);

      // read a certificate in PEM format from a BIO
      certificate = PEM_read_bio_X509(bio, NULL, NULL, NULL);
      if (!certificate) {
        throw std::runtime_error(
            "Failed to encode certificate received from client for client "
            "authorization - PEM_read_bio_X509() failed!");
      }

      // get the subject from the certificate
      char* subj =
          X509_NAME_oneline(X509_get_subject_name(certificate), NULL, 0);
      std::string result(subj);

      // parse the OU field i.e., the client_id from the certificate
      std::string delim = "OU=";
      size_t start = result.find(delim) + delim.length();
      size_t end = result.find("/", start);
      client_id = result.substr(start, end - start);
      return client_id;
    }
    return client_id;
  }

  void GetClientIdFromRootCert(const std::string& root_cert_path,
                               std::unordered_set<std::string>& common_names) {
    std::array<char, 128> buffer;
    std::string result;
    // Openssl doesn't provide a method to fetch all the x509 certificates
    // directly from a bundled cert, due to the assumption of one certificate
    // per file. But for some reason openssl supports displaying multiple certs
    // from a pkcs7 file. So we generate an intermediate pkcs7 file using
    // crl2pkcs7 openssl command to get the subject fields of all the certs from
    // the bundled root cert.
    std::string cmd = "openssl crl2pkcs7 -nocrl -certfile " + root_cert_path +
                      " | openssl pkcs7 -print_certs -noout | grep .";
    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd.c_str(), "r"),
                                                  pclose);
    if (!pipe) {
      throw std::runtime_error(
          "Failed to read subject fields from root cert - popen() failed!");
    }
    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
      result = buffer.data();
      // parse the common name i.e., the client id from the subject field
      common_names.insert(ParseClientIdFromSubject(result));
    }
  }

 private:
  template <typename ServerContextT, typename ServerWriterT>
  void ReadFromKvbAndSendData(
      logging::Logger logger, ServerContextT* context, ServerWriterT* stream,
      kvbc::BlockId start, kvbc::BlockId end,
      std::shared_ptr<storage::KvbAppFilter> kvb_filter) {
    using namespace std::chrono_literals;
    boost::lockfree::spsc_queue<storage::KvbUpdate> queue{10};
    std::atomic_bool close_stream = false;

    auto kvb_reader = std::async(
        std::launch::async, &storage::KvbAppFilter::ReadBlockRange, kvb_filter,
        start, end, std::ref(queue), std::ref(close_stream));

    storage::KvbUpdate kvb_update;
    std::string correlation_id;
    while (kvb_reader.wait_for(0s) != std::future_status::ready ||
           !queue.empty()) {
      if (context->IsCancelled()) {
        throw StreamCancelled("Kvb data stream cancelled");
      }
      while (queue.pop(kvb_update)) {
        try {
          auto& kv = kvb_update.second;
          correlation_id = ExtractCid(kv);
          SendData(stream, kvb_update, correlation_id);
        } catch (StreamClosed& error) {
          LOG_WARN(logger, "Data stream closed at block " << kvb_update.first);

          // Stop kvb_reader and empty queue
          close_stream = true;
          while (!queue.empty()) {
            queue.pop(kvb_update);
          }
          throw;
        }
      }
    }
    assert(queue.empty());

    // Throws exception if something goes wrong
    kvb_reader.get();
  }

  template <typename ServerContextT, typename ServerWriterT>
  void ReadFromKvbAndSendHashes(
      logging::Logger logger, ServerContextT* context, ServerWriterT* stream,
      kvbc::BlockId start, kvbc::BlockId end,
      std::shared_ptr<storage::KvbAppFilter> kvb_filter) {
    for (auto block_id = start; block_id <= end; ++block_id) {
      if (context->IsCancelled()) {
        throw StreamCancelled("Kvb hash stream cancelled");
      }
      size_t hash = kvb_filter->ReadBlockHash(block_id);
      SendHash(stream, block_id, hash);
    }
  }

  // Read from KVB and send to the given stream depending on the data type
  template <typename ServerContextT, typename ServerWriterT, typename DataT>
  inline void ReadAndSend(logging::Logger logger, ServerContextT* context,
                          ServerWriterT* stream, kvbc::BlockId start,
                          kvbc::BlockId end,
                          std::shared_ptr<storage::KvbAppFilter> kvb_filter) {
    static_assert(
        std::is_same<DataT, com::vmware::concord::thin_replica::Data>() ||
            std::is_same<DataT, com::vmware::concord::thin_replica::Hash>(),
        "We expect either a Data or Hash type");
    if constexpr (std::is_same<DataT,
                               com::vmware::concord::thin_replica::Data>()) {
      ReadFromKvbAndSendData(logger, context, stream, start, end, kvb_filter);
    } else if constexpr (std::is_same<
                             DataT,
                             com::vmware::concord::thin_replica::Hash>()) {
      ReadFromKvbAndSendHashes(logger, context, stream, start, end, kvb_filter);
    }
  }

  // Read from KVB until we are in sync with the live updates. This function
  // returns when the next update can be taken from the given live updates.
  // We assume that the Commands handler is already filling the queue. Also, we
  // don't care if the queue fills up. In that case, the caller won't be able to
  // use the queue as soon as he consumes it.
  template <typename ServerContextT, typename ServerWriterT, typename DataT>
  void SyncAndSend(ServerContextT* context, kvbc::BlockId start,
                   std::shared_ptr<SubUpdateBuffer> live_updates,
                   ServerWriterT* stream,
                   std::shared_ptr<storage::KvbAppFilter> kvb_filter) {
    kvbc::BlockId end = rostorage_->getLastBlock();
    assert(start <= end);

    // Let's not wait for a live update yet due to there might be lots of
    // history we have to catch up with first
    LOG_INFO(logger_, "Sync reading from KVB [" << start << ", " << end << "]");
    ReadAndSend<ServerContextT, ServerWriterT, DataT>(logger_, context, stream,
                                                      start, end, kvb_filter);

    // Let's wait until we have at least one live update
    live_updates->WaitUntilNonEmpty();

    // We are in sync already
    if (live_updates->OldestBlockId() == (end + 1)) {
      return;
    }

    // Gap:
    // If the first live update is not the follow-up to the last read block from
    // KVB then we need to fill the gap. Let's read from KVB starting at end + 1
    // up to updates that are part of the live updates already. Thereby, we
    // create an overlap between what we read from KVB and what is currently in
    // the live updates.
    if (live_updates->OldestBlockId() > (end + 1)) {
      start = end + 1;
      end = live_updates->NewestBlockId();

      LOG_INFO(logger_, "Sync filling gap [" << start << ", " << end << "]");
      ReadAndSend<ServerContextT, ServerWriterT, DataT>(
          logger_, context, stream, start, end, kvb_filter);
    }

    // Overlap:
    // If we read updates from KVB that were added to the live updates already
    // then we just need to drop the overlap and return
    assert(live_updates->OldestBlockId() <= end);
    SubUpdate update;
    do {
      live_updates->Pop(update);
      LOG_INFO(logger_, "Sync dropping " << update.first);
    } while (update.first < end);
  }

  // Send* prepares the response object and puts it on the stream
  template <typename ServerWriterT>
  void SendData(ServerWriterT* stream,
                const concord::thin_replica::SubUpdate& update,
                const std::string& correlation_id,
                const SpanPtr& span = nullptr) {
    com::vmware::concord::thin_replica::Data data;
    data.set_block_id(update.first);

    for (const auto& [key, value] : update.second) {
      com::vmware::concord::thin_replica::KVPair* kvp_out = data.add_data();
      kvp_out->set_key(key.data(), key.length());
      kvp_out->set_value(value.data(), value.length());
    }
    data.set_correlation_id(correlation_id);
    if (span) {
      std::ostringstream context;
      span->tracer().Inject(span->context(), context);
      data.set_span_context(context.str());
    }
    if (!stream->Write(data)) {
      throw StreamClosed("Data stream closed");
    }
  }

  template <typename ServerWriterT>
  void SendHash(ServerWriterT* stream, kvbc::BlockId block_id,
                size_t update_hash) {
    com::vmware::concord::thin_replica::Hash hash;
    hash.set_block_id(block_id);
    hash.set_hash(&update_hash, sizeof update_hash);
    if (!stream->Write(hash)) {
      throw StreamClosed("Hash stream closed");
    }
  }

  // Get client_id from metadata if using insecure TRS
  // Get client_id from the client certs if using secure TRS
  // and compare the client_id with the client_id in known root cert
  template <typename ServerContextT>
  std::string GetClientId(ServerContextT* context) {
    if (is_insecure_trs_) {
      auto metadata = context->client_metadata();
      auto client_id = metadata.find("client_id");
      if (client_id != metadata.end()) {
        return std::string(client_id->second.data(),
                           client_id->second.length());
      }
      throw std::invalid_argument("client_id metadata is missing");
    } else {
      return getAuthorizedClientId(context);
    }
  }

  // Get client_id from the client certs if using secure TRS
  // and compare the client_id with the client_id in known root cert
  template <typename ServerContextT>
  std::string getAuthorizedClientId(ServerContextT* context) {
    if (context->auth_context() &&
        context->auth_context()->IsPeerAuthenticated()) {
      // get common names from the root cert
      std::unordered_set<std::string> client_id_set;
      std::string root_cert_path = tls_trs_cert_path_ + "/client.cert";
      GetClientIdFromRootCert(root_cert_path, client_id_set);
      std::string client_id = GetClientIdFromClientCert(context);
      if (!client_id.empty()) {
        if (client_id_set.find(client_id) != client_id_set.end()) {
          LOG_INFO(logger_, "Client " << client_id << " is authorized");
          return client_id;
        } else {
          LOG_FATAL(logger_, "Client is not authorized, given client_id "
                                 << client_id
                                 << " doesn't match any known client IDs");
          throw std::runtime_error("Client is not authorized!");
        }
      }
      throw std::invalid_argument(
          "client_id is missing in the client certificates");
    }
    throw std::runtime_error("Client is not authenticated!");
  }

  template <typename ServerContextT, typename RequestT>
  std::tuple<grpc::Status, KvbAppFilterPtr> CreateKvbFilter(
      ServerContextT* context, const RequestT* request, bool keep_cid) {
    KvbAppFilterPtr kvb_filter;
    std::set<storage::KvbAppFilter::AppType> types = {
        storage::KvbAppFilter::kDaml};
    if (keep_cid) types.emplace(storage::KvbAppFilter::kCid);
    try {
      kvb_filter = std::make_shared<storage::KvbAppFilter>(
          rostorage_, types, GetClientId(context), request->key_prefix());
    } catch (std::exception& error) {
      std::stringstream msg;
      msg << "Failed to set up filter: " << error.what();
      LOG_ERROR(logger_, msg.str());
      return {grpc::Status(grpc::StatusCode::UNKNOWN, msg.str()), nullptr};
    }
    return {grpc::Status::OK, kvb_filter};
  }

  template <typename RequestT>
  std::tuple<grpc::Status, std::shared_ptr<SubUpdateBuffer>>
  SubscribeToLiveUpdates(RequestT* request) {
    auto live_updates = std::make_shared<SubUpdateBuffer>(kSubUpdateBufferSize);
    subscriber_list_.AddBuffer(live_updates);

    if (request->block_id() > rostorage_->getLastBlock()) {
      subscriber_list_.RemoveBuffer(live_updates);
      live_updates->RemoveAllUpdates();
      std::stringstream msg;
      msg << "Block " << request->block_id() << " doesn't exist yet";
      return {grpc::Status(grpc::StatusCode::FAILED_PRECONDITION, msg.str()),
              live_updates};
    }
    return {grpc::Status::OK, live_updates};
  }

 private:
  logging::Logger logger_;
  const bool is_insecure_trs_ = true;
  const std::string tls_trs_cert_path_;
  const concord::kvbc::ILocalKeyValueStorageReadOnly* rostorage_;
  SubBufferList& subscriber_list_;
  std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry_;
  prometheus::Family<prometheus::Gauge>& metric_stats_;

  const kvbc::Key cid_key_ = kvbc::Key(
      new decltype(storage::kKvbKeyCorrelationId)[1]{
          storage::kKvbKeyCorrelationId},
      1);

  std::string ExtractCid(kvbc::SetOfKeyValuePairs& kv) {
    std::string ret;
    auto pos = kv.find(cid_key_);
    if (pos != kv.end()) {
      auto& val = pos->second;
      ret = val.toString();
      kv.erase(pos);
    }
    return ret;
  }
};
}  // namespace thin_replica
}  // namespace concord

#endif /* end of include guard: THIN_REPLICA_IMPL_HPP_ */
