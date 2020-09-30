// Copyright 2020 VMware, all rights reserved

#include "trs_connection.hpp"

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>
#include "thin_replica.grpc.pb.h"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
using com::vmware::concord::thin_replica::ThinReplica;

using grpc::Channel;
using grpc::ChannelArguments;
using grpc::InsecureChannelCredentials;
using grpc::SslCredentialsOptions;
using grpc_connectivity_state::GRPC_CHANNEL_READY;

using namespace std::chrono_literals;

namespace thin_replica_client {

// Daml ledger api container is loaded with the environment variable
// THIN_REPLICA_SETTINGS. If the value of the key `trc_tls_enable` set in
// THIN_REPLICA_SETTINGS is true, TLS enabled secure channel will be opened
// by the thin replica client, else an insecure channel will be employed.
// The value of the key `thin_replica_tls_cert_path` in THIN_REPLICA_SETTINGS
// specifies the path of the certificates used for the TLS channel.
const static std::string kThinReplicaSettings = "THIN_REPLICA_SETTINGS";
const static std::string kTrcTlsEnable = "trc_tls_enable";
const static std::string kThinReplicaTlsCertPath = "thin_replica_tls_cert_path";

void TrsConnection::createStub() {
  assert(channel_);
  stub_ = ThinReplica::NewStub(channel_);
}

void TrsConnection::createChannel() {
  grpc::ChannelArguments args;
  args.SetMaxReceiveMessageSize(kGrpcMaxInboundMsgSizeInBytes);

  auto tr_env = std::getenv(kThinReplicaSettings.c_str());

  std::string trc_tls_enable_val, thin_replica_tls_cert_path;
  if (tr_env != NULL) {
    // Check if TLS has been enabled in THIN_REPLICA_SETTINGS environment
    // variable
    trc_tls_enable_val = parseThinReplicaEnv(tr_env, kTrcTlsEnable);

    // Transform the tls_enable_val to lowercase for comparison
    std::transform(trc_tls_enable_val.begin(), trc_tls_enable_val.end(),
                   trc_tls_enable_val.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    // Get TLS certificate paths set in THIN_REPLICA_SETTINGS environment
    // variable
    thin_replica_tls_cert_path =
        parseThinReplicaEnv(tr_env, kThinReplicaTlsCertPath);
  } else {
    trc_tls_enable_val = "false";
  }

  if (trc_tls_enable_val == "true") {
    LOG4CPLUS_INFO(
        logger_, "TLS for thin replica client is enabled, certificate path: "
                     << thin_replica_tls_cert_path << ", server: " << address_);

    std::string client_cert, client_key, root_cert;
    // Use the last char in client_id_, i.e., the numeric identifier to uniquely
    // identify the certs folder for the client. For e.g., if client_id is
    // daml_ledger_api1, c0 is the client certs folder Similarly, use the last
    // char in server name part of the address to uniquely identify the server
    // cert folder. For e.g., if server name is concord1, s0 is the server certs
    // folder
    std::string client_cert_path =
        thin_replica_tls_cert_path + "/c" +
        std::to_string((client_id_.back() - '0') - 1);
    std::string server_cert_path =
        thin_replica_tls_cert_path + "/s" +
        std::to_string(((address_.substr(0, address_.find(":"))).back() - '0') -
                       1);

    readCert(client_cert_path + "/client.cert", client_cert);
    readCert(client_cert_path + "/pk.pem", client_key);
    readCert(server_cert_path + "/server.cert", root_cert);

    grpc::SslCredentialsOptions opts = {root_cert, client_key, client_cert};
    channel_ =
        grpc::CreateCustomChannel(address_, grpc::SslCredentials(opts), args);
  } else {
    LOG4CPLUS_WARN(logger_,
                   "TLS for thin replica client is disabled, falling back to "
                   "insecure channel");
    channel_ = grpc::CreateCustomChannel(
        address_, grpc::InsecureChannelCredentials(), args);
  }
}

void TrsConnection::connect() {
  if (!channel_) {
    createChannel();
    createStub();
  } else if (!stub_) {
    createStub();
  }
  // Initiate connection
  channel_->GetState(true);
}

bool TrsConnection::isConnected() {
  if (!channel_) {
    return false;
  }
  auto status = channel_->GetState(false);
  LOG4CPLUS_DEBUG(logger_,
                  "gRPC connection status (" << address_ << ") " << status);
  return status == GRPC_CHANNEL_READY;
}

void TrsConnection::disconnect() {
  closeStateStream();
  closeDataStream();
  closeHashStream();
  stub_.reset();
  channel_.reset();
}

void TrsConnection::openDataStream(const SubscriptionRequest& request) {
  assert(stub_);
  assert(!data_stream_);

  data_context_.reset(new grpc::ClientContext());
  data_context_->AddMetadata("client_id", client_id_);

  data_stream_ = stub_->SubscribeToUpdates(data_context_.get(), request);
}

void TrsConnection::closeDataStream() {
  if (!data_stream_) {
    return;
  }
  assert(data_context_);
  data_context_->TryCancel();
  data_context_.reset();
  data_stream_.reset();
}

bool TrsConnection::hasDataStream() { return bool(data_stream_); }

bool TrsConnection::readData(Data* data) {
  assert(data_stream_);
  return data_stream_->Read(data);
}

void TrsConnection::openStateStream(const ReadStateRequest& request) {
  assert(stub_);
  assert(!state_stream_);

  state_context_.reset(new grpc::ClientContext());
  state_context_->AddMetadata("client_id", client_id_);

  state_stream_ = stub_->ReadState(state_context_.get(), request);
}

bool TrsConnection::closeStateStream() {
  if (!state_stream_) {
    return true;
  }
  assert(state_context_);
  // "state" is not an infite data stream and we expect proper termination
  auto status = state_stream_->Finish();
  if (!status.ok()) {
    LOG4CPLUS_WARN(logger_, "Finishing ReadState from "
                                << address_ << " failed with error code: "
                                << status.error_code() << ", \""
                                << status.error_message() << "\").");
  }
  state_context_.reset();
  state_stream_.reset();
  return status.ok();
}

bool TrsConnection::hasStateStream() { return bool(state_stream_); }

bool TrsConnection::readState(Data* data) {
  assert(state_stream_);
  return state_stream_->Read(data);
}

bool TrsConnection::readStateHash(const ReadStateHashRequest& request,
                                  Hash* hash) {
  grpc::ClientContext hash_context;
  hash_context.AddMetadata("client_id", client_id_);
  auto status = stub_->ReadStateHash(&hash_context, request, hash);
  if (!status.ok()) {
    LOG4CPLUS_WARN(logger_, "ReadStateHash from "
                                << address_ << " failed with error code: "
                                << status.error_code() << ", \""
                                << status.error_message() << "\").");
  }
  return status.ok();
}

void TrsConnection::openHashStream(SubscriptionRequest& request) {
  assert(stub_);
  assert(!hash_stream_);

  hash_context_.reset(new grpc::ClientContext());
  hash_context_->AddMetadata("client_id", client_id_);

  hash_stream_ = stub_->SubscribeToUpdateHashes(hash_context_.get(), request);
}

void TrsConnection::closeHashStream() {
  if (!hash_stream_) {
    return;
  }
  assert(hash_context_);
  hash_context_->TryCancel();
  hash_context_.reset();
  hash_stream_.reset();
}

bool TrsConnection::hasHashStream() { return bool(hash_stream_); }

bool TrsConnection::readHash(Hash* hash) {
  assert(hash_stream_);
  return hash_stream_->Read(hash);
}

void TrsConnection::readCert(const std::string& input_filename,
                             std::string& out_data) {
  std::ifstream input_file(input_filename.c_str(), std::ios::in);

  if (!input_file.is_open()) {
    LOG4CPLUS_FATAL(logger_, "Failed to construct thin replica client.");
    throw std::runtime_error(
        __PRETTY_FUNCTION__ + std::string(": Could not open the input file (") +
        input_filename +
        std::string(
            ") to establish TLS connection with the thin replica server."));
  } else {
    try {
      std::stringstream read_buffer;
      read_buffer << input_file.rdbuf();
      input_file.close();
      out_data = read_buffer.str();
      LOG4CPLUS_INFO(logger_,
                     "Successfully loaded the contents of " + input_filename);
    } catch (std::exception& e) {
      LOG4CPLUS_FATAL(logger_, "Failed to construct thin replica client.");
      throw std::runtime_error(
          __PRETTY_FUNCTION__ +
          std::string(
              ": An exception occurred while trying to read the input file (") +
          input_filename + std::string("): ") + std::string(e.what()));
    }
  }
  return;
}

std::string TrsConnection::parseThinReplicaEnv(const std::string& env,
                                               const std::string& key) {
  size_t key_pos = env.find(key);

  std::string val;

  if (key_pos != std::string::npos) {
    int val_start_pos = key_pos + key.length() + 1;
    // If the key is not the last key in the environment variable string, parse
    // until the next space character seen, else, parse until end of line
    if (env.find(" ", key_pos) != std::string::npos) {
      int val_len = env.find(" ", key_pos) - val_start_pos;
      val = env.substr(val_start_pos, val_len);
    } else {
      val = env.substr(val_start_pos);
    }
  }
  return val;
}

}  // namespace thin_replica_client
