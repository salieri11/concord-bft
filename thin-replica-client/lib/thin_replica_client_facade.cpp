// Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential

#include "thin_replica_client_facade.hpp"
#include <log4cplus/configurator.h>
#include "thin_replica_client.hpp"

using grpc::Channel;
using grpc::ChannelArguments;
using grpc::InsecureChannelCredentials;
using log4cplus::Logger;
using std::endl;
using std::exception;
using std::pair;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::ThinReplicaClientFacade;
using thin_replica_client::Update;
using thin_replica_client::UpdateQueue;

// A global instance of LoggerInitializer class is created when the thin
// replica client library is loaded into process and destroyed when it is
// unloaded. It follows the steps suggested for code using log4cplus
// versions prior to 2.0:
// https://sourceforge.net/p/log4cplus/wiki/CodeExamples/
// LoggerInitializer is responsible for calling log4cplus::initialize in
// the constructor followed by a one-time configuration based on the settings
// from a file pointed to by the LOG4CPLUS_CONFIGURATION environment variable.
class LoggerInitializer {
 public:
  LoggerInitializer() {
    log4cplus::initialize();
    if (const char* log_properties = std::getenv("LOG4CPLUS_CONFIGURATION")) {
      log4cplus::PropertyConfigurator::doConfigure(log_properties);
    } else {
      log4cplus::BasicConfigurator cfg;
      cfg.configure();
    }
  }
  ~LoggerInitializer() { log4cplus::Logger::shutdown(); }
  LoggerInitializer(LoggerInitializer const&) = delete;
  LoggerInitializer(LoggerInitializer&&) = delete;
  LoggerInitializer& operator=(LoggerInitializer const&) = delete;
  LoggerInitializer& operator=(LoggerInitializer&&) = delete;
};
LoggerInitializer loggerInitializer;

class thin_replica_client::ThinReplicaClientFacade::Impl {
 public:
  shared_ptr<UpdateQueue> update_queue;
  unique_ptr<ThinReplicaClient> trc;
  Logger logger;
  Impl() : logger(Logger::getInstance("thin_replica.facade")) {
    update_queue.reset(new BasicUpdateQueue());
  }
};

ThinReplicaClientFacade::ThinReplicaClientFacade(
    const std::string& client_id, uint16_t max_faulty,
    const std::string& private_key,
    const std::vector<std::pair<std::string, std::string>>& servers)
    : impl(new Impl()) {
  try {
    ChannelArguments args;
    args.SetMaxReceiveMessageSize(kGrpcMaxInboundMsgSizeInBytes);
    vector<pair<string, shared_ptr<Channel>>> serverChannels;
    for (auto& server : servers) {
      serverChannels.push_back(pair<string, shared_ptr<Channel>>(
          server.first,
          CreateCustomChannel(server.second, InsecureChannelCredentials(),
                              args)));
    }

    impl->trc.reset(new ThinReplicaClient(
        client_id, impl->update_queue, max_faulty, private_key,
        serverChannels.begin(), serverChannels.end()));
  } catch (const exception& e) {
    LOG4CPLUS_ERROR(
        impl->logger,
        "An exception occurred while trying to construct a ThinReplicaClient "
        "and connect it to the Thin Replica Server(s). Exception message:"
            << endl
            << e.what());
    throw;
  }
}

ThinReplicaClientFacade::~ThinReplicaClientFacade() {}

void ThinReplicaClientFacade::Subscribe(const std::string& prefix) {
  impl->trc->Subscribe(prefix);
}

void ThinReplicaClientFacade::Subscribe(const std::string& prefix,
                                        uint64_t last_known_block_id) {
  impl->trc->Subscribe(prefix, last_known_block_id);
}

void ThinReplicaClientFacade::Unsubscribe() { impl->trc->Unsubscribe(); }

std::unique_ptr<Update> ThinReplicaClientFacade::Pop() {
  return impl->update_queue->Pop();
}

std::unique_ptr<Update> ThinReplicaClientFacade::TryPop() {
  return impl->update_queue->TryPop();
}

void ThinReplicaClientFacade::AcknowledgeBlockID(uint64_t block_id) {
  impl->trc->AcknowledgeBlockID(block_id);
}
