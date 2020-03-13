// Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential

#include "thin_replica_client_facade.hpp"
#include <log4cplus/configurator.h>
#include "thin_replica.grpc.pb.h"
#include "thin_replica_client.hpp"
#include "thin_replica_client_facade_impl.hpp"

using grpc::Channel;
using grpc::ChannelArguments;
using grpc::InsecureChannelCredentials;
using log4cplus::Logger;
using std::endl;
using std::exception;
using std::pair;
using std::runtime_error;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::vector;
using std::chrono::operator""s;
using com::vmware::concord::thin_replica::ThinReplica;
using std::chrono::seconds;
using std::chrono::system_clock;
using std::this_thread::sleep_for;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::ThinReplicaClientFacade;
using thin_replica_client::Update;
using thin_replica_client::UpdateQueue;

// If the ThinReplicaClientFacade constructor fails to find enough servers to be
// responsive, it will repeatedly sleep for kTimeToSleepBetweenConectionAttempts
// and retry connection, for up to kMaxTimeToWaitForServerConnectivity, after
// which point it will throw an exception; this can help prevent failures of the
// Thin Replica Client induced when it tries to connect to a cluster that is
// still in the process of coming up, which may be common in testing scenarios.
const static seconds kMaxTimeToWaitForServerConnectivity = 60s;
const static string kMaxTimeToWaitForServerConnectivityUnitLabel = "seconds";
const static seconds kTimeToSleepBetweenConnectionAttempts = 1s;

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

ThinReplicaClientFacade::ThinReplicaClientFacade(
    const std::string& client_id, uint16_t max_faulty,
    const std::string& private_key,
    const std::vector<std::pair<std::string, std::string>>& servers)
    : impl(new Impl()) {
  try {
    ChannelArguments args;
    args.SetMaxReceiveMessageSize(kGrpcMaxInboundMsgSizeInBytes);
    vector<shared_ptr<Channel>> serverChannels;
    for (auto& server : servers) {
      serverChannels.push_back(shared_ptr<Channel>(CreateCustomChannel(
          server.second, InsecureChannelCredentials(), args)));
    }

    // Wait (up to a timeout) to validate the server(s) to connect to are
    // responsive before proceeding to construct the ThinReplicaClient object;
    // this should prevent crashes and exceptions possibly arising from trying
    // to connect to a cluster in a transient start-up state that is not yet
    // fully up and running.
    //
    // Note this is a temporary fix; we intend for the ThinReplicaClient itself
    // will more gracefully error-handle this case in the future. Note this
    // check assumes the ThinReplicaClient implementation will need to be able
    // to connect to a total of max_faulty + 1 servers to make progress.
    assert(serverChannels.size() > 0);
    bool servers_responsive = false;
    system_clock::time_point time_to_wait_until =
        system_clock::now() + kMaxTimeToWaitForServerConnectivity;
    while (!servers_responsive && (system_clock::now() <= time_to_wait_until)) {
      size_t num_responsive_servers = 0;
      for (const auto& server : serverChannels) {
        if (server->GetState(true) ==
            grpc_connectivity_state::GRPC_CHANNEL_READY) {
          ++num_responsive_servers;
        }
      }
      if (num_responsive_servers >= (max_faulty + 1)) {
        servers_responsive = true;
      } else {
        sleep_for(kTimeToSleepBetweenConnectionAttempts);
      }
    }
    if (!servers_responsive) {
      throw runtime_error(
          "Failed to construct Thin Replica Client: could not connect to "
          "enough of the provided servers within " +
          to_string(kMaxTimeToWaitForServerConnectivity.count()) + " " +
          kMaxTimeToWaitForServerConnectivityUnitLabel + ".");
    }

    vector<pair<string, unique_ptr<ThinReplica::StubInterface>>> server_stubs;
    assert(serverChannels.size() == servers.size());
    for (size_t i = 0; i < servers.size(); ++i) {
      server_stubs.push_back(
          pair<string, unique_ptr<ThinReplica::StubInterface>>{
              servers[i].first, ThinReplica::NewStub(serverChannels[i])});
    }

    impl->trc.reset(new ThinReplicaClient(
        client_id, impl->update_queue, max_faulty, private_key,
        server_stubs.begin(), server_stubs.end()));
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
                                        uint64_t block_id) {
  impl->trc->Subscribe(prefix, block_id);
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

ThinReplicaClientFacade::ThinReplicaClientFacade(
    unique_ptr<ThinReplicaClientFacade::Impl>&& impl)
    : impl(move(impl)) {}
