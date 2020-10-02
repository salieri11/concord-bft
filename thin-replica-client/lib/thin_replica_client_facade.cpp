// Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential

#include "thin_replica_client_facade.hpp"
#include <log4cplus/configurator.h>
#include "thin_replica_client.hpp"
#include "thin_replica_client_facade_impl.hpp"
#include "trs_connection.hpp"

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
using thin_replica_client::TrsConnection;
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

// HACK: Log initialization should happen in the "main" function
// Re-read the log4cplus configuration at runtime. Given that we integrate with
// JAVA and a different log framework, let's define a global variable here to
// make sure this thread will stay alive as long as the process is alive. If we
// integrate this library into C++ code then the application should do this.
const static int kLogConfigRefreshIntervalInMs = 60 * 1000;

const static char* GetLog4CplusConfigLocation() {
  auto log_location = std::getenv("LOG4CPLUS_CONFIGURATION");
  return log_location ? log_location : "LOG4CPLUS_CONFIGURATION_NOT_SET";
}
const static log4cplus::ConfigureAndWatchThread kLogConfigThread(
    GetLog4CplusConfigLocation(), kLogConfigRefreshIntervalInMs);

ThinReplicaClientFacade::ThinReplicaClientFacade(
    const std::string& client_id, uint16_t max_faulty,
    const std::string& private_key, const std::vector<std::string>& servers,
    const uint16_t max_read_data_timeout, const uint16_t max_read_hash_timeout,
    const std::string& jaeger_agent)
    : impl(new Impl()) {
  if (max_read_data_timeout == 0 || max_read_hash_timeout == 0) {
    throw runtime_error(
        "Read data/hash timeouts are set to 0 seconds. Please set a timeout "
        "> 0.");
  }

  try {
    vector<std::unique_ptr<TrsConnection>> trs_connections;

    // Create TRS connections and connect to the server
    for (const auto& address : servers) {
      auto trsc = std::make_unique<TrsConnection>(address, client_id);
      LOG4CPLUS_INFO(impl->logger, "Connecting to TRS " << *trsc);
      trsc->connect();
      trs_connections.push_back(std::move(trsc));
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
    bool servers_responsive = false;
    system_clock::time_point deadline =
        system_clock::now() + kMaxTimeToWaitForServerConnectivity;
    while (system_clock::now() <= deadline) {
      size_t num_responsive_servers = 0;
      for (const auto& trsc : trs_connections) {
        if (trsc->isConnected()) {
          ++num_responsive_servers;
        }
      }
      if (num_responsive_servers >= (max_faulty + 1)) {
        servers_responsive = true;
        break;
      }
      sleep_for(kTimeToSleepBetweenConnectionAttempts);
    }

    if (!servers_responsive) {
      throw runtime_error(
          "Failed to construct Thin Replica Client: could not connect to "
          "enough of the provided servers within " +
          to_string(kMaxTimeToWaitForServerConnectivity.count()) + " " +
          kMaxTimeToWaitForServerConnectivityUnitLabel + ".");
    }

    impl->trc.reset(new ThinReplicaClient(
        client_id, impl->update_queue, max_faulty, private_key,
        std::move(trs_connections), max_read_data_timeout,
        max_read_hash_timeout, jaeger_agent));
  } catch (const exception& e) {
    LOG4CPLUS_ERROR(
        impl->logger,
        "An exception occurred while trying to construct a ThinReplicaClient "
        "and connect it to the Thin Replica Server(s). Exception message: "
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

void ThinReplicaClientFacade::ReleaseConsumers() {
  impl->update_queue->ReleaseConsumers();
}

void ThinReplicaClientFacade::AcknowledgeBlockID(uint64_t block_id) {
  impl->trc->AcknowledgeBlockID(block_id);
}
