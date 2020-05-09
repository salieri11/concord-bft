// Copyright 2019-2020 VMware, all rights reserved   -  March 11 2020 @bjay

#include <grpc++/grpc++.h>
#include <log4cplus/configurator.h>
#include <log4cplus/loggingmacros.h>
#include <thread>
#include "thin_replica.grpc.pb.h"
#include "thin_replica_client_facade.hpp"

// namespace
using namespace std;
using log4cplus::Logger;
using thin_replica_client::ThinReplicaClientFacade;
using thin_replica_client::Update;

// static pointers
static unique_ptr<Logger> logger_p = nullptr;
static unique_ptr<ThinReplicaClientFacade> trc_facade = nullptr;
static unique_ptr<grpc::Server> app_grpc_server = nullptr;

// globals
bool global_stop = false;
mutex mutex_;
condition_variable cv_;

/*
Test App uses DecoratedThinReplica object: a decorated thin replica server grpc
to communicate with hermes only subscribe and unsunscribe is implimented can
only call subscribe once, and then unsubscribe

known issue: program hangs on exit.. requires an addition block to exit from
concord

params: PORT - this grpc server listens on a port passed in args 0.0.0.0:<port>
  meant for use with docker network and -p <host_port>:<port>
*/
class DecoratedThinReplica
    : public com::vmware::concord::thin_replica::ThinReplica::Service {
 private:
  bool subscribed = false;

 public:
  grpc::Status SubscribeToUpdates(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest* request,
      grpc::ServerWriter<com::vmware::concord::thin_replica::Data>* stream)
      override {
    LOG4CPLUS_DEBUG(*logger_p, "SubscribeToUpdates called");

    if (subscribed || global_stop) return grpc::Status::CANCELLED;

    subscribed = true;
    doSubscribeUpdate(stream);

    // exiting now
    LOG4CPLUS_DEBUG(*logger_p, "after doSubscribeUpdate");
    Stop();
    return grpc::Status::OK;
  }

  grpc::Status Unsubscribe(grpc::ServerContext* context,
                           const google::protobuf::Empty* request,
                           google::protobuf::Empty* response) override {
    LOG4CPLUS_DEBUG(*logger_p, "Unsubscribe called");

    if (subscribed && !global_stop)
      trc_facade->ReleaseConsumers();
    else
      Stop();
    return grpc::Status::OK;
  }

  grpc::Status doSubscribeUpdate(
      grpc::ServerWriter<com::vmware::concord::thin_replica::Data>* stream) {
    com::vmware::concord::thin_replica::Data data;
    while (true) {
      try {
        std::unique_ptr<Update> update = trc_facade->Pop();  // block
        data = com::vmware::concord::thin_replica::Data::default_instance();

        if (!update) {
          LOG4CPLUS_DEBUG(*logger_p, "pop-!update");
          break;
        }
        LOG4CPLUS_DEBUG(*logger_p, "pop-" << update->block_id);
        data.set_block_id(update->block_id);
        for (const auto& kvpi : update->kv_pairs) {
          auto kvpo = data.add_data();
          kvpo->set_key(kvpi.first);
          kvpo->set_value(kvpi.second);
        }
        stream->Write(data);
      } catch (std::exception& error) {
        cerr << endl;
        LOG4CPLUS_INFO(*logger_p,
                       "Data subscription stream closed: " << error.what());
        break;
      }
    }

    LOG4CPLUS_DEBUG(*logger_p, "doSubscribeUpdate return");
    return grpc::Status::OK;
  }

  void Stop() {
    LOG4CPLUS_DEBUG(*logger_p, "Stop()");
    if (!global_stop) {
      global_stop = true;
      cv_.notify_one();
    }
  }

  grpc::Status ReadState(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::ReadStateRequest* request,
      grpc::ServerWriter<com::vmware::concord::thin_replica::Data>* stream)
      override {
    return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "ReadState");
  }

  grpc::Status ReadStateHash(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::ReadStateHashRequest* request,
      com::vmware::concord::thin_replica::Hash* hash) override {
    return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "ReadStateHash");
  }

  grpc::Status AckUpdate(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::BlockId* block_id,
      google::protobuf::Empty* empty) override {
    return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "AckUpdate");
  }

  grpc::Status SubscribeToUpdateHashes(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest* request,
      grpc::ServerWriter<com::vmware::concord::thin_replica::Hash>* stream)
      override {
    return grpc::Status(grpc::StatusCode::UNIMPLEMENTED,
                        "SubscribeToUpdateHashes");
  }
};

/* RunGrpc function runs as detached thread for grpc server */
void RunGrpc(const std::string& server_address) {
  LOG4CPLUS_DEBUG(*logger_p, "thread start RunGrpc: " << server_address);

  DecoratedThinReplica* grpc_service = new DecoratedThinReplica();
  LOG4CPLUS_DEBUG(*logger_p, "grpc_service");

  grpc::ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(grpc_service);
  app_grpc_server = unique_ptr<grpc::Server>(builder.BuildAndStart());
  app_grpc_server->Wait();
  LOG4CPLUS_DEBUG(*logger_p, "thread end RunGrpc");
}

int main(int argc, char** argv) {
  // logger
  log4cplus::initialize();
  if (const char* log_properties = std::getenv("LOG4CPLUS_CONFIGURATION")) {
    log4cplus::PropertyConfigurator::doConfigure(log_properties);
  } else {
    log4cplus::BasicConfigurator cfg;
    cfg.configure();
  }
  logger_p = unique_ptr<Logger>(
      new Logger(Logger::getInstance("thin_replica.test_app")));

  // args
  int ii = 0;
  const size_t kClientIdIndex = ++ii;
  const size_t kGrpcPortIndex = ++ii;
  const size_t kNumServersIndex = ++ii;
  const size_t kMaxFaultyIndex = ++ii;
  const size_t kServersOffset = ++ii;
  string usage_text =
      "trc_test_app \n"
      "usage: trc_test_app<CLIENT_ID> <GRPC_PORT> <NUM_SERVERS> <MAX_FAULTY> "
      "<SERVERS...>\n"
      "<CLIENT_ID> is the client_id for subscribing to ThinReplicaServer"
      "<GRPC_PORT> is the port for communcating with this running app, "
      "through a Decorated ThinReplicaServer interface \n"
      "<NUM_SERVERS> is the number of thin replica servers available in this "
      "cluster for the thin replica client to connect to.\n"
      "<MAX_FAULTY> is the maximum number of Byzantine-faulty thin replica "
      "servers the thin replica client must tolerate. <NUM_SERVERS> must be at "
      "least (3 * <MAX_FAULTY> + 1).\n"
      "<SERVERS...> should be <NUM_SERVERS> separate arguments, each a network "
      "address for one of the thin replica servers avaliable for the thin "
      "replica client to connect to. Network addresses should be of the form "
      "<IP ADDRESS>:<PORT NUMBER> or <HOST NAME>:<PORT NUMBER>";

  if (argc <= 1) {
    cout << usage_text << endl;
    return 0;
  }

  if (argc < kServersOffset) {
    LOG4CPLUS_ERROR(
        *logger_p,
        "Too few arguments to trc_test_app (outputting usage text).");
    cout << usage_text << endl;
    return -1;
  }

  string client_id(argv[kClientIdIndex]);
  uint16_t grpc_port;
  uint16_t num_servers;
  uint16_t max_faulty;
  bool has_grpc_port = false;
  try {
    unsigned long long port_raw = stoull(argv[kGrpcPortIndex]);
    num_servers = (uint16_t)port_raw;
    has_grpc_port = ((port_raw > 0) && (port_raw <= UINT16_MAX));
  } catch (const exception& e) {
    has_grpc_port = false;
  }
  if (!has_grpc_port) {
    LOG4CPLUS_FATAL(*logger_p, "Invalid grpc port: " << argv[kGrpcPortIndex]);
    return -1;
  }

  bool has_num_servers = false;
  try {
    unsigned long long num_servers_raw = stoull(argv[kNumServersIndex]);
    num_servers = (uint16_t)num_servers_raw;
    has_num_servers =
        ((num_servers_raw > 0) && (num_servers_raw <= UINT16_MAX));
  } catch (const exception& e) {
    has_num_servers = false;
  }
  if (!has_num_servers) {
    LOG4CPLUS_FATAL(
        *logger_p, "Invalid number of servers: \""
                       << argv[kNumServersIndex]
                       << "\". The number of servers is expected to "
                          "be a positive integer in range ["
                       << to_string(1) << "," << to_string(UINT16_MAX) << "].");
    return -1;
  }

  bool has_max_faulty = false;
  try {
    unsigned long long max_faulty_raw = stoull(argv[kMaxFaultyIndex]);
    max_faulty = (uint16_t)max_faulty_raw;
    has_max_faulty = ((max_faulty_raw >= 0) && (max_faulty_raw <= UINT16_MAX));
  } catch (const exception& e) {
    has_max_faulty = false;
  }
  if (!has_max_faulty) {
    LOG4CPLUS_FATAL(
        *logger_p,
        "Invalid maximum number of faulty servers: \""
            << argv[kMaxFaultyIndex]
            << "\". The maximum number of faulty servers is expected "
               "to be a non-negative integer in range ["
            << to_string(0) << "," << to_string(UINT16_MAX) << "].");
    return -1;
  }

  // Note we attempt to compute whether num_servers is sufficient to accomodate
  // max_faulty using 64-bit arithmetic in case (3 * max_faulty + 1) overflows a
  // 16-bit unsigned integer
  if ((uint64_t)num_servers <
      ((uint64_t)max_faulty * (uint64_t)3 + (uint64_t)1)) {
    LOG4CPLUS_FATAL(*logger_p,
                    "Insufficient number of servers ("
                        << to_string(num_servers)
                        << ") to accomodate maximum number of faulty servers("
                        << to_string(max_faulty)
                        << "). The number of servers must be at least (3 * "
                           "(max faulty servers) + 1).");
    return -1;
  }

  if ((size_t)argc != ((size_t)kServersOffset + (size_t)num_servers)) {
    LOG4CPLUS_FATAL(*logger_p,
                    "Number of servers given ("
                        << to_string(num_servers)
                        << ") does not agree with number of network addresses "
                           "for servers provided in arguments (counted "
                        << to_string(argc - kServersOffset)
                        << " address arguments).");
    return -1;
  }

  vector<pair<string, string>> servers;
  for (uint16_t i = 0; i < num_servers; ++i) {
    servers.push_back(make_pair(string(), argv[kServersOffset + i]));
  }

  // run
  string private_key;
  int ret_status = 0;
  try {
    LOG4CPLUS_DEBUG(
        *logger_p, "Attempting to construct ThinReplicaClient..." << client_id);

    trc_facade.reset(new ThinReplicaClientFacade(
        client_id, max_faulty, private_key, servers, 5, 5, "127.0.0.1:6831"));

    LOG4CPLUS_DEBUG(*logger_p, "ThinReplicaClient constructed.");
    trc_facade->Subscribe("");
    LOG4CPLUS_DEBUG(*logger_p, "ThinReplicaClient subscribed.");
    auto dserver = string("0.0.0.0:") + argv[kGrpcPortIndex];
    LOG4CPLUS_INFO(*logger_p,
                   "test app decorated grpc thin replica server: " << dserver);

    // grpc thread
    std::thread(RunGrpc, string("0.0.0.0:") + argv[kGrpcPortIndex]).detach();
    {
      LOG4CPLUS_DEBUG(*logger_p, "test app running grpc server");
      std::unique_lock<std::mutex> lock(mutex_);
      cv_.wait(lock, [] { return global_stop; });
    }

    // shutdown
    LOG4CPLUS_DEBUG(*logger_p, "global_stop - Shutting down test app");
    LOG4CPLUS_INFO(*logger_p, "Stopping test-app");

    std::chrono::system_clock::time_point deadline =
        std::chrono::system_clock::now() + std::chrono::milliseconds(1000);
    app_grpc_server->Shutdown(deadline);
    LOG4CPLUS_DEBUG(*logger_p, "grpc unblocked Shutting down test app");

    LOG4CPLUS_DEBUG(
        *logger_p,
        "Try Unsubscribe trc - sometimes waits on new Block from TRS-concord");
    trc_facade->Unsubscribe();
    LOG4CPLUS_INFO(*logger_p, "ThinReplicaClient unsubscribed.");
  } catch (const exception& e) {
    LOG4CPLUS_ERROR(*logger_p, "exception test-app:" << e.what());
    ret_status = -1;
  }

  // debug clean exit issues
  app_grpc_server.reset();
  LOG4CPLUS_DEBUG(*logger_p, "app_grpc_server reset.");
  trc_facade.reset();
  LOG4CPLUS_DEBUG(*logger_p, "ThinReplicaClient reset.");
  LOG4CPLUS_DEBUG(*logger_p, "return " << to_string(ret_status) << "; //main");
  return ret_status;
}
