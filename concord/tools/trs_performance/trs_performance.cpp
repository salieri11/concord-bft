// Copyright 2018-2020 VMware, all rights reserved
//
// Concord node startup.

#define BOOST_BIND_NO_PLACEHOLDERS

#include <grpcpp/grpcpp.h>
#include <grpcpp/resource_quota.h>
#include <sys/stat.h>
#include <boost/asio.hpp>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>
#include <csignal>
#include <fstream>
#include <iostream>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <thread>

#include "Logger.hpp"
#include "thin_replica/grpc_services.hpp"
#include "thin_replica/subscription_buffer.hpp"
#include "thin_replica/thin_replica_impl.hpp"
#include "trs_perf_mock_db_interfaces.hpp"
#include "utils/concord_prometheus_metrics.hpp"

using namespace std;

using logging::Logger;

using concord::thin_replica::SubBufferList;
using concord::thin_replica::ThinReplicaImpl;
using concord::thin_replica::ThinReplicaService;

static unique_ptr<grpc::Server> thin_replica_server = nullptr;

/*
 * Reads certificates to be used by thin replica server to establish
 * TLS connection with thin replica client if TLS is enabled for TRS
 */
void readCert(const std::string &input_filename, std::string &out_data) {
  Logger logger = Logger::getInstance("concord.thin_replica");

  std::ifstream input_file(input_filename.c_str(), std::ios::in);

  if (!input_file.is_open()) {
    LOG_FATAL(logger, "Failed to construct thin replica server.");
    throw runtime_error(
        __PRETTY_FUNCTION__ +
        std::string(": Concord could not open the input file (") +
        input_filename +
        std::string(
            ") to establish TLS connection with the thin replica client."));
  } else {
    try {
      std::stringstream read_buffer;
      read_buffer << input_file.rdbuf();
      input_file.close();
      out_data = read_buffer.str();
      LOG_INFO(logger,
               "Successfully loaded the contents of " + input_filename + ".");
    } catch (const std::exception &e) {
      LOG_FATAL(logger, "Failed to construct thin replica server.");
      throw runtime_error(
          __PRETTY_FUNCTION__ +
          std::string(
              ": An exception occurred while trying to read the input file (") +
          input_filename + std::string("): ") + std::string(e.what()));
    }
  }
  return;
}

void RunThinReplicaServer(
    std::string server_address,
    concord::perf::trs::TrsPerfFakeStorage *ro_storage,
    SubBufferList &subscriber_list, int max_num_threads,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry,
    bool is_insecure_trs, std::string thin_replica_tls_cert_path,
    int num_blocks, int num_kvp, int key_size, int val_size) {
  Logger logger = Logger::getInstance("concord.perf.trs");

  auto thinReplicaServiceImpl = std::make_unique<ThinReplicaImpl>(
      ro_storage, subscriber_list, prometheus_registry);
  auto thinReplicaService =
      std::make_unique<ThinReplicaService>(std::move(thinReplicaServiceImpl));

  grpc::ResourceQuota quota;
  quota.SetMaxThreads(max_num_threads);

  grpc::ServerBuilder builder;

  if (not is_insecure_trs) {
    LOG_INFO(logger,
             "TLS for thin replica server is enabled, certificate path: "
                 << thin_replica_tls_cert_path);
    std::string server_cert_path, server_key_path, root_cert_path;
    std::string server_cert, server_key, root_cert;

    server_cert_path = thin_replica_tls_cert_path;
    // Read the certs
    readCert(server_cert_path + "/server.cert", server_cert);
    readCert(server_cert_path + "/pk.pem", server_key);

    grpc::SslServerCredentialsOptions::PemKeyCertPair keycert = {server_key,
                                                                 server_cert};

    grpc::SslServerCredentialsOptions sslOps;

    sslOps.pem_key_cert_pairs.push_back(keycert);

    // Request and verify client certificate for mutual TLS
    sslOps.client_certificate_request =
        GRPC_SSL_REQUEST_AND_REQUIRE_CLIENT_CERTIFICATE_BUT_DONT_VERIFY;

    // Use populated ssl server credentials
    builder.AddListeningPort(server_address,
                             grpc::SslServerCredentials(sslOps));
  } else {
    LOG_WARN(logger,
             "TLS for thin replica server is disabled, falling back to "
             "insecure channel");
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  }

  builder.SetResourceQuota(quota);
  builder.RegisterService(thinReplicaService.get());

  int start_block_id = 1;
  int last_block_id = num_blocks;

  ro_storage->fillWithData(start_block_id, last_block_id, num_kvp, key_size,
                           val_size);
  thin_replica_server = unique_ptr<grpc::Server>(builder.BuildAndStart());

  LOG_INFO(logger, "Thin replica server listening on " << server_address);

  thin_replica_server->Wait();
}

std::shared_ptr<concord::utils::PrometheusRegistry>
initialize_prometheus_metrics(Logger &logger) {
  uint64_t dump_time_interval = 600;
  std::string host = "0.0.0.0";
  std::string prom_bindaddress = host + ":" + "5458";
  LOG_INFO(logger, "prometheus metrics address is: "
                       << prom_bindaddress
                       << " dumping metrics interval: " << dump_time_interval);
  return std::make_shared<concord::utils::PrometheusRegistry>(
      prom_bindaddress, dump_time_interval);
}

// Start the ThinReplicaService
int run_service(Logger &logger, int num_blocks, int num_kvp, int key_size,
                int val_size) {
  auto prometheus_registry = initialize_prometheus_metrics(logger);

  // List of ring buffers (one per subscriber) for thin replica subscription
  // service.
  SubBufferList subscriber_list;

  try {
    std::string server_addr{std::getenv("ADDRESS")};

    // Limit the amount of gRPC threads
    int max_num_threads = 32;

    // The tool runs without TLS by default
    bool is_insecure_trs = true;
    std::string thin_replica_tls_cert_path = "";

    concord::perf::trs::TrsPerfFakeStorage fake_storage;

    std::thread t1(RunThinReplicaServer, server_addr, &fake_storage,
                   std::ref(subscriber_list), max_num_threads,
                   prometheus_registry, is_insecure_trs,
                   thin_replica_tls_cert_path, num_blocks, num_kvp, key_size,
                   val_size);
    t1.join();

  } catch (std::exception &ex) {
    LOG_FATAL(logger, ex.what());
    return -1;
  }
  return 0;
}

int main(int argc, char **argv) {
  int result = 0;

  Logger trsLogger = Logger::getInstance("concord.perf.trs");
  try {
    // Initialize logger
    LOG_CONFIGURE_AND_WATCH("/concord/config-public/log4cplus.properties",
                            60000);

    // re-initialize logger after configuration
    trsLogger = Logger::getInstance("concord.main");

    // say hello
    LOG_INFO(trsLogger, "TRC-TRS Performance Benchmark Tool Starting");

    std::string usage_string =
        "./trs_performance <num-blocks> <num-key-value-pairs-per-block> "
        "<key-size> <value-size>\n"
        "num-blocks: number of blocks to be sent per second\n"
        "num-key-value-pairs-per-block: number of key value pairs to be "
        "sent per block\n"
        "key-size: size of each key in kilobytes\n"
        "value-size: size of each value in kilobytes\n";

    if (argc < 5) {
      LOG_FATAL(trsLogger,
                "The benchmark tool expects five arguments. Usage guide: \n"
                    << usage_string);
      return -1;
    }

    int num_blocks = std::stoi(argv[1]);
    int num_kvp = std::stoi(argv[2]);
    int key_size = std::stoi(argv[3]) * 1024;
    int val_size = std::stoi(argv[4]) * 1024;

    std::string temp;
    if (key_size > (temp.max_size() * sizeof(char))) {
      LOG_FATAL(trsLogger,
                "The key_size should be < ." << temp.max_size() * sizeof(char));
      return -1;
    }
    if (val_size > (temp.max_size() * sizeof(char))) {
      LOG_FATAL(trsLogger,
                "The val_size should be < ." << temp.max_size() * sizeof(char));
      return -1;
    }

    LOG_INFO(trsLogger, "Benchmark Input Parameters:\n num_blocks:"
                            << num_blocks << "\n num_kvp: " << num_kvp
                            << "\n key_size: " << key_size
                            << "\n val_size: " << val_size);

    result = run_service(trsLogger, num_blocks, num_kvp, key_size, val_size);

    LOG_INFO(trsLogger, "TRC-TRS Performance Benchmark Tool Halting");
  } catch (const std::exception &ex) {
    LOG_FATAL(trsLogger, ex.what());
    result = -1;
  }
  return result;
}
