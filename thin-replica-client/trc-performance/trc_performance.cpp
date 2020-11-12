// Copyright 2019-2020 VMware, all rights reserved

#include <grpc++/grpc++.h>
#include <log4cplus/configurator.h>
#include <log4cplus/loggingmacros.h>
#include <unistd.h>
#include <chrono>
#include <condition_variable>
#include <fstream>
#include <iomanip>
#include <numeric>
#include <thread>
#include "thin_replica.grpc.pb.h"
#include "thin_replica_client_facade.hpp"

// namespace
using log4cplus::Logger;
using thin_replica_client::ThinReplicaClientFacade;
using thin_replica_client::Update;

// static pointers
static std::unique_ptr<Logger> logger_p = nullptr;
static std::unique_ptr<ThinReplicaClientFacade> trc_facade = nullptr;

// calculate median
int median(std::vector<int>& v) {
  size_t n = v.size() / 2;
  std::nth_element(v.begin(), v.begin() + n, v.end());
  return v[n];
}

// calculate percentile
double percentile(std::vector<int>& v, double percent) {
  auto nth = v.begin() + (percent * v.size()) / 100;
  std::nth_element(v.begin(), nth, v.end());
  return *nth;
}

// read from the populated csv file, time
void getResults(std::string server, std::string trc_time_file_path,
                int latest_block_id, const int num_times) {
  std::string trs_time_file_path = "/concord/trc-trs-perf-results/trs" +
                                   std::string(1, server.back()) + ".csv";
  std::ifstream trs_res(trs_time_file_path);
  std::ifstream trc_res(trc_time_file_path);
  int latency = 0;
  unsigned long total_bytes_rcvd = 0;
  int count = 0;
  std::vector<int> latency_vec;
  unsigned long start_time = 0;
  unsigned long end_time = 0;
  while (!trs_res.eof() & !trc_res.eof()) {
    std::string trs_out, trc_out;
    std::vector<std::string> trs_temp, trc_temp;

    if (!trs_res.eof()) trs_res >> trs_out;
    if (!trc_res.eof()) trc_res >> trc_out;

    // If block ids match get the data into vectors
    if (trs_out.front() == trc_out.front()) {
      size_t pos = 0;
      std::string delim = ",";
      while ((pos = trs_out.find(delim)) != std::string::npos) {
        std::string token = trs_out.substr(0, pos);
        trs_temp.push_back(token);
        trs_out.erase(0, pos + delim.length());
      }
      while ((pos = trc_out.find(delim)) != std::string::npos) {
        std::string token = trc_out.substr(0, pos);
        trc_temp.push_back(token);
        trc_out.erase(0, pos + delim.length());
      }
      trc_temp.push_back(trc_out);
    }
    count++;
    // get the start timestamp i.e., when the first update was read from
    // fakestorage
    if (count == 1) {
      start_time = std::stoull(trs_temp[2]);
    }
    // for each update, calculate latency i.e., post-processing completion time
    // at TRC - preprocessing start time at TRS
    latency_vec.push_back(std::stoull(trc_temp[2]) - std::stoull(trs_temp[2]));
    total_bytes_rcvd += std::stoull(trc_temp[3]);
    if (count == latest_block_id * num_times) {
      end_time = std::stoull(trc_temp[2]);
      break;
    }
  }
  // calculate throughput
  int throughput = 0;
  int throughput_updates_per_sec = 0;
  if (end_time - start_time) {
    throughput = total_bytes_rcvd / (end_time - start_time);
    throughput_updates_per_sec =
        latency_vec.size() * 1000 / (end_time - start_time);
  }
  // calculate median, max, average, 99th percentile latency
  int median_latency = median(latency_vec);
  int max_latency = *max_element(latency_vec.begin(), latency_vec.end());
  int average_latency = 0;
  if (latency_vec.size() != 0) {
    average_latency =
        std::accumulate(latency_vec.begin(), latency_vec.end(), 0.0) /
        latency_vec.size();
  }
  int percentile_99th_latency = percentile(latency_vec, 99);
  LOG4CPLUS_INFO(*logger_p, "Performance Benchmark Results");
  LOG4CPLUS_INFO(*logger_p, "Throughput: " << throughput << " MBps");
  LOG4CPLUS_INFO(*logger_p, "Throughput: " << throughput_updates_per_sec
                                           << " Updates/second");
  LOG4CPLUS_INFO(*logger_p, "Median Latency: " << median_latency << " ms");
  LOG4CPLUS_INFO(*logger_p, "Max Latency: " << max_latency << " ms");
  LOG4CPLUS_INFO(*logger_p, "Average Latency: " << average_latency << " ms");
  LOG4CPLUS_INFO(*logger_p, "99th Percentile Latency: "
                                << percentile_99th_latency << " ms\n");
}

int main(int argc, char** argv) {
  // initialize logger
  log4cplus::initialize();
  if (const char* log_properties = std::getenv("LOG4CPLUS_CONFIGURATION")) {
    std::cout << log_properties << std::endl;
    log4cplus::PropertyConfigurator::doConfigure(log_properties);
  } else {
    log4cplus::BasicConfigurator cfg;
    cfg.configure();
  }
  logger_p = std::unique_ptr<Logger>(
      new Logger(Logger::getInstance("concord.perf.trc")));

  // command line arguments
  int ii = 0;
  const size_t kClientIdIndex = ++ii;
  const size_t kNumServersIndex = ++ii;
  const size_t kMaxFaultyIndex = ++ii;
  const size_t kNumOfTimesToReadUpdates = ++ii;
  const size_t kServersOffset = ++ii;
  std::string usage_text =
      "trc_perf_app \n"
      "usage: trc_perf_app <client_id> <num_servers> <max_faulty> "
      "<list_of_server_addresses>\n"

      "<client_id> is the client_id used for subscribing to ThinReplicaServer"

      "<num_servers> is the number of thin replica servers available in this "
      "cluster for the thin replica client to connect to.\n"

      "<max_faulty> is the maximum number of Byzantine-faulty thin replica "
      "servers the thin replica client must tolerate. <num_servers> must be at "
      "least (3 * <max_faulty> + 1).\n"

      "<num_times> is the number of times to read k unique updates generated "
      "by the ThinReplicaServer.\n"

      "<list_of_server_addresses> should be <num_servers> separate arguments, "
      "each a network "
      "address for one of the thin replica servers available for the thin "
      "replica client to connect to. Network addresses should be of the form "
      "<IP ADDRESS>:<PORT NUMBER> or <HOST NAME>:<PORT NUMBER>";

  if (argc < 2) {
    std::cout << usage_text << std::endl;
    return 0;
  }

  if (argc < kServersOffset) {
    LOG4CPLUS_ERROR(
        *logger_p,
        "Too few arguments to trc_perf_client (outputting usage text).");
    std::cout << usage_text << std::endl;
    return -1;
  }

  std::string client_id(argv[kClientIdIndex]);
  const uint16_t num_servers = (uint16_t)std::stoull(argv[kNumServersIndex]);
  const uint16_t max_faulty = (uint16_t)std::stoull(argv[kMaxFaultyIndex]);
  const uint16_t num_times_to_read =
      (uint16_t)std::stoull(argv[kNumOfTimesToReadUpdates]);
  uint16_t num_times = num_times_to_read;

  if (num_servers < 0 || num_servers > UINT16_MAX) {
    LOG4CPLUS_FATAL(
        *logger_p, "Invalid number of servers: \""
                       << argv[kNumServersIndex]
                       << "\". The number of servers is expected to "
                          "be a positive integer in range ["
                       << std::to_string(1) << "," << std::to_string(UINT16_MAX)
                       << "].");
    return -1;
  }

  if (max_faulty < 0 || max_faulty > UINT16_MAX) {
    LOG4CPLUS_FATAL(
        *logger_p,
        "Invalid maximum number of faulty servers: \""
            << argv[kMaxFaultyIndex]
            << "\". The maximum number of faulty servers is expected "
               "to be a non-negative integer in range ["
            << std::to_string(0) << "," << std::to_string(UINT16_MAX) << "].");
    return -1;
  }

  if (num_times < 1 || num_times > UINT16_MAX) {
    LOG4CPLUS_FATAL(*logger_p, "Invalid number of times: \""
                                   << argv[kNumOfTimesToReadUpdates]
                                   << "\". The maximum number of times to read "
                                      "k unique updates is expected "
                                      "to be a non-negative integer in range ["
                                   << std::to_string(0) << ","
                                   << std::to_string(UINT16_MAX) << "].");
    return -1;
  }

  // Note we attempt to compute whether num_servers is sufficient to accomodate
  // max_faulty using 64-bit arithmetic in case (3 * max_faulty + 1) overflows a
  // 16-bit unsigned integer
  if ((uint64_t)num_servers <
      ((uint64_t)max_faulty * (uint64_t)3 + (uint64_t)1)) {
    LOG4CPLUS_FATAL(*logger_p,
                    "Insufficient number of servers ("
                        << std::to_string(num_servers)
                        << ") to accomodate maximum number of faulty servers("
                        << std::to_string(max_faulty)
                        << "). The number of servers must be at least (3 * "
                           "(max faulty servers) + 1).");
    return -1;
  }

  if ((size_t)argc != ((size_t)kServersOffset + (size_t)num_servers)) {
    LOG4CPLUS_FATAL(*logger_p,
                    "Number of servers given ("
                        << std::to_string(num_servers)
                        << ") does not agree with number of network addresses "
                           "for servers provided in arguments (counted "
                        << std::to_string(argc - kServersOffset)
                        << " address arguments).");
    return -1;
  }

  std::vector<std::string> servers;
  for (uint16_t i = 0; i < num_servers; ++i) {
    LOG4CPLUS_INFO(*logger_p, "Server: " << argv[kServersOffset + i]);
    servers.push_back(argv[kServersOffset + i]);
  }

  // run
  std::string private_key;
  std::vector<std::string> trc_time_vec;
  int ret_status = 0;
  try {
    LOG4CPLUS_INFO(*logger_p,
                   "Attempting to construct ThinReplicaClient with client id: "
                       << client_id);

    trc_facade.reset(new ThinReplicaClientFacade(
        client_id, max_faulty, private_key, servers, 5, 5, "127.0.0.1:6831"));

    LOG4CPLUS_INFO(*logger_p, "ThinReplicaClient constructed.");
    trc_facade->Subscribe("");
    LOG4CPLUS_INFO(*logger_p, "ThinReplicaClient subscribed.");

    auto update = trc_facade->TryPop();
    bool has_update = (bool)update;
    uint64_t latest_block_id = 0;
    if (!has_update) {
      LOG4CPLUS_INFO(
          *logger_p,
          "Subscription call did not yield any updates as initial state.");
    } else {
      LOG4CPLUS_INFO(
          *logger_p,
          "The subscribe appears to have returned initial state to the update "
          "queue; fetching state from the update queue...");
    }
    // Keep reading the same updates <num-times>
    while (update && num_times) {
      latest_block_id = update->block_id;
      auto latest_kvp = update->kv_pairs;

      trc_facade->AcknowledgeBlockID(latest_block_id);
      LOG4CPLUS_DEBUG(*logger_p, "Update(s) acknowledged.");

      // get the time when post-processing stops at the client
      const auto end_time = std::chrono::system_clock::now();
      auto duration = end_time.time_since_epoch();
      auto millis =
          std::chrono::duration_cast<std::chrono::milliseconds>(duration)
              .count();

      // calculate length of data received
      int len_rcvd = 0;
      for (auto kv : latest_kvp) {
        len_rcvd += kv.first.length() + kv.second.length();
      }
      LOG4CPLUS_DEBUG(
          *logger_p,
          "TRC (" << client_id << ") received the block id: " << latest_block_id
                  << " of length: " << len_rcvd << " at time:" << millis
                  << "(time since epoch (in ms)).");

      // collect the data for latency and throughput measurement calculation
      trc_time_vec.push_back(std::to_string(latest_block_id) + "," + client_id +
                             "," + std::to_string(millis) + "," +
                             std::to_string(len_rcvd * sizeof(char)) + "\n");

      update = trc_facade->TryPop();

      // subscribe again
      if (!update && num_times != 1) {
        num_times--;
        trc_facade->Unsubscribe();
        LOG4CPLUS_DEBUG(*logger_p, "ThinReplicaClient constructed.");
        trc_facade->Subscribe("");
        LOG4CPLUS_DEBUG(*logger_p, "ThinReplicaClient subscribed.");

        update = trc_facade->TryPop();
        has_update = (bool)update;
        latest_block_id = 0;
        if (!has_update) {
          LOG4CPLUS_DEBUG(
              *logger_p,
              "Subscription call did not yield any updates as initial state.");
        } else {
          LOG4CPLUS_DEBUG(
              *logger_p,
              "The subscribe appears to have returned initial state "
              "to the update "
              "queue; fetching state from the update queue...");
        }
      }
    }
    // shutdown
    LOG4CPLUS_INFO(*logger_p, "Global_stop - Shutting down Performance TRC");

    LOG4CPLUS_DEBUG(
        *logger_p,
        "Try Unsubscribe trc - sometimes waits on new Block from TRS");
    trc_facade->Unsubscribe();
    LOG4CPLUS_INFO(*logger_p, "ThinReplicaClient unsubscribed.");

    // save all the collected data in file
    std::ofstream trc_time_file;
    std::string trc_time_file_path = "/concord/trc-trs-perf-results/trc" +
                                     std::string(1, client_id.back()) + ".csv";
    trc_time_file.open(trc_time_file_path);
    for (int i = 0; i < trc_time_vec.size(); i++) {
      trc_time_file << trc_time_vec[i];
    }
    trc_time_file.close();

    // get the metrics for each server
    for (auto server : servers) {
      LOG4CPLUS_INFO(
          *logger_p,
          "Latency and Throughput Calculation w.r.t. Server " << server);
      getResults(server, trc_time_file_path, latest_block_id,
                 num_times_to_read);
    }
  } catch (const std::exception& e) {
    LOG4CPLUS_ERROR(*logger_p, "Exception perf-trc:" << e.what());
    ret_status = -1;
  }

  // debug clean exit issues
  trc_facade.reset();
  LOG4CPLUS_DEBUG(*logger_p, "ThinReplicaClient reset.");
  LOG4CPLUS_DEBUG(*logger_p, "return " << std::to_string(ret_status));
  return ret_status;
}
