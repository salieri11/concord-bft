// Copyright 2019 VMware, all rights reserved

#include <grpcpp/grpcpp.h>
#include <log4cplus/configurator.h>
#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>
#include <iomanip>
#include "thin_replica_client.hpp"

using grpc::Channel;
using grpc::InsecureChannelCredentials;
using grpc::Status;
using log4cplus::Logger;
using std::cout;
using std::endl;
using std::exception;
using std::hex;
using std::pair;
using std::setfill;
using std::setw;
using std::shared_ptr;
using std::stoull;
using std::string;
using std::stringstream;
using std::to_string;
using std::unique_ptr;
using std::vector;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::Update;
using thin_replica_client::UpdateQueue;

int main(int argc, char** argv) {
  log4cplus::initialize();
  if (const char* log_properties = std::getenv("LOG4CPLUS_CONFIGURATION")) {
    log4cplus::PropertyConfigurator::doConfigure(log_properties);
  } else {
    log4cplus::BasicConfigurator cfg;
    cfg.configure();
  }

  Logger logger(Logger::getInstance("thin_replica.example"));

  static const size_t kNumServersIndex = 1;
  static const size_t kMaxFaultyIndex = 2;
  static const size_t kServersOffset = 3;
  string usage_text =
      "trc_example\n"
      "usage: trc_example <NUM_SERVERS> <MAX_FAULTY> <SERVERS...>\n"
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
  } else if (argc < kServersOffset) {
    LOG4CPLUS_ERROR(
        logger, "Too few arguments to trc_example (outputting usage text).");
    cout << usage_text << endl;
    return -1;
  }

  uint16_t num_servers;
  uint16_t max_faulty;

  bool has_num_servers = false;
  bool has_max_faulty = false;

  try {
    unsigned long long num_servers_raw = stoull(argv[kNumServersIndex]);
    num_servers = (uint16_t)num_servers_raw;
    has_num_servers =
        ((num_servers_raw > 0) && (num_servers_raw <= UINT16_MAX));
  } catch (const exception& e) {
    has_num_servers = false;
  }
  if (!has_num_servers) {
    LOG4CPLUS_FATAL(logger, "Invalid number of servers: \""
                                << argv[kNumServersIndex]
                                << "\". The number of servers is expected to "
                                   "be a positive integer in range ["
                                << to_string(1) << "," << to_string(UINT16_MAX)
                                << "].");
    return -1;
  }
  try {
    unsigned long long max_faulty_raw = stoull(argv[kMaxFaultyIndex]);
    max_faulty = (uint16_t)max_faulty_raw;
    has_max_faulty = ((max_faulty_raw >= 0) && (max_faulty_raw <= UINT16_MAX));
  } catch (const exception& e) {
    has_max_faulty = false;
  }
  if (!has_num_servers) {
    LOG4CPLUS_FATAL(
        logger, "Invalid maximum number of faulty servers: \""
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
    LOG4CPLUS_FATAL(logger,
                    "Insufficient number of servers ("
                        << to_string(num_servers)
                        << ") to accomodate maximum number of faulty servers("
                        << to_string(max_faulty)
                        << "). The number of servers must be at least (3 * "
                           "(max faulty servers) + 1).");
    return -1;
  }
  if ((size_t)argc != ((size_t)kServersOffset + (size_t)num_servers)) {
    LOG4CPLUS_FATAL(logger,
                    "Number of servers given ("
                        << to_string(num_servers)
                        << ") does not agree with number of network addresses "
                           "for servers provided in arguments (counted "
                        << to_string(argc - kServersOffset)
                        << " address arguments).");
    return -1;
  }

  shared_ptr<UpdateQueue> update_queue(new BasicUpdateQueue());
  string private_key;
  vector<pair<string, shared_ptr<Channel>>> servers;
  for (uint16_t i = 0; i < num_servers; ++i) {
    servers.push_back(pair<string, shared_ptr<Channel>>(
        string(),
        CreateChannel(argv[kServersOffset + i], InsecureChannelCredentials())));
  }

  int ret_status = 0;
  unique_ptr<ThinReplicaClient> trc;

  try {
    LOG4CPLUS_INFO(logger, "Attempting to construct ThinReplicaClient...");
    trc.reset(new ThinReplicaClient(update_queue, max_faulty, private_key,
                                    servers.begin(), servers.end()));
    LOG4CPLUS_INFO(logger, "ThinReplicaClient constructed.");
    trc->Subscribe("");
    LOG4CPLUS_INFO(logger, "ThinReplicaClient subscribed.");

    unique_ptr<Update> update = update_queue->TryPop();
    bool has_update = (bool)update;
    uint64_t latest_block_id = 0;
    if (!has_update) {
      LOG4CPLUS_INFO(
          logger,
          "Subscription call did not yield any updates as initial state.");
    }
    while (update) {
      vector<pair<string, string>>& update_data = update->kv_pairs;
      stringstream update_report;
      update_report << "ThinReplicaClient reported an update (Block ID: "
                    << update->block_id << ")." << endl;
      if (update_data.size() < 1) {
        update_report << "The update appears to be empty.";
      } else {
        update_report << "Update contains data for the following key(s) (not "
                         "displaying values for concision):";
        for (const auto& kv_pair : update_data) {
          update_report << endl << "  0x" << hex << setfill('0');
          for (size_t i = 0; i < kv_pair.first.length(); ++i) {
            update_report << setw(2) << (unsigned int)(kv_pair.first[i]);
          }
        }
      }
      LOG4CPLUS_INFO(logger, update_report.str());
      latest_block_id = update->block_id;
      update = update_queue->TryPop();
    }

    if (has_update) {
      trc->AcknowledgeBlockID(latest_block_id);
      LOG4CPLUS_INFO(logger, "Update(s) acknowledged.");
    }
    trc->Unsubscribe();
    LOG4CPLUS_INFO(logger, "ThinReplicaClient unsubscribed.");
  } catch (const exception& e) {
    LOG4CPLUS_ERROR(
        logger,
        "An exception occurred while trying to construct a ThinReplicaClient "
        "and connect it to the Thin Replica Server(s). Exception message:"
            << endl
            << e.what());
    ret_status = -1;
  }

  trc.reset();
  update_queue.reset();
  return ret_status;
}
