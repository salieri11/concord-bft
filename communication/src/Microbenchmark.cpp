// Concord
//
// Copyright (c) 2018-2020 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.
#include <string>
#include <variant>
#include <memory>
#include <chrono>
#include <mutex>
#include <thread>
#include <condition_variable>
#include <getopt.h>
#include <signal.h>
#include "communication/CommFactory.hpp"
#include "communication/CommDefs.hpp"
#include "communication/ICommunication.hpp"
#include "histogram.hpp"
#include "Logger.hpp"

using namespace bft::communication;
using namespace concordUtils;
using namespace std;

unique_ptr<ICommunication> server = nullptr;
unique_ptr<ICommunication> client = nullptr;
Histogram hist;
logging::Logger log = logging::getLogger("comm_microbenchmark");

const int client_wait = 3000;

int num_of_requests = 1000;
int req_size_bytes = 15000;
bool is_server = false;
int client_id = 0;
bool verbose = false;
char* request_bytes = nullptr;


mutex done_mutex;
condition_variable cv;
bool done = false;
auto logger = logging::getLogger("comm_microbench");

// Define the function to be called when ctrl-c (SIGINT) is sent to process
void signal_callback_handler(int signum) {
  cout << "Caught signal " << signum << endl;
  done = true;
  cv.notify_all();
  exit(signum);
}

class ServerReceiver : IReceiver {
  char* reply;
  // int count = 0;
 public:
  ServerReceiver() {
    reply = new char[100];
    for(int i =0; i < 100; i++)
      reply[i] = (char)(i+1);
  }

  ~ServerReceiver() {
    delete[] reply;
  }

  virtual void onConnectionStatusChanged(NodeNum node, ConnectionStatus newStatus) override {}
  virtual void onNewMessage(NodeNum sourceNode, const char *const message, size_t messageLength) override {
    // LOG_INFO(logger, "got " << messageLength << " bytes from " << sourceNode << ", total " << ++count);
    server->sendAsyncMessage(sourceNode, reply, 100);
  }
};

class ClientReceiver : IReceiver {
  int done_counter = 0;
 public:
  ulong receive_bytes = 0;
  virtual void onConnectionStatusChanged(NodeNum node, ConnectionStatus newStatus) override {}
  virtual void onNewMessage(NodeNum sourceNode, const char *const message, size_t messageLength) override {
    ++done_counter;
    receive_bytes += messageLength;
    if(verbose && done_counter % 1000 == 0)
      LOG_INFO(log, "total " << done_counter);
    LOG_DEBUG(log, "got " << messageLength << " bytes from " << sourceNode << ", total " << done_counter);
    if(done_counter == num_of_requests) {
      done = true;
      cv.notify_all();
    }
  }
};

void start_server() {
  server->Start();
  {
    unique_lock l(done_mutex);
    cv.wait(l, []() { return done; });
  }
  server->Stop();
}

ulong start_client() {
  client->Start();
  LOG_INFO(logger, "waiting for client to connect...");
  std::this_thread::sleep_for(chrono::milliseconds(client_wait));
  int count = 0;
  LOG_INFO(logger, "client connected...");
  ulong sent_bytes = 0;
  for(int i =0; i < num_of_requests; i++) {
    // LOG_INFO(log, "sending " << i);
    while(client->sendAsyncMessage(0, (const char*)request_bytes, req_size_bytes) != 0);
    //std::this_thread::sleep_for(chrono::milliseconds(1));
    count++;
    sent_bytes += req_size_bytes;
  }
  //std::this_thread::sleep_for(chrono::milliseconds(10));
  LOG_INFO(log, "sent " << count);
  {
    unique_lock l(done_mutex);
    cv.wait(l, []() { return done; });
  }

  return sent_bytes;
}

void parse_args(int argc, char** argv) {
  static struct option longOptions[] = {{"num_of_requests", required_argument, nullptr, 'r'},
                                        {"req_size", required_argument, nullptr, 's'},
                                        {"mode", required_argument, nullptr, 'm'},
                                        {"verbose", required_argument, nullptr, 'v'},
                                        {"client_id", required_argument, nullptr, 'c'},
                                        {nullptr, 0, nullptr, 0}};
  int optionIndex = 0;
  int option = 0;
  while ((option = getopt_long(argc, argv, "r:s:m:v:c:", longOptions, &optionIndex)) != -1) {
    switch (option) {
      case 'r': {
        auto t = stoi(string(optarg));
        if (t > 0) num_of_requests = t;
        break;
      }
      case 's': {
        auto t = stoi(string(optarg));
        if (t > 0) req_size_bytes = t;
        break;
      }
      case 'm': {
        string t = string(optarg);
        if (t == "server")
          is_server = true;
        break;
      }
      case 'v': {
          verbose = true;
          break;
      }
      case 'c':
      {
        auto t = stoi(string(optarg));
        if (t > 0) client_id = t;
      }
      default: {

      }
    }
  }
}

int main(int argc, char** argv) {
  parse_args(argc, argv);
  string cipherSuite = "ECDHE-ECDSA-AES256-GCM-SHA384";
  NodeMap nodes;
  nodes[0] = NodeInfo{"127.0.0.1", 3501, true};
  nodes[1] = NodeInfo{"127.0.0.1", 3501, false};
  TlsTcpConfig conf_server = TlsTcpConfig
      ("0.0.0.0", 3501, 64000, nodes,0, 0, "certs", cipherSuite);
  TlsTcpConfig conf_client = TlsTcpConfig
      ("0.0.0.0", 3501, 64000, nodes,0, client_id, "certs", cipherSuite);

  if(is_server) {
    server = unique_ptr<TlsTCPCommunication>(TlsTCPCommunication::create(conf_server));
    server->setReceiver(0, (IReceiver*)new ServerReceiver());
    start_server();
  } else {
    request_bytes = new char[req_size_bytes];
    for(int i = 0; i < req_size_bytes; i++)
      request_bytes[i] = (char)(i+1);
    client = unique_ptr<TlsTCPCommunication>(TlsTCPCommunication::create(conf_client));
    auto receiver  = new ClientReceiver();
    client->setReceiver(1, (IReceiver*)receiver);
    auto start = chrono::steady_clock::now();
    auto sent_bytes = start_client();
    auto end = chrono::steady_clock::now();
    client->Stop();
    auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count() - client_wait;
    double tp = (double)num_of_requests / dur * 1000.0;
    ulong tp1 =  sent_bytes / dur * 1000;
    cout << "Total: " << num_of_requests << " requests, bytes sent: "
         << sent_bytes << ", bytes received: " << receiver->receive_bytes << ", dur: " << dur << " ms" << endl;
    cout << "TP: " << tp << " req/sec" << endl;
    cout << "TP: " << tp1<< " bytes/sec" << endl;
  }
}