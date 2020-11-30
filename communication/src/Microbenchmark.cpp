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
#include <iostream>
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

typedef std::shared_ptr<ICommunication> COMM_PTR;

unique_ptr<ICommunication> server = nullptr;

Histogram hist;
const int client_wait = 3000;

uint receive_counter = 0;
uint64_t received_bytes = 0;
uint num_of_requests = 1000;
int req_size_bytes = 15000;
bool is_server = false;
int first_client_id = 1;
int num_of_clients = 1;
bool verbose = false;
char* request_bytes = nullptr;

mutex client_done_mutex;
condition_variable client_cv;
mutex server_done_mutex;
condition_variable server_cv;
bool done = false;
auto log = logging::getLogger("comm_microbench");

// Define the function to be called when ctrl-c (SIGINT) is sent to process
void signal_callback_handler(int signum) {
  cout << "Caught signal " << signum << endl;
  done = true;
  if(is_server)
    server_cv.notify_all();
  else
    client_cv.notify_all();

  exit(signum);
}

class ServerReceiver : IReceiver {
  char* reply;
  int count = 0;
 public:
  ServerReceiver() {
    reply = new char[req_size_bytes];
    for (int i = 0; i < req_size_bytes; i++) reply[i] = (char)(i + 1);
  }

  ~ServerReceiver() { delete[] reply; }

  virtual void onConnectionStatusChanged(NodeNum node, ConnectionStatus newStatus) override {}
  virtual void onNewMessage(NodeNum sourceNode, const char* const message, size_t messageLength) override {
    ++count;
    if (verbose && count > 0 && count % 1000 == 0) LOG_INFO(log, "total " << count);
    LOG_DEBUG(log, "got " << messageLength << " bytes from " << sourceNode << ", total " << count);
    while(server->sendAsyncMessage(sourceNode, reply, req_size_bytes) != 0);
  }
};

class ClientReceiver : public IReceiver {
 public:
  virtual void onConnectionStatusChanged(NodeNum node, ConnectionStatus newStatus) override {}
  virtual void onNewMessage(NodeNum sourceNode, const char* const message, size_t messageLength) override {
    {
      unique_lock<mutex> ul(client_done_mutex);
      ++receive_counter;
      received_bytes += messageLength;
      if (receive_counter == num_of_requests * num_of_clients) {
        done = true;
    }
    if(done)
      client_cv.notify_all();

    if (verbose && receive_counter > 0 && receive_counter % 1000 == 0) LOG_INFO(log, "total " << receive_counter);
    LOG_DEBUG(log, "got " << messageLength << " bytes from " << sourceNode << ", total " << receive_counter);
    }
  }
};

void start_server() {
  server->Start();
  {
    unique_lock l(server_done_mutex);
    server_cv.wait(l, []() { return done; });
  }
  server->Stop();
}

void start_client(const COMM_PTR&& client, uint&& client_id) {
  client->Start();
  LOG_INFO(log, "waiting for client to connect...");
  std::this_thread::sleep_for(chrono::milliseconds(client_wait));
  LOG_INFO(log, "client connected...");
  uint sent_counter = 0;
  while (sent_counter < num_of_requests) {
    // LOG_INFO(log, "sending " << i);
    while (client->sendAsyncMessage(0, (const char*)request_bytes, req_size_bytes) != 0);
    ++sent_counter;
    // std::this_thread::sleep_for(chrono::milliseconds(1));
  }
  LOG_INFO(log, "client " << client_id << " done sending " << sent_counter << " requests");
  // std::this_thread::sleep_for(chrono::milliseconds(10));
  {
    unique_lock<mutex> ul(client_done_mutex);
    client_cv.wait(ul, [&](){
      return done;
    });
  }
}

void parse_args(int argc, char** argv) {
  static struct option longOptions[] = {{"num_of_requests", required_argument, nullptr, 'r'},
                                        {"req_size", required_argument, nullptr, 's'},
                                        {"mode", required_argument, nullptr, 'm'},
                                        {"verbose", required_argument, nullptr, 'v'},
                                        {"first_client_id", required_argument, nullptr, 'c'},
                                        {"num_of_clients", required_argument, nullptr, 'n'},
                                        {nullptr, 0, nullptr, 0}};
  int optionIndex = 0;
  int option = 0;
  while ((option = getopt_long(argc, argv, "r:s:m:v:c:n:", longOptions, &optionIndex)) != -1) {
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
        if (t == "server") is_server = true;
        break;
      }
      case 'v': {
        verbose = true;
        break;
      }
      case 'c': {
        auto t = stoi(string(optarg));
        if (t > 0) first_client_id = t;
      }
      case 'n': {
        auto t = stoi(string(optarg));
        if (t > 0) num_of_clients = t;
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
  for (int i = 0; i < num_of_clients; ++i) {
    nodes[first_client_id + i] = NodeInfo{"127.0.0.1", 3501, false};
  }

  TlsTcpConfig conf_server = TlsTcpConfig("0.0.0.0", 3501, 64000, nodes, 0, 0, "certs", cipherSuite);

  vector<COMM_PTR> clients;
  vector<std::thread> threads;
  threads.reserve(num_of_clients);
  clients.reserve(num_of_clients);

  if (is_server) {
    server = unique_ptr<TlsTCPCommunication>(TlsTCPCommunication::create(conf_server));
    server->setReceiver(0, (IReceiver*)new ServerReceiver());
    start_server();
  } else {
    request_bytes = new char[req_size_bytes];
    for (int i = 0; i < req_size_bytes; i++) request_bytes[i] = (char)(i + 1);

    auto start = chrono::steady_clock::now();
    for (int i = 0; i < num_of_clients; ++i) {
      auto conf_client = TlsTcpConfig(
          "0.0.0.0", 3501, 64000, nodes,
          0, first_client_id + i, "certs", cipherSuite);
      clients.emplace_back(TlsTCPCommunication::create(conf_client));
      auto receiver = new ClientReceiver();
      clients.back()->setReceiver(i, receiver);
      threads.emplace_back(start_client, clients.back(), i);
    }
    for (auto& t : threads) {
      t.join();
    }

    for(auto &t : clients)
      t->Stop();

    threads.clear();
    clients.clear();

    delete[] request_bytes;
    auto end = chrono::steady_clock::now();
    auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count() - client_wait;
    double tp = (double)num_of_requests * num_of_clients / dur * 1000.0;
    ulong tp1 = num_of_requests * req_size_bytes * num_of_clients / dur * 1000;
    cout << "Total: " << num_of_requests * num_of_clients << " requests, bytes sent: " << num_of_requests * req_size_bytes * num_of_clients
         << ", bytes received: " << received_bytes << ", dur: " << dur << " ms" << endl;
    cout << "TP: " << tp << " req/sec" << endl;
    cout << "TP: " << tp1 << " bytes/sec" << endl;
    exit(0);
  }
}