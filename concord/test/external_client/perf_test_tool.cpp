// Copyright 2020 VMware, all rights reserved

// This tool uses Concord External Client Pool object to interact
// with PEE. The tool will be replaced by integrating concord pool option to the
// performance python script.
// The configuration file, external_client_tls_20.config, can be found under
// concord/test/resources

#include <getopt.h>
#include <grpcpp/grpcpp.h>
#include <log4cplus/configurator.h>
#include <log4cplus/loglevel.h>
#include <chrono>
#include <ctime>
#include <memory>
#include <mutex>
#include <random>
#include <unordered_map>
#include "Logger.hpp"
#include "concord.pb.h"
#include "concord_client_pool.hpp"
#include "grpcpp/impl/codegen/status.h"
#include "histogram.hpp"
#include "performance.grpc.pb.h"
#include "performance.pb.h"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::PerfRequest;
using com::vmware::concord::PerfResponse;
using com::vmware::concord::performance::PerfCleanRequest;
using com::vmware::concord::performance::PerfCleanResponse;
using com::vmware::concord::performance::PerfInitRequest;
using com::vmware::concord::performance::PerfInitResponse;
using com::vmware::concord::performance::PerfWriteRequest;
using com::vmware::concord::performance::PerfWriteResponse;

using namespace std;
using namespace bftEngine;
using namespace concord::concord_client_pool;
using namespace concordUtils;
using namespace google::protobuf;

bool printReqDurations = true;
uint32_t numOfBlocks = 4000000;
auto flags = ClientMsgFlag::EMPTY_FLAGS_REQ;
uint32_t numOfKvs = 151;
uint32_t keySize = 76;
uint32_t valSize = 502;
uint32_t payloadSize = 15100;
uint32_t concurrencyLevel = 16;

const float kWarmUpPerc = 0.02;
const string kPerfServiceHost = "127.0.0.1:50051";
const string poolConfigPath = "external_client_tls_20.config";
const log4cplus::LogLevel kLogLevel = log4cplus::ERROR_LOG_LEVEL;

bool done = false;
chrono::steady_clock::time_point globalStart;
chrono::steady_clock::time_point globalEnd;
uint32_t actualRequests = 0;

struct ReqData {
  chrono::steady_clock::time_point startTime;
  ConcordRequest* req = nullptr;
  char* replyData = nullptr;
  uint32_t replyDataSize = 0;
  string request;

  ReqData(chrono::steady_clock::time_point start, ConcordRequest* cr,
          char* reply, uint32_t replyLength)
      : startTime{start},
        req{cr},
        replyData{reply},
        replyDataSize{replyLength} {}

  ReqData() = default;

  ~ReqData() {
    ;
    delete[] replyData;
    delete req;
  }
};

unordered_map<string, ReqData*> active_requests_data;
int activeRequests = 0;
mutex reqSynch;
condition_variable reqSignal;
auto hist = Histogram();
vector<pair<string, long>> durations;

void print_results() {
  auto hstring = hist.ToString();
  auto dur =
      chrono::duration_cast<chrono::milliseconds>(globalEnd - globalStart)
          .count();

  auto tp = (double)actualRequests / dur * 1000;
  cout << "Done. Total requests: " << numOfBlocks << "." << endl
       << "Duration: " << dur << " ms."
       << " for " << actualRequests << " requests."
       << " Actual TP: " << tp << " tps." << endl
       << hstring << endl;

  if (printReqDurations) {
    cout << "*******************" << endl;
    cout << "Requests durations:" << endl;
    cout << "cid, duration" << endl;
    for (auto& t : durations) cout << t.first << "," << t.second << endl;
  }
}

static void signalHandler(int signum) {
  globalEnd = chrono::steady_clock::now();
  if (!done) {
    done = true;
    print_results();
  }
  signal(signum, SIG_DFL);
  raise(signum);
}

void req_callback(const uint64_t& sn, const string& cid, uint32_t replySize) {
  ReqData* reqData = nullptr;
  static int count = 0;
  static auto logger = logging::getLogger("callback");

  {
    lock_guard<mutex> l(reqSynch);
    reqData = active_requests_data[cid];
    if (!reqData) cout << "BADDDDD " << cid << endl;
    assert(reqData);
    --activeRequests;
    active_requests_data.erase(cid);
  }
  reqSignal.notify_one();
  LOG_DEBUG(logger, "finished cid: " << cid);

  auto start = reqData->startTime;
  auto end = chrono::steady_clock::now();
  auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count();

  ++count;
  if ((float)count >= (float)(numOfBlocks / 100.0 * kWarmUpPerc)) {
    hist.Add(dur);
    actualRequests++;
    durations.emplace_back(cid, dur);
  }

  LOG_DEBUG(logger, "Got " << count << " response with cid: " << cid);

  ConcordResponse cr;
  cr.ParseFromString(reqData->replyData);
  assert(cr.has_perf_response());
  PerfResponse pr = cr.perf_response();
  assert(pr.has_response_content());
  PerfWriteResponse pwr;
  pwr.ParseFromString(pr.response_content());
  if (pwr.message() != "OK")
    LOG_ERROR(logger, "request failed, response msg: " << pwr.message());
  else
    LOG_DEBUG(logger, "response msg: " << pwr.message() << ", new blockID: "
                                       << pwr.new_block_id() << ", "
                                       << pr.response_content());
}

void do_preloaded_test(log4cplus::Logger& logger, ConcordClientPool* pool) {
  grpc::ChannelArguments chArgs;
  auto perfServiceStub =
      com::vmware::concord::performance::PerformanceService::NewStub(
          grpc::CreateCustomChannel(kPerfServiceHost,
                                    grpc::InsecureChannelCredentials(), chArgs))
          .release();

  // init perf handler
  PerfInitRequest initReq;
  initReq.set_block_count(numOfBlocks);
  initReq.set_kv_count(numOfKvs);
  initReq.set_key_size(keySize);
  initReq.set_value_size(valSize);
  grpc::ClientContext context;

  PerfInitResponse initResp;
  grpc::Status status = perfServiceStub->PerfInit(&context, initReq, &initResp);

  assert(status.ok());
  LOG_INFO(logger, "perf data id: " << initResp.id() << ", set info: "
                                    << initResp.workset_info());
  // return 0;
  hist.Clear();

  using namespace ::com::vmware::concord::performance;

  char payload[payloadSize];
  for (int i = 0; i < payloadSize; i++) payload[i] = (char)i;

  for (int i = 0; i < numOfBlocks; i++) {
    if (done) break;
    {
      unique_lock<mutex> l(reqSynch);
      while (activeRequests == concurrencyLevel) reqSignal.wait(l);
      activeRequests++;
    }

    auto pWriteReq = new PerfWriteRequest();
    auto fromInit = pWriteReq->mutable_from_init();
    fromInit->set_init_id(initResp.id());
    fromInit->set_block_id(i);

    string pl = string{payload, payloadSize};
    pWriteReq->set_payload(pl.c_str(), payloadSize);

    string cid = "block_" + to_string(i);
    auto* cr = new ConcordRequest();
    PerfRequest* pr = cr->mutable_perf_request();
    string s;
    pWriteReq->SerializeToString(&s);
    pr->set_request_content(s);
    pr->set_type(com::vmware::concord::PerfRequest_PerfRequestType_Write);

    uint32_t replyLength = 16000000;
    char* reply = new char[16000000];
    auto reqData =
        new ReqData(chrono::steady_clock::now(), cr, reply, replyLength);
    cr->SerializeToString(&reqData->request);
    assert(cr->has_perf_request());

    LOG_DEBUG(logger, "sending cid " << cid);

    {
      lock_guard<mutex> l(reqSynch);
      active_requests_data[cid] = reqData;
    }

    auto res = pool->SendRequest(
        vector<char>{reqData->request.c_str(),
                     reqData->request.c_str() + reqData->request.size()},
        flags, chrono::milliseconds::max(), reply, replyLength, cid);
    assert(res == SubmitResult::Acknowledged);
  }
}

void do_on_fly_test(log4cplus::Logger& logger, ConcordClientPool* pool) {
  hist.Clear();

  using namespace ::com::vmware::concord::performance;

  char payload[payloadSize];
  for (int i = 0; i < payloadSize; i++) payload[i] = (char)i;

  std::random_device rd;
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<uint32_t> distr;

  for (int i = 1; i <= numOfBlocks; i++) {
    if (done) break;

    {
      unique_lock<mutex> l(reqSynch);
      while (activeRequests == concurrencyLevel) reqSignal.wait(l);
      activeRequests++;
    }

    auto pWriteReq = new PerfWriteRequest();
    auto fromExternal = pWriteReq->mutable_external();
    uint32_t keyPrefix = distr(eng);
    uint32_t valPrefix = distr(eng);
    fromExternal->set_key_prefix(keyPrefix);
    fromExternal->set_val_prefix(valPrefix);
    fromExternal->set_kv_count(numOfKvs);
    fromExternal->set_key_size(keySize);
    fromExternal->set_value_size(valSize);
    fromExternal->set_max_exec_time_milli(0);

    string pl = string{payload, payloadSize};
    pWriteReq->set_payload(pl.c_str(), payloadSize);

    string cid = "block_" + to_string(i);
    auto* cr = new ConcordRequest();
    PerfRequest* pr = cr->mutable_perf_request();
    string s;
    pWriteReq->SerializeToString(&s);
    pr->set_request_content(s);
    pr->set_type(com::vmware::concord::PerfRequest_PerfRequestType_Write);

    uint32_t replyLength = 16000000;
    char* reply = new char[16000000];
    auto reqData =
        new ReqData(chrono::steady_clock::now(), cr, reply, replyLength);
    cr->SerializeToString(&reqData->request);
    assert(cr->has_perf_request());

    LOG_DEBUG(logger, "sending cid " << cid);

    {
      lock_guard<mutex> l(reqSynch);
      active_requests_data[cid] = reqData;
    }

    auto res = pool->SendRequest(
        vector<char>{reqData->request.c_str(),
                     reqData->request.c_str() + reqData->request.size()},
        flags, chrono::milliseconds::max(), reply, replyLength, cid);
    assert(res == SubmitResult::Acknowledged);
  }
}

void show_help(char** argv) {
  LOG_INFO(
      GL,
      "Command line options: \n"
          << " -b NBR - a number of requests to launch (default: 4000000) \n"
          << " -p 1/0 - requests pre-processing on/off (default: off) \n"
          << " -k NBR - number of keys (default: 151) \n"
          << " -s NBR - single key size (default: 76) \n"
          << " -v NBR - key value size (default: 502) \n"
          << " -d NBR - payload size (default: 15100) \n"
          << " -c NBR - concurrency level: capacity of the clients pool "
             "(default: 16) \n"
          << " -i 1/0 - to print or not requests durations (default: true)");
}

bool parse_args(int argc, char** argv) {
  try {
    static struct option longOptions[] = {
        {"num_of_blocks", required_argument, nullptr, 'b'},
        {"pre_process_requests", required_argument, nullptr, 'p'},
        {"num_of_keys", required_argument, nullptr, 'k'},
        {"key_size", required_argument, nullptr, 's'},
        {"key_value_size", required_argument, nullptr, 'v'},
        {"payload_size", required_argument, nullptr, 'd'},
        {"concurrency_level", required_argument, nullptr, 'c'},
        {"print_req_durations", required_argument, nullptr, 'i'},
        {nullptr, 0, nullptr, 0}};
    int optionIndex = 0;
    int option = 0;
    while ((option = getopt_long(argc, argv, "b:p:k:s:v:d:c:i:", longOptions,
                                 &optionIndex)) != -1) {
      switch (option) {
        case 'b': {
          auto blocks = stoi(string(optarg));
          if (blocks > 0) numOfBlocks = blocks;
          break;
        }
        case 'p': {
          if (stoi(string(optarg)) == 0)
            flags = ClientMsgFlag::EMPTY_FLAGS_REQ;
          else
            flags = ClientMsgFlag::PRE_PROCESS_REQ;
          break;
        }
        case 'k': {
          auto num_of_keys = stoi(string(optarg));
          if (num_of_keys > 0) numOfKvs = num_of_keys;
          break;
        }
        case 's': {
          auto key_size = stoi(string(optarg));
          if (key_size > 0) keySize = key_size;
          break;
        }
        case 'v': {
          auto key_value_size = stoi(string(optarg));
          if (key_value_size > 0) valSize = key_value_size;
          break;
        }
        case 'd': {
          auto payload_size = stoi(string(optarg));
          if (payload_size > 0) payloadSize = payload_size;
          break;
        }
        case 'c': {
          auto concurrency_level = stoi(string(optarg));
          if (concurrency_level > 0) concurrencyLevel = concurrency_level;
          break;
        }
        case 'i': {
          printReqDurations = stoi(string(optarg)) != 0;
          break;
        }
        default:
          return false;
      }
    }
    return true;
  } catch (const std::exception& e) {
    LOG_FATAL(GL, "Failed to parse command line arguments: " << e.what());
    return false;
  }
}

int main(int argc, char** argv) {
  if (!parse_args(argc, argv)) {
    show_help(argv);
    return 1;
  }

  signal(SIGTERM, signalHandler);
  signal(SIGINT, signalHandler);

  log4cplus::initialize();
  log4cplus::BasicConfigurator config;
  config.configure();
  log4cplus::Logger logger = logging::getLogger("test");
  logger.getRoot().setLogLevel(kLogLevel);
  auto pool = new ConcordClientPool(poolConfigPath);
  pool->SetDoneCallback(req_callback);
  parse_args(argc, argv);

  durations.reserve(numOfBlocks);

  // wait for the pool to connect
  this_thread::sleep_for(chrono::seconds(5));

  globalStart = chrono::steady_clock::now();
  // do_preloaded_test(logger, pool);
  do_on_fly_test(logger, pool);

  {
    unique_lock<mutex> l(reqSynch);
    while (activeRequests > 0) reqSignal.wait(l);
  }
  assert(active_requests_data.empty());
  globalEnd = chrono::steady_clock::now();

  delete pool;
  print_results();
}