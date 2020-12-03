// Copyright 2020 VMware, all rights reserved

// This tool uses Concord External Client Pool object to interact
// with PEE. The tool will be replaced by integrating concord pool option to the
// performance python script.
// The configuration file, external_client_tls_20.config, can be found under
// concord/test/resources

#include <getopt.h>
#include <grpcpp/grpcpp.h>
#include <sys/resource.h>
#include <chrono>
#include <memory>
#include <mutex>
#include <random>
#include <string>
#include <unordered_map>
#include "Logger.hpp"
#include "concord.pb.h"
#include "concord_client_pool.hpp"
#include "grpcpp/impl/codegen/status.h"
#include "histogram.hpp"
#include "kvstream.h"
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

bool printReqDurations = false;
uint32_t numOfBlocks = 10000;
auto flags = ClientMsgFlag::EMPTY_FLAGS_REQ;
uint32_t concurrencyLevel = 15;
bool busyWait = true;
uint32_t commBufferLength = 64000;
uint initialWaitMs = 10;
uint maxWaitTimes = 500;
uint loaderConcurrencyLevel = 30;
uint loaderNumOfWorkers = 4;
uint timeOut = 30000;
string testCase = "TwoLegsClosed";
uint waitMethodParam = 0;
std::function<void(ConcordClientPool*)> waitMethod = nullptr;

// first leg
uint32_t numOfKvs = 9;
uint32_t keySize = 118;
uint32_t valSize = 1200;
uint32_t payloadSize = 10000;
uint32_t executionTime = 0;
// second leg
uint32_t numOfKvs2 = 21;
uint32_t keySize2 = 118;
uint32_t valSize2 = 3500;
uint32_t payloadSize2 = 10000;
uint32_t executionTime2 = 0;

atomic<uint> expired = 0;
const float kWarmUpPerc = 0.02;
const string kPerfServiceHost = "127.0.0.1:50051";
string poolConfigPath = "external_client_tls.config";

string log_properties_file = "log4cplus.properties";
uint log_level = 0;  // info

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
  std::function<void(std::shared_ptr<ReqData>)> callback = nullptr;
  std::string cid;

  ReqData(chrono::steady_clock::time_point start, ConcordRequest* cr,
          char* reply, uint32_t replyLength, string& cid_)
      : startTime{start},
        req{cr},
        replyData{reply},
        replyDataSize{replyLength},
        cid{cid_} {}

  ReqData() = default;

  ~ReqData() {
    delete[] replyData;
    delete req;
  }
};

unordered_map<string, std::shared_ptr<ReqData>> active_requests_data;
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
       << "Expired requests: " << expired << endl
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
  std::shared_ptr<ReqData> reqData = nullptr;
  static int count = 0;
  static auto logger = logging::getLogger("perf-test-tool.callback");

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

  if (reqData->callback)
    reqData->callback(reqData);
  else {
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
                                         << pwr.new_block_id());
  }
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

  std::string payload(payloadSize, 0);
  std::iota(payload.begin(), payload.end(), 0);

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

    pWriteReq->set_payload(payload.c_str(), payloadSize);

    string cid = "block_" + to_string(i);
    auto* cr = new ConcordRequest();
    PerfRequest* pr = cr->mutable_perf_request();
    string s;
    pWriteReq->SerializeToString(&s);
    pr->set_request_content(s);
    pr->set_type(com::vmware::concord::PerfRequest_PerfRequestType_Write);

    uint32_t replyLength = commBufferLength;
    char* reply = new char[commBufferLength];
    auto reqData = std::make_shared<ReqData>(chrono::steady_clock::now(), cr,
                                             reply, replyLength, cid);
    cr->SerializeToString(&reqData->request);
    assert(cr->has_perf_request());

    LOG_DEBUG(logger, "sending cid " << cid);

    {
      lock_guard<mutex> l(reqSynch);
      active_requests_data[cid] = reqData;
    }

    auto res = pool->SendRequest(
        vector<uint8_t>{reqData->request.c_str(),
                        reqData->request.c_str() + reqData->request.size()},
        flags, chrono::milliseconds::max(), reply, replyLength, 0, cid);
    assert(res == SubmitResult::Acknowledged);
  }
}

void wait_with_callback(ConcordClientPool* pool) {
  {
    unique_lock<mutex> l(reqSynch);
    assert(activeRequests <= concurrencyLevel);
    reqSignal.wait(l, [&] {
      if (activeRequests == concurrencyLevel) {
        // cout << " waiting ..." << endl;
        return false;
      }
      return true;
    });
    ++activeRequests;
  }
}

void wait_const_time(ConcordClientPool* pool) {
  auto count = 0;
  while (PoolStatus::NotServing == pool->HealthStatus()) {
    this_thread::sleep_for(std::chrono::milliseconds(initialWaitMs));
    ++count;
    if (count == maxWaitTimes) assert(false);
  }

  {
    unique_lock<mutex> l(reqSynch);
    ++activeRequests;
  }
}

void send_request(
    log4cplus::Logger& logger, ConcordClientPool* pool, uint32_t& keyPrefix,
    uint32_t& valPrefix, uint32_t& numOfKvs_, uint32_t& keySize_,
    uint32_t& valSize_, uint32_t& executionTime_, string& payload, string& cid,
    chrono::steady_clock::time_point& time,
    std::function<void(std::shared_ptr<ReqData> reqData)> callback = nullptr) {
  waitMethod(pool);

  auto pWriteReq = new PerfWriteRequest();
  auto fromExternal = pWriteReq->mutable_external();
  fromExternal->set_key_prefix(keyPrefix);
  fromExternal->set_val_prefix(valPrefix);
  fromExternal->set_kv_count(numOfKvs_);
  fromExternal->set_key_size(keySize_);
  fromExternal->set_value_size(valSize_);
  fromExternal->set_max_exec_time_milli(executionTime_);
  fromExternal->set_busy_wait(busyWait);
  pWriteReq->set_payload(payload.c_str(), payload.size());

  auto* cr = new ConcordRequest();
  PerfRequest* pr = cr->mutable_perf_request();
  string s;
  pWriteReq->SerializeToString(&s);
  pr->set_request_content(s);
  pr->set_type(com::vmware::concord::PerfRequest_PerfRequestType_Write);

  uint32_t replyLength = commBufferLength;
  char* reply = new char[commBufferLength];
  auto reqData = new ReqData(time, cr, reply, replyLength, cid);
  reqData->callback = callback;

  cr->SerializeToString(&reqData->request);
  assert(cr->has_perf_request());

  LOG_DEBUG(logger, "sending cid " << cid);

  {
    lock_guard<mutex> l(reqSynch);
    active_requests_data.emplace(cid, reqData);
  }

  auto res = pool->SendRequest(
      vector<uint8_t>{reqData->request.c_str(),
                      reqData->request.c_str() + reqData->request.size()},
      flags, std::chrono::milliseconds(timeOut), reply, replyLength, 0, cid);
  assert(res == SubmitResult::Acknowledged);
}

void do_close_model_trades_test(log4cplus::Logger& logger,
                                ConcordClientPool* pool) {
  using namespace ::com::vmware::concord::performance;
  hist.Clear();

  std::string payload(payloadSize, 0);
  std::iota(payload.begin(), payload.end(), 0);
  std::string payload2(payloadSize2, 0);
  std::iota(payload2.begin(), payload2.end(), 0);

  std::random_device rd;
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<uint32_t> distr(1, 4294967295);

  mutex m;
  condition_variable cv;
  unordered_map<string, chrono::steady_clock::time_point>
      pending_requests_times;
  queue<pair<string, chrono::steady_clock::time_point>> ready_requests;
  std::function<void(std::shared_ptr<ReqData> &&)> callback =
      [&](std::shared_ptr<ReqData>&& reqData) {
        m.lock();
        ready_requests.emplace(reqData->cid, chrono::steady_clock::now());
        m.unlock();
        cv.notify_one();
      };

  uint blocksDone = 0;
  uint blocksSent = 0;

  vector<std::thread> threads;
  threads.reserve(loaderNumOfWorkers);
  vector<Histogram> firstLegHistograms;
  firstLegHistograms.reserve(loaderNumOfWorkers);
  vector<Histogram> secondLegHistograms;
  secondLegHistograms.reserve(loaderNumOfWorkers);
  vector<Histogram> totalHistograms;
  totalHistograms.reserve(loaderNumOfWorkers);

  for (int i = 0; i < loaderNumOfWorkers; ++i) {
    firstLegHistograms.emplace_back().Clear();
    secondLegHistograms.emplace_back().Clear();
    totalHistograms.emplace_back().Clear();

    threads.emplace_back(
        [&](Histogram* first, Histogram* second, Histogram* total, int&& id) {
          LOG_INFO(logger, "worker " << id << " started");

          string cid;
          string block_id;
          uint leg;
          chrono::steady_clock::time_point end;
          while (true) {
            {
              unique_lock<mutex> ul(m);
              if (ready_requests.empty()) {
                cv.wait(ul, [&] {
                  if (blocksDone < numOfBlocks) return !ready_requests.empty();
                  return true;
                });
              }
              if (blocksDone >= numOfBlocks) {
                cout << "worker " << id << ", notifying all: " << blocksDone
                     << endl;
                cv.notify_all();
                break;
              }
              cid = ready_requests.front().first;
              end = ready_requests.front().second;
              block_id = cid.substr(8, cid.size() - 8);
              leg = stoi(cid.substr(0, 1));
              ready_requests.pop();
            }

            auto legStart = std::chrono::steady_clock::now();
            if (leg == 1) {
              auto dur = chrono::duration_cast<chrono::milliseconds>(
                             end - pending_requests_times[cid])
                             .count();

              if (dur < timeOut) {
                first->Add(dur);
                // send second leg
                auto keyPrefix = distr(eng);
                auto valPrefix = distr(eng);
                cid = "2_block_" + block_id;
                pending_requests_times[cid] = legStart;
                send_request(logger, pool, keyPrefix, valPrefix, numOfKvs2,
                             keySize2, valSize2, executionTime2, payload2, cid,
                             legStart, callback);
              } else {
                ++expired;
                pending_requests_times[cid] = legStart;
                uint32_t keyPrefix = distr(eng);
                uint32_t valPrefix = distr(eng);
                send_request(logger, pool, keyPrefix, valPrefix, numOfKvs,
                             keySize, valSize, executionTime, payload, cid,
                             legStart, callback);
              }
            } else {
              auto dur = chrono::duration_cast<chrono::milliseconds>(
                             end - pending_requests_times[cid])
                             .count();
              if (dur < timeOut) second->Add(dur);
              auto trade_dur =
                  chrono::duration_cast<chrono::milliseconds>(
                      end - pending_requests_times[cid.replace(0, 1, "1")])
                      .count();
              if (trade_dur < timeOut) {
                total->Add(trade_dur);
                bool send = false;
                {
                  lock_guard lg{m};
                  ++blocksDone;
                  if (trade_dur < timeOut) ++actualRequests;
                  if (blocksSent < numOfBlocks) {
                    send = true;
                    cid = "1_block_" + to_string(blocksSent++);
                    pending_requests_times[cid] = legStart;
                  }
                }
                if (send) {
                  uint32_t keyPrefix = distr(eng);
                  uint32_t valPrefix = distr(eng);
                  send_request(logger, pool, keyPrefix, valPrefix, numOfKvs,
                               keySize, valSize, executionTime, payload, cid,
                               legStart, callback);
                }
              } else {
                ++expired;
                pending_requests_times[cid] = legStart;
                uint32_t keyPrefix = distr(eng);
                uint32_t valPrefix = distr(eng);
                send_request(logger, pool, keyPrefix, valPrefix, numOfKvs,
                             keySize, valSize, executionTime, payload, cid,
                             legStart, callback);
              }
            }
          }
          LOG_INFO(logger, "worker " << id << " finished");
        },
        &firstLegHistograms[i], &secondLegHistograms[i], &totalHistograms[i],
        i);
  }

  for (; blocksSent < loaderConcurrencyLevel; ++blocksSent) {
    uint32_t keyPrefix = distr(eng);
    uint32_t valPrefix = distr(eng);
    string cid = "1_block_" + to_string(blocksSent);
    auto tradeStart = std::chrono::steady_clock::now();
    pending_requests_times[cid] = tradeStart;
    send_request(logger, pool, keyPrefix, valPrefix, numOfKvs, keySize, valSize,
                 executionTime, payload, cid, tradeStart, callback);
  }

  for (auto&& tt : threads) tt.join();

  Histogram first;
  first.Clear();
  Histogram second;
  second.Clear();
  for (auto&& h : firstLegHistograms) first.Merge(h);
  for (auto&& h : secondLegHistograms) second.Merge(h);
  for (auto&& h : totalHistograms) hist.Merge(h);

  cout << "First leg:" << endl
       << first.ToString() << endl
       << "Second leg:" << endl
       << second.ToString() << endl;
}

void do_on_fly_test(log4cplus::Logger& logger, ConcordClientPool* pool) {
  hist.Clear();

  using namespace ::com::vmware::concord::performance;

  std::string payload(payloadSize, 0);
  std::iota(payload.begin(), payload.end(), 0);

  std::random_device rd;
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<uint32_t> distr(1, 4294967295);

  uint count = 0;
  auto callback = [&](std::shared_ptr<ReqData>&& reqData) {
    auto start = reqData->startTime;
    auto end = chrono::steady_clock::now();
    auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count();

    ++count;
    if ((float)count >= (float)(numOfBlocks / 100.0 * kWarmUpPerc)) {
      hist.Add(dur);
      actualRequests++;
      durations.emplace_back(reqData->cid, dur);
    }
  };

  for (int i = 1; i <= numOfBlocks; i++) {
    if (done) break;

    uint32_t keyPrefix = distr(eng);
    uint32_t valPrefix = distr(eng);

    string cid = "block_" + to_string(i);
    auto time = std::chrono::steady_clock::now();
    send_request(logger, pool, keyPrefix, valPrefix, numOfKvs, keySize, valSize,
                 executionTime, payload, cid, time, callback);
  }
}

void show_help(char** argv) {
  LOG_INFO(
      GL,
      "Command line options: \n"
          << " -b NBR - a number of requests to launch (default: 4000000) \n"
          << " -p 1/0 - requests pre-processing on/off (default: off) \n"
          << " -k NBR - leg1 number of keys (default: 9) \n"
          << " -s NBR - single key size (default: 118) \n"
          << " -v NBR - leg1 key value size (default: 1200) \n"
          << " -d NBR - payload size (default: 15100) \n"
          << " -c NBR - concurrency level: capacity of the clients pool "
             "(default: 15) \n"
          << " -e NBR - execution time (default: 0) \n"
          << " -f STRING - path to the client pool configuration file (default "
             "is external_client_tls_20.config) \n"
          << " -w 1/0 - to use a busy-wait (1) or a regular sleep (0) "
             "simulating "
             "execution time latency (default: 1)\n"
          << " -i 1/0 - to print (1) or not (0) request durations (default: "
             "1) \n"
          << " -l (0,1,2,3 - off, error, info, debug) - log level (default: "
             "1)\n"
          << " -x NBR - number of concurrent trades(for ClosedModel) (default: "
             "40)\n"
          << " -n NBR - number of worker threads (for ClosedModel) (default: "
             "8)\n"
          << " -t NBR - request timeout ms (default: 30000)\n"
          << " -u STRING - test case(Simple,TwoLegsClosed) (default: "
             "TwoLegsClosed)\n"
          << " -K NBR - leg2 keys number (for ClosedModel) (default: 30)\n"
          << " -V NBR - leg2 value size (for ClosedModel) (default: 2841)\n"
          << " -U NBR - client pool waiting mode (0 - callback, 1 - poll each "
             "10ms) (default: 0)");
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
        {"execution_time", required_argument, nullptr, 'e'},
        {"print_req_durations", required_argument, nullptr, 'i'},
        {"busy_wait", required_argument, nullptr, 'w'},
        {"pool_config_path", required_argument, nullptr, 'f'},
        {"log_level", required_argument, nullptr, 'l'},
        {"log_properties_file", required_argument, nullptr, 'a'},
        {"num_of_conc_trades", required_argument, nullptr, 'x'},
        {"num_of_worker_threads", required_argument, nullptr, 'n'},
        {"request_timeout", required_argument, nullptr, 't'},
        {"test_case", required_argument, nullptr, 'u'},
        {"leg2_num_of_keys", required_argument, nullptr, 'K'},
        {"leg2_val_size", required_argument, nullptr, 'V'},
        {"wait_mode", required_argument, nullptr, 'U'},
        {nullptr, 0, nullptr, 0}};
    int optionIndex = 0;
    int option = 0;
    while ((option = getopt_long(argc, argv,
                                 "b:p:k:s:v:d:c:e:i:w:f:l:a:x:n:t:u:K:V:U:",
                                 longOptions, &optionIndex)) != -1) {
      waitMethod = wait_with_callback;
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
        case 'e': {
          auto execution_time = stoi(string(optarg));
          if (execution_time >= 0) {
            executionTime = execution_time;
            executionTime2 = execution_time;
          }
          break;
        }
        case 'i': {
          printReqDurations = stoi(string(optarg)) != 0;
          break;
        }
        case 'w': {
          busyWait = stoi(string(optarg)) != 0;
          break;
        }
        case 'f': {
          poolConfigPath = std::string(optarg);
          break;
        }
        case 'l': {
          log_level = stoi(string(optarg));
          break;
        }
        case 'a': {
          log_properties_file = string(optarg);
          break;
        }
        case 'x': {
          loaderConcurrencyLevel = stoul(string(optarg));
          break;
        }
        case 'n': {
          loaderNumOfWorkers = stoul(string(optarg));
          break;
        }
        case 't': {
          timeOut = stoul(string(optarg));
          break;
        }
        case 'u': {
          testCase = string(optarg);
          if (testCase != "Simple" && testCase != "TwoLegsClosed") {
            cout << "Unsupported test case!!!" << endl;
            return false;
          }
          break;
        }
        case 'K': {
          numOfKvs2 = stoul(string(optarg));
          break;
        }
        case 'V': {
          valSize2 = stoul(string(optarg));
          break;
        }
        case 'U': {
          waitMethodParam = stoul(string(optarg));
          waitMethod = waitMethodParam ? wait_const_time : wait_with_callback;
          break;
        }
        default:
          cout << "Unknown option: " << option << endl;
          return false;
      }
    }
    return true;
  } catch (const std::exception& e) {
    cout << "Failed to parse command line arguments" << endl;
    LOG_FATAL(GL, "Failed to parse command line arguments: " << e.what());
    return false;
  }
}

void setMaxNumOfOpenFiles() {
  rlimit limit;

  limit.rlim_cur = 65535;
  limit.rlim_max = 65535;
  setrlimit(RLIMIT_NOFILE, &limit);
  getrlimit(RLIMIT_NOFILE, &limit);
  LOG_INFO(GL, KVLOG(limit.rlim_cur, limit.rlim_max));
}

int main(int argc, char** argv) {
  if (!parse_args(argc, argv)) {
    show_help(argv);
    return 1;
  }

  signal(SIGTERM, signalHandler);
  signal(SIGINT, signalHandler);

  bool logInit = false;
  {
    ifstream f(log_properties_file);
    if (f.good()) {
      logging::initLogger(log_properties_file);
      logInit = true;
    }
  }
  logging::Logger logger = logging::getLogger("perf-test-tool");
  if (!logInit)
    logger.setLogLevel(!log_level ? log4cplus::OFF_LOG_LEVEL
                                  : log_level * 10000u);
  cout << "Starting with the following parameters:\n"
       << "testCase: " << testCase << "\nnumOfKeys1: " << numOfKvs
       << "\nvalSize1: " << valSize << "\nnumOfKeys2: " << numOfKvs2
       << "\nvalSize2: " << valSize2 << "\npayload: " << payloadSize
       << "\npoolConfigPath: " << poolConfigPath
       << "\nlogConfigPath: " << log_properties_file
       << "\nclientConcLevel: " << concurrencyLevel
       << "\nloaderConcLevel: " << loaderConcurrencyLevel
       << "\nnumOfLoaderThreads:" << loaderNumOfWorkers
       << "\nreqTimeout: " << timeOut << "\nnumOfBlocks: " << numOfBlocks
       << "\nwaitMethod: " << (waitMethodParam ? "10ms poll" : "callback")
       << "\n**************************************************" << endl;

  setMaxNumOfOpenFiles();
  auto pool = new ConcordClientPool(poolConfigPath);
  pool->SetDoneCallback(req_callback);

  durations.reserve(numOfBlocks);

  // wait for the pool to connect
  this_thread::sleep_for(chrono::seconds(5));
  auto start = chrono::steady_clock::now();
  while (PoolStatus::NotServing == pool->HealthStatus()) {
    this_thread::sleep_for(chrono::seconds(5));
    if (chrono::duration_cast<chrono::seconds>(chrono::steady_clock::now() -
                                               start)
            .count() > 120) {
      LOG_FATAL(logger,
                "pool can't connect to at least 1 replica in 60 seconds. "
                "Aborting.");
      return 2;
    }
  }
  LOG_INFO(logger, "starting test");

  globalStart = chrono::steady_clock::now();
  // do_preloaded_test(logger, pool);
  if (testCase == "Simple")
    do_on_fly_test(logger, pool);
  else if (testCase == "TwoLegsClosed")
    do_close_model_trades_test(logger, pool);

  {
    unique_lock<mutex> l(reqSynch);
    while (activeRequests > 0) reqSignal.wait(l);
  }
  assert(active_requests_data.empty());
  globalEnd = chrono::steady_clock::now();

  delete pool;
  print_results();
}
