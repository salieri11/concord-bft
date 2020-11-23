// Concord
//
// Copyright (c) 2018 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.

#include "KVBCInterfaces.h"
#include "merkle_tree_storage_factory.h"
#include "histogram.hpp"
#include "Logger.hpp"
#include <thread>
#include <getopt.h>
#include <sys/resource.h>
#include <atomic>
#include <random>
#include <iomanip>
#include <unordered_map>
#include <condition_variable>
#include <endianness.hpp>
#include <queue>
#include <mutex>

using namespace std;
using namespace concord::kvbc;
using namespace concord::storage;
using namespace concord::kvbc::v2MerkleTree;
using namespace concordUtils;

uint curr_request = 0;
mutex q_mutex;
queue<SetOfKeyValuePairs *> produced_blocks;
uint num_of_producers = 1;

// all these values, including CLI params should be aligned to multiplies of 4!!!!
const uint kReadKeyLength = 20;
const uint kReadValueLength = 420;
const bool kCheckOutput = true;
// cli params
uint read_kv_count = 31;
uint write_kv_count = 31;
uint write_key_length = 20;
uint write_value_length = 840;
uint reader_sleep_milli = 10;
uint num_of_reader_threads = 2;
uint num_of_blocks = 10000;
std::string rocksdb_path = "rocksdbdata";
uint log_level = 2;

uint dataset_size = num_of_blocks / 10;
vector<Key> read_keys;
vector<Value> read_values;
vector<Key> write_keys;
vector<Key> write_values;

logging::Logger logger;

enum class Benchmark {
  FillDb,
  ReadWriteRocksDb,
  ReadWriteMerkle
};

template <typename T>
class Reader {
 private:
  Histogram h;
  IStorageFactory::DatabaseSet *dbset_ptr = nullptr;
  bool done = false;
  uint32_t read_count = 0;
  uint32_t read_size = 0;
  uint reader_id;
  T on_read_func = nullptr;

 public:
  Reader(IStorageFactory::DatabaseSet *dbset, uint &id, T &&on_read)
      : dbset_ptr{dbset}, reader_id{id}, on_read_func{on_read} {
    h.Clear();
  }

  void stop() { done = true; }

  void operator()() {
    cout << "reader started" << endl;
    std::default_random_engine generator;
    std::uniform_int_distribution<int> distribution(0, dataset_size);

    KeysVector read_keys_batch;
    ValuesVector read_values_batch;
    SetOfKeyValuePairs write_batch;
    vector<uint> indices;
    while (!done) {
      int ind = distribution(generator);

      // do read
      read_keys_batch.clear();
      read_values_batch.clear();
      indices.clear();
      for (uint i = ind, j = 0; j < read_kv_count; ++j, ++i) {
        if (i == dataset_size) i = 0;
        read_keys_batch.push_back(read_keys[i]);
        indices.push_back(i);
      }
      auto read_start = chrono::steady_clock::now();
      Status s = dbset_ptr->dataDBClient->multiGet(read_keys_batch, read_values_batch);
      ++read_count;
      auto read_end = chrono::steady_clock::now();
      auto dur = chrono::duration_cast<chrono::microseconds>(read_end - read_start).count();
      h.Add(dur);
      assert(s.isOK());
      assert(read_values_batch.size() == indices.size());
      for (uint i = 0; i < read_keys_batch.size(); ++i) {
        if (kCheckOutput) assert(read_values_batch[i] == read_values[indices[i]]);
        read_size += read_values[indices[i]].length();
      }

      if (on_read_func) {
        on_read_func();
      }
      if (reader_sleep_milli) this_thread::sleep_for(chrono::milliseconds(reader_sleep_milli));
    }
  }

  pair<uint32_t, uint32_t> get_sizes() { return {read_count, read_size}; }

  Histogram &get_histogram() { return h; }
};

void generate_data() {
  mt19937 generator(0);
  uniform_int_distribution<uint32_t> distribution(0, UINT32_MAX);

  for (uint i = 0; i < dataset_size; ++i) {
    char *read_key = new char[kReadKeyLength];
    for (uint j = 0; j < kReadKeyLength / sizeof(int); j++) {
      auto r = distribution(generator);
      memcpy(read_key + j * sizeof(int), &r, sizeof(int));
    }
    read_keys.emplace_back(read_key, kReadKeyLength);

    char *read_value = new char[kReadValueLength];
    for (uint j = 0; j < kReadValueLength / sizeof(int); j++) {
      auto r = distribution(generator);
      memcpy(read_value + j * sizeof(int), &r, sizeof(int));
    }
    read_values.emplace_back(read_value, kReadValueLength);

    char *write_key = new char[write_key_length];
    for (uint j = 0; j < write_key_length / sizeof(int); j++) {
      auto r = distribution(generator);
      memcpy(write_key + j * sizeof(int), &r, sizeof(int));
    }
    write_keys.emplace_back(write_key, write_key_length);

    char *write_value = new char[write_value_length];
    for (uint j = 0; j < write_value_length / sizeof(int); j++) {
      auto r = distribution(generator);
      memcpy(write_value + j * sizeof(int), &r, sizeof(int));
    }
    write_values.emplace_back(write_value, write_value_length);
  }
}

void create_db(const IStorageFactory::DatabaseSet *dbset) {
  concord::storage::SetOfKeyValuePairs write_set;
  int done = 0;
  for (uint i = 0; i < dataset_size;) {
    uint count = 0;
    while (count++ < min(100u, dataset_size - done)) {
      write_set[read_keys[i]] = read_values[i];
      i++;
    }
    dbset->dataDBClient->multiPut(write_set);
    done += 100;
    write_set.clear();
  }
}

void generate_blocks(uint blocks_count) {
  mt19937 generator(0);
  uniform_int_distribution<uint32_t> distribution(0, UINT32_MAX);

  for (uint i = 0; i < blocks_count; ++i) {
    uint count = 0;
    concord::storage::SetOfKeyValuePairs *write_set = new SetOfKeyValuePairs();
    while (count++ < write_kv_count) {
      char *write_key = new char[write_key_length];
      for (uint j = 0; j < write_key_length / sizeof(int); j++) {
        auto r = distribution(generator);
        memcpy(write_key + j * sizeof(int), &r, sizeof(int));
      }
      char *write_value = new char[write_value_length];
      for (uint j = 0; j < write_value_length / sizeof(int); j++) {
        auto r = distribution(generator);
        memcpy(write_value + j * sizeof(int), &r, sizeof(int));
      }
      write_set->emplace(Sliver{write_key, write_key_length}, Sliver{write_value, write_value_length});
    }

    produced_blocks.push(write_set);
  }
}

template <typename T>
void collect_and_wait(vector<T> &workers,
                      vector<thread> &threads,
                      Histogram &read_hist,
                      uint64_t &out_read_count,
                      uint64_t &out_read_size) {
  for (auto &w : workers) {
    w.stop();
    read_hist.Merge(w.get_histogram());
    auto sizes = w.get_sizes();
    out_read_count += sizes.first;
    out_read_size += sizes.second;
  }
  for (auto &t : threads) t.join();
}

void do_writes(Histogram &writer_hist,
               IStorageFactory::DatabaseSet &dbset,
               uint64_t &out_write_size,
               function<void()> &&wait_before = nullptr) {
  std::default_random_engine generator;
  std::uniform_int_distribution<int> distribution(0, dataset_size);
  SetOfKeyValuePairs write_batch;
  while (curr_request < num_of_blocks) {
    uint ind = distribution(generator);
    write_batch.clear();
    for (uint i = ind, j = 0; j < write_kv_count; ++j, ++i) {
      if (i == dataset_size) i = 0;
      write_batch[write_keys[i]] = write_values[i];
      out_write_size += write_values[i].length();
    }

    if (wait_before) wait_before();

    auto write_start = chrono::steady_clock::now();
    Status s = dbset.dataDBClient->multiPut(write_batch);
    assert(s.isOK());
    auto write_end = chrono::steady_clock::now();
    auto dur = chrono::duration_cast<chrono::microseconds>(write_end - write_start).count();
    writer_hist.Add(dur);
    curr_request++;
  }
}

void stress_test(Histogram &write_hist,
                 Histogram &read_hist,
                 IStorageFactory::DatabaseSet &dbset,
                 uint64_t &out_read_count,
                 uint64_t &out_read_size,
                 uint64_t &out_write_size) {
  vector<thread> threads;
  threads.reserve(num_of_reader_threads);
  vector<Reader<std::function<void()>>> workers;
  workers.reserve(num_of_reader_threads);
  for (uint i = 0; i < num_of_reader_threads; ++i) {
    workers.emplace_back(&dbset, i, nullptr);
  }
  for (auto &w : workers) {
    threads.emplace_back(std::ref(w));
  }

  do_writes(write_hist, dbset, out_write_size);
  collect_and_wait(workers, threads, read_hist, out_read_count, out_read_size);
}

void read_read_write_test(Histogram &write_hist,
                          Histogram &read_hist,
                          IStorageFactory::DatabaseSet &dbset,
                          uint64_t &out_read_count,
                          uint64_t &out_read_size,
                          uint64_t &out_write_size) {
  unordered_map<uint, uint64_t> read_count;  // reader id -> read done count
  vector<thread> threads;
  threads.reserve(num_of_reader_threads);
  vector<Reader<std::function<void()>>> workers;
  workers.reserve(num_of_reader_threads);
  uint signalled = 0;
  condition_variable cv;
  mutex m;
  for (uint i = 0; i < num_of_reader_threads; ++i) {
    workers.emplace_back(&dbset, i, [&signalled, &m, &cv]() mutable {
      lock_guard<mutex> lg(m);
      ++signalled;
      cv.notify_all();
    });
  }
  for (auto &w : workers) {
    threads.emplace_back(std::ref(w));
  }

  do_writes(write_hist, dbset, out_write_size, [&signalled, &cv, &m]() mutable {
    unique_lock<mutex> ul(m);
    if (signalled < 1) {
      cv.wait(ul, [&signalled]() { return signalled > 1; });
      --signalled;
    }
  });
  collect_and_wait(workers, threads, read_hist, out_read_count, out_read_size);
}

uint64_t write1(const IStorageFactory::DatabaseSet *dbset, Histogram &h) {
  uint64_t count = 0;
  BlockId lastBlockId;
  concord::storage::SetOfKeyValuePairs write_set;
  mt19937 generator(0);
  uniform_int_distribution<uint32_t> distribution(0, UINT32_MAX);
  uint64_t dur = 0;

  while (count < num_of_blocks) {
    uint kcount = 0;
    while (kcount++ < write_kv_count) {
      char *write_key = new char[write_key_length];
      for (uint j = 0; j < write_key_length / sizeof(int); j++) {
        auto r = distribution(generator);
        memcpy(write_key + j * sizeof(int), &r, sizeof(int));
      }
      char *write_value = new char[write_value_length];
      for (uint j = 0; j < write_value_length / sizeof(int); j++) {
        auto r = distribution(generator);
        memcpy(write_value + j * sizeof(int), &r, sizeof(int));
      }
      write_set.emplace(Sliver{write_key, write_key_length}, Sliver{write_value, write_value_length});
    }

    auto s = std::chrono::steady_clock::now();
    lastBlockId = dbset->dbAdapter->addBlock(write_set);
    auto e = std::chrono::steady_clock::now();
    auto d = std::chrono::duration_cast<std::chrono::microseconds>(e - s).count();
    h.Add(d);
    dur += d;
    write_set.clear();
    if (++count % 1000 == 0) cout << "Written " << count << " blocks" << endl;
  }

  return dur;

  //  const Sliver last_agreed_prunable_block_id_key_{std::string{0x24}};
  //  const auto block =
  //      SetOfKeyValuePairs{std::make_pair(last_agreed_prunable_block_id_key_,
  //      concordUtils::toBigEndianStringBuffer(0L))};
  //  cout << "Done. Last block id: " << lastBlockId << endl;
  //  lastBlockId = dbset->dbAdapter->addBlock(block);
  //  cout << "Done. Last block after adding pruning info is: " << lastBlockId << endl;
}

uint64_t write(const IStorageFactory::DatabaseSet *dbset, Histogram &h) {
  uint64_t count = 0;
  uint64_t dur = 0;
  BlockId lastBlockId;
  while (count < num_of_blocks) {
    if (produced_blocks.empty()) {
      this_thread::sleep_for(1s);
      continue;
    }
    SetOfKeyValuePairs *block = produced_blocks.front();
    auto s = std::chrono::steady_clock::now();
    lastBlockId = dbset->dbAdapter->addBlock(*block);
    // dbset->dataDBClient->multiPut(*block);
    auto e = std::chrono::steady_clock::now();
    auto d = std::chrono::duration_cast<std::chrono::microseconds>(e - s).count();
    h.Add(d);
    dur += d;
    produced_blocks.pop();
    delete block;
    if (++count % 100 == 0) cout << "Written " << count << " blocks" << endl;
  }

  const Sliver last_agreed_prunable_block_id_key_{std::string{0x24}};
  const auto block =
      SetOfKeyValuePairs{std::make_pair(last_agreed_prunable_block_id_key_, concordUtils::toBigEndianStringBuffer(0L))};
  cout << "Done. Last block id: " << lastBlockId << endl;
  lastBlockId = dbset->dbAdapter->addBlock(block);
  cout << "Done. Last block after adding pruning info is: " << lastBlockId << endl;
  return dur;
}

void show_help() {
  std::cout <<
      "Command line options: \n"
          << " -b uint - number of blocks, multiply of 4 (default: 10000) \n"
          << " -k uint - number of keys (default: 31) \n"
          << " -s uint - single key size, multiply of 4 (default: 20) \n"
          << " -v uint - single value size, multiply of 4 (default: 840) \n"
          << " -c uint - concurrency level: number of readers (for ReadWrite test) "
             "(default: 4) \n"
          << " -e uint - reader delay, time to wait between 2 reads, ms (default: 10) \n"
          << " -r strimg - path to the RocksDb location (default "
             "./rocksdbdata) \n"
          << " -l (0,1,2,3 - off, error, info, debug) - log level (default: 1" << endl;
}

bool parse_args(int argc, char** argv) {
  try {
    static struct option longOptions[] = {
        {"num_of_blocks", required_argument, nullptr, 'b'},
        {"num_of_keys", required_argument, nullptr, 'k'},
        {"key_size", required_argument, nullptr, 's'},
        {"single_value_size", required_argument, nullptr, 'v'},
        {"concurrency_level", required_argument, nullptr, 'c'},
        {"reader_delay", required_argument, nullptr, 'e'},
        {"rocksdb_config_path", required_argument, nullptr, 'f'},
        {"log_level", required_argument, nullptr, 'l'},
        {nullptr, 0, nullptr, 0}};
    int optionIndex = 0;
    int option = 0;
    while ((option = getopt_long(argc, argv, "b:k:s:v:c:e:f:l:",
                                 longOptions, &optionIndex)) != -1) {
      switch (option) {
        case 'b': {
          auto blocks = stoi(string(optarg));
          if (blocks > 0) num_of_blocks = blocks;
          break;
        }
        case 'k': {
          auto nk = stoi(string(optarg));
          if (nk > 0) write_kv_count = nk;
          break;
        }
        case 's': {
          auto key_size = stoi(string(optarg));
          if (key_size > 0) write_key_length = key_size;
          break;
        }
        case 'v': {
          auto key_value_size = stoi(string(optarg));
          if (key_value_size > 0) write_value_length = key_value_size;
          break;
        }
        case 'c': {
          auto concurrency_level = stoi(string(optarg));
          if (concurrency_level > 0) num_of_reader_threads = concurrency_level;
          break;
        }
        case 'e': {
          auto execution_time = stoi(string(optarg));
          if (execution_time >= 0) reader_sleep_milli = execution_time;
          break;
        }
        case 'f': {
          rocksdb_path = std::string(optarg);
          break;
        }
        case 'l': {
          log_level = stoi(string(optarg));
          break;
        }
        default:
          return false;
      }
    }
    return true;
  } catch (const std::exception& e) {
    cout << "Failed to parse command line arguments: " << e.what() << endl;
    return false;
  }
}

void setMaxNumOfOpenFiles() {
  rlimit limit;
  limit.rlim_cur = 65535;
  limit.rlim_max = 65535;
  setrlimit(RLIMIT_NOFILE, &limit);
  getrlimit(RLIMIT_NOFILE, &limit);
  LOG_INFO(logger, KVLOG(limit.rlim_cur, limit.rlim_max));
}

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  if (!parse_args(argc, argv)) {
    show_help();
    return 1;
  }

  logging::initLogger("log4cplus.properties");
  logger = logging::getLogger("storage-benchmark");

#if USE_LOG4CPP
  logger.setLogLevel(log_level ? log4cplus::OFF_LOG_LEVEL : log_level * 1000u);
#else
  logger.setLogLevel(log_level);
#endif

  IStorageFactory *factory = new RocksDBStorageFactory(rocksdb_path);
  auto dbset = factory->newDatabaseSet();

  cout << "generating data..." << endl;
  // generate_data();
  // create_db(&dbset);

  num_of_blocks = 10000;
  // int num_of_cores = 1;
  vector<thread> threads;
  /*threads.reserve(num_of_cores);
  uint numThreads = num_of_cores;
  uint chunkSize = ceil((double)num_of_blocks / numThreads);
  cout << chunkSize << endl;
  for (uint i = 0; i < numThreads; i++) {
    uint c = min(chunkSize, num_of_blocks - (i * chunkSize));
    cout << c << endl;
    threads.emplace_back(generate_blocks, c);
  }*/
  Histogram h;
  h.Clear();
  auto d = write1(&dbset, h);
  cout << "total: " << d << " for " << num_of_blocks << endl;
  cout << h.ToString() << endl;
  return 0;

  for (auto &t : threads) t.join();
  // write1(&dbset);
  return 0;

  Histogram writer_hist;
  Histogram readers_hist;
  readers_hist.Clear();
  writer_hist.Clear();
  uint64_t read_count = 0;
  uint64_t read_size = 0;
  uint64_t write_size = 0;

  cout << "Started test" << endl;
  auto start = chrono::steady_clock::now();
  // stress_test(writer_hist, readers_hist, dbset, read_count, read_size, write_size);
  read_read_write_test(writer_hist, readers_hist, dbset, read_count, read_size, write_size);
  auto end = chrono::steady_clock::now();
  cout << "Finished test" << endl;

  auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count();
  cout << "Test duration: " << dur << " milliseconds" << endl;
  cout << "Read rate: " << setprecision(15) << floor((double)read_count / dur * 1000) << " reads/sec => ";
  cout << setprecision(15) << floor((double)read_size / dur * 1000) << " bytes/sec"
       << ", total bytes read: " << read_size << endl;
  cout << "Readers details (microseconds): " << endl << readers_hist.ToString() << endl;
  cout << "Write rate: " << setprecision(15) << floor((double)num_of_blocks / dur * 1000) << " writes/sec => ";
  cout << setprecision(15) << floor((double)write_size / dur * 1000) << " bytes/sec"
       << ", total bytes written: " << write_size << endl;
  cout << "Writer details (microseconds):" << endl << writer_hist.ToString() << endl;
  return 0;
}