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
#include <condition_variable>
#include <endianness.hpp>
#include <queue>
#include <mutex>
#include <rocksdb/client.h>

using namespace std;
using namespace concord::kvbc;
using namespace concord::storage;
using namespace concord::kvbc::v2MerkleTree;
using namespace concordUtils;

struct xorshift64_state {
  uint64_t a;
};

uint64_t xorshift64(struct xorshift64_state *state) {
  uint64_t x = state->a;
  x ^= x << 13;
  x ^= x >> 7;
  x ^= x << 17;
  return state->a = x;
}

typedef std::function<Status(SetOfKeyValuePairs &)> WriteFunction;

enum class Benchmark : uint8_t { FillDb, ReadWriteStress, MIN_VALUE = FillDb, MAX_VALUE = ReadWriteStress };

enum class WriteType : uint8_t { Merkle, RocksDbDirect, MIN_VALUE = Merkle, MAX_VALUE = RocksDbDirect };

enum class FillMethod : uint8_t { Simple, PseudoRandom, Random, MIN_VALUE = Simple, MAX_VALUE = Random };

// all these values, including CLI params should be aligned to multiplies of 4!!!!
const uint kReadKeyLength = 20;
const uint kReadValueLength = 420;
const uint data_set_size_factor = 10;
const uint stat_dump_perc = 1;
const string log_properties_file = "log4cplus.properties";
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
Benchmark benchmark = Benchmark::FillDb;
WriteType write_type = WriteType::RocksDbDirect;
FillMethod random_type = FillMethod::PseudoRandom;

uint readset_size = num_of_blocks / data_set_size_factor;
uint64_t interval_count = 0;
vector<Key> read_keys;
vector<Value> read_values;
vector<BlockId> blocks_for_added_read_keys;
mt19937 generator(0);
uniform_int_distribution<uint64_t> distribution(0, UINT64_MAX);
uint curr_block = 0;
logging::Logger logger = logging::getLogger("storage-benchmark");

class Reader {
 protected:
  Histogram h;
  bool done = false;
  uint32_t read_count = 0;
  uint32_t read_size = 0;
  uint reader_id;
  shared_ptr<thread> t = nullptr;

  Reader(uint &id) : reader_id{id} { h.Clear(); }

 public:
  Reader() {}
  virtual void start() = 0;

  virtual ~Reader() {}

  void stop() {
    done = true;
    t->join();
  }
  pair<uint32_t, uint32_t> get_sizes() { return {read_count, read_size}; }
  Histogram &get_histogram() { return h; }
};

class DirectKeysReader : public Reader {
 public:
  DirectKeysReader(uint &id, std::shared_ptr<concord::storage::IDBClient> client) : Reader(id) { dbclient = client; }

  virtual void start() override {
    t = make_shared<thread>([&]() {
      LOG_INFO(logger, "DirectKeysReader " << reader_id << " started");

      KeysVector read_keys_batch;
      ValuesVector read_values_batch;
      SetOfKeyValuePairs write_batch;
      vector<uint> indices;
      while (!done) {
        uint ind = distribution(generator) % readset_size;
        // do read
        read_keys_batch.clear();
        read_values_batch.clear();
        indices.clear();
        for (uint i = ind, j = 0; j < read_kv_count; ++j, ++i) {
          if (i == readset_size) i = 0;
          read_keys_batch.push_back(read_keys[i]);
          indices.push_back(i);
        }
        auto read_start = chrono::steady_clock::now();
        Status s = dbclient->multiGet(read_keys_batch, read_values_batch);
        auto read_end = chrono::steady_clock::now();
        auto dur = chrono::duration_cast<chrono::microseconds>(read_end - read_start).count();
        h.Add(dur);
        assert(s.isOK());
        assert(read_values_batch.size() == indices.size());
        for (uint i = 0; i < read_keys_batch.size(); ++i) {
          assert(read_values_batch[i] == read_values[indices[i]]);
          read_size += read_values[indices[i]].length();
        }

        if (reader_sleep_milli) this_thread::sleep_for(chrono::milliseconds(reader_sleep_milli));
      }
    });
  }

 private:
  std::shared_ptr<concord::storage::IDBClient> dbclient = nullptr;
};

class KvbcKeysReader : public Reader {
 public:
  KvbcKeysReader(uint &id, IDbAdapter *ad) : Reader(id) { kvbc_adapter = ad; }

  virtual void start() override {
    t = make_shared<thread>([&]() {
      LOG_INFO(logger, "KvbcKeysReader " << reader_id << " started");

      while (!done) {
        try {
          auto read_start = chrono::steady_clock::now();
          for (uint j = 0; j < read_kv_count; ++j) {
            uint ind = distribution(generator) % readset_size;
            BlockId blockId = blocks_for_added_read_keys[ind];
            LOG_DEBUG(logger, "try to read values for block id " << blockId << " at index " << ind);
            Sliver res = kvbc_adapter->getValue(read_keys[ind], blockId).first;
            assert(!res.empty());
            assert(res == read_values[ind]);
            ++read_count;
            read_size += res.size();
          }
          auto read_end = chrono::steady_clock::now();
          auto dur = chrono::duration_cast<chrono::microseconds>(read_end - read_start).count();
          h.Add(dur);

          if (reader_sleep_milli) this_thread::sleep_for(chrono::milliseconds(reader_sleep_milli));
        } catch (std::exception &ex) {
          LOG_ERROR(logger, "kvbc_reader " << ex.what());
        }
      }
    });
  }

 private:
  // not owning
  IDbAdapter *kvbc_adapter = nullptr;
};

class ReaderFactory {
 public:
  virtual shared_ptr<Reader> create_reader(uint &id) = 0;
};

class DirectKeysReaderFactory : public ReaderFactory {
 public:
  DirectKeysReaderFactory(std::shared_ptr<concord::storage::IDBClient> cl) { client = cl; }

  virtual shared_ptr<Reader> create_reader(uint &id) override { return std::make_shared<DirectKeysReader>(id, client); }

 private:
  std::shared_ptr<concord::storage::IDBClient> client = nullptr;
};

class KvbcKeyReaderFactory : public ReaderFactory {
 public:
  KvbcKeyReaderFactory(IDbAdapter *ad) { kvbc_adapter = ad; }

  virtual shared_ptr<Reader> create_reader(uint &id) override {
    return std::make_shared<KvbcKeysReader>(id, kvbc_adapter);
  }

 private:
  // not owning
  IDbAdapter *kvbc_adapter = nullptr;
};

// possible to have both Block`reader and kets readers?
Reader *create_reader(std::shared_ptr<concord::storage::IDBClient> client, uint &&id) {
  assert(write_type == WriteType::RocksDbDirect);
  return new DirectKeysReader(id, client);
}

void generate_data(char *data, const uint &size) {
  uint i = 0;
  static xorshift64_state xor_state{static_cast<uint64_t>(std::chrono::steady_clock::now().time_since_epoch().count())};
  switch (random_type) {
    case (FillMethod::Simple): {
      uint64_t v = xorshift64(&xor_state);
      memcpy(data, &v, sizeof(uint64_t));
      v = xorshift64(&xor_state);
      memcpy(data + sizeof(uint64_t), &v, sizeof(uint64_t));
      i += (2 * sizeof(uint64_t));
      for (; i < size; ++i) data[i] = i;
    }
    case FillMethod::PseudoRandom: {
      for (; i < size; i += sizeof(uint64_t)) {
        uint64_t v = xorshift64(&xor_state);
        memcpy(data + i, &v, sizeof(uint64_t));
      }
      break;
    }
    case FillMethod::Random: {
      for (; i < size; i += sizeof(uint64_t)) {
        uint64_t r = distribution(generator);
        memcpy(data + i, &r, sizeof(uint64_t));
      }
      break;
    }
    default: {
      assert(false);
    }
  }
}

void generate_read_data() {
  LOG_INFO(logger, "generating new read values");
  for (uint i = 0; i < readset_size; ++i) {
    char *read_key = new char[kReadKeyLength];
    generate_data(read_key, kReadKeyLength);
    read_keys.emplace_back(read_key, kReadKeyLength);

    char *read_value = new char[kReadValueLength];
    generate_data(read_value, kReadValueLength);
    read_values.emplace_back(read_value, kReadValueLength);
  }
  LOG_INFO(logger, "done generating new read values");
}

void update_db(WriteFunction &&write_function) {
  LOG_INFO(logger, "updating db with new read values");
  SetOfKeyValuePairs write_set;
  for (uint i = 0; i < readset_size; ++i) {
    write_set[read_keys[i]] = read_values[i];
    write_function(write_set);
    write_set.clear();
  }

  LOG_INFO(logger, "done updating db with new read values");
}

void collect_and_wait(vector<shared_ptr<Reader>> &workers,
                      vector<thread> &threads,
                      Histogram &read_hist,
                      uint64_t &out_read_count,
                      uint64_t &out_read_size) {
  for (auto &w : workers) {
    w->stop();
    read_hist.Merge(w->get_histogram());
    auto sizes = w->get_sizes();
    out_read_count += sizes.first;
    out_read_size += sizes.second;
  }
  for (auto &t : threads) t.join();
}

void dump_stats(uint64_t &interval_duration) {
  if (curr_block % interval_count == 0) {
    cout << "Interval stats: " << curr_block << " blocks written, "
         << "rate: " << setprecision(4) << (double)interval_count / interval_duration * 1000000 << " blocks/sec"
         << endl;
    interval_duration = 0;
  }
}

uint64_t do_writes(Histogram &writer_hist, WriteFunction &&write_function, uint64_t &out_write_size) {
  SetOfKeyValuePairs write_batch;
  uint64_t totalDur = 0;

  uint64_t interval_duration = 0;
  while (curr_block < num_of_blocks) {
    write_batch.clear();
    for (uint i = 0; i < write_kv_count; ++i) {
      char *key = new char[write_key_length];
      generate_data(key, write_key_length);
      char *value = new char[write_value_length];
      generate_data(value, write_value_length);
      write_batch.emplace(Sliver{key, write_key_length}, Sliver{value, write_value_length});
      out_write_size += write_value_length;
    }

    auto write_start = chrono::steady_clock::now();
    Status s = write_function(write_batch);
    assert(s.isOK());
    auto write_end = chrono::steady_clock::now();
    auto dur = chrono::duration_cast<chrono::microseconds>(write_end - write_start).count();
    totalDur += dur;
    interval_duration += dur;
    writer_hist.Add(dur);
    curr_block++;

    dump_stats(interval_duration);
  }

  return totalDur;
}

uint64_t stress_test(WriteFunction &write_data_fn,
                     ReaderFactory &readerFactory,
                     Histogram &write_hist,
                     Histogram &read_hist,
                     uint64_t &out_read_count,
                     uint64_t &out_read_size,
                     uint64_t &out_write_size) {
  generate_read_data();
  update_db(std::forward<WriteFunction>(write_data_fn));

  std::vector<thread> threads;
  threads.reserve(num_of_reader_threads);
  vector<shared_ptr<Reader>> workers;
  workers.reserve(num_of_reader_threads);
  for (uint i = 0; i < num_of_reader_threads; ++i) {
    auto reader = readerFactory.create_reader(i);
    workers.push_back(reader);
    reader->start();
  }

  auto res = do_writes(write_hist, std::forward<WriteFunction>(write_data_fn), out_write_size);
  collect_and_wait(workers, threads, read_hist, out_read_count, out_read_size);
  return res;
}

uint64_t create_and_fill(Histogram &h, uint64_t &outWriteSize, WriteFunction &write_data_fn) {
  concord::storage::SetOfKeyValuePairs write_set;
  uint64_t dur = 0;
  uint64_t interval_dur = 0;

  while (curr_block++ < num_of_blocks) {
    uint kcount = 0;
    while (kcount++ < write_kv_count) {
      char *write_key = new char[write_key_length];
      generate_data(write_key, write_key_length);
      char *write_value = new char[write_value_length];
      generate_data(write_value, write_value_length);
      write_set.emplace(Sliver{write_key, write_key_length}, Sliver{write_value, write_value_length});
      outWriteSize += write_value_length;
    }

    auto s = std::chrono::steady_clock::now();
    auto status = write_data_fn(write_set);
    assert(status.isOK());
    auto e = std::chrono::steady_clock::now();
    auto d = std::chrono::duration_cast<std::chrono::microseconds>(e - s).count();
    h.Add(d);
    dur += d;
    interval_dur += d;
    dump_stats(interval_dur);
    write_set.clear();
  }

  return dur;
}

void show_help() {
  std::cout << "Command line options: \n"
            << " -b uint - number of blocks, multiply of 4 (default: 10000) \n"
            << " -k uint - number of keys (default: 31) \n"
            << " -s uint - single key size, multiply of 4 (default: 20) \n"
            << " -v uint - single value size, multiply of 4 (default: 840) \n"
            << " -c uint - concurrency level: number of readers (for ReadWrite test) "
               "(default: 4) \n"
            << " -e uint - reader delay, time to wait between 2 reads, ms (default: 10) \n"
            << " -f string - path to the RocksDb location (default "
               "./rocksdbdata) \n"
            << " -l (0,1,2,3 - off, error, info, debug) - log level, will be overrided by log4cplus.properties file "
               " if found in current folder (default: 1) \n"
            << " -t (0, 1 - FIllDb, ReadWriteStress) - benchmark to run (default: 1) \n"
            << " -w (0, 1 - Merkle Tree, RocksdDbDirect) - layer to write data with (default: 0) \n"
            << endl;
}

bool parse_args(int argc, char **argv) {
  try {
    static struct option longOptions[] = {{"num_of_blocks", required_argument, nullptr, 'b'},
                                          {"num_of_keys", required_argument, nullptr, 'k'},
                                          {"key_size", required_argument, nullptr, 's'},
                                          {"single_value_size", required_argument, nullptr, 'v'},
                                          {"concurrency_level", required_argument, nullptr, 'c'},
                                          {"reader_delay", required_argument, nullptr, 'e'},
                                          {"rocksdb_config_path", required_argument, nullptr, 'f'},
                                          {"log_level", required_argument, nullptr, 'l'},
                                          {"benchmark_to_run", required_argument, nullptr, 't'},
                                          {"write_type", required_argument, nullptr, 'w'},
                                          {"random_type", required_argument, nullptr, 'r'},
                                          {nullptr, 0, nullptr, 0}};
    int optionIndex = 0;
    int option = 0;
    while ((option = getopt_long(argc, argv, "b:k:s:v:c:e:f:l:t:w:r:", longOptions, &optionIndex)) != -1) {
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
        case 't': {
          uint b = stoi(string(optarg));
          if (b < (uint)Benchmark::MIN_VALUE || b > (uint)Benchmark::MAX_VALUE) return false;
          benchmark = static_cast<Benchmark>(b);
          break;
        }
        case 'w': {
          uint w = stoi(string(optarg));
          if (w < (uint)WriteType::MIN_VALUE || w > (uint)WriteType::MAX_VALUE) return false;
          write_type = static_cast<WriteType>(w);
          break;
        }
        case 'r': {
          uint r = stoi(string(optarg));
          if (r < (uint)FillMethod::MIN_VALUE || r > (uint)FillMethod::MAX_VALUE) return false;
          random_type = static_cast<FillMethod>(r);
          break;
        }
        default:
          return false;
      }
    }
    return true;
  } catch (const std::exception &e) {
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

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;
  if (!parse_args(argc, argv)) {
    show_help();
    return 1;
  }

  interval_count = num_of_blocks / 100 * stat_dump_perc;

  {
    ifstream f(log_properties_file);
    if (f.good()) {
      logging::initLogger(log_properties_file);
    } else {
#if USE_LOG4CPP
      logger.setLogLevel(log_level ? log4cplus::OFF_LOG_LEVEL : log_level * 10000u);
#else
      logger.setLogLevel(log_level);
#endif
    }
  }

  Histogram write_hist;
  write_hist.Clear();
  Histogram readers_hist;
  readers_hist.Clear();
  uint64_t read_count = 0;
  uint64_t read_size = 0;
  uint64_t write_size = 0;
  uint64_t write_duration = 0;

  RocksDBStorageFactory *factory = nullptr;
  IStorageFactory::DatabaseSet dbSet;
  shared_ptr<concord::storage::IDBClient> dbclient = nullptr;

  WriteFunction write_function = nullptr;
  ReaderFactory *rf = nullptr;
  if (WriteType::Merkle == write_type) {
    factory = new RocksDBStorageFactory(rocksdb_path);
    dbSet = factory->newDatabaseSet();
    blocks_for_added_read_keys.reserve(readset_size);
    BlockId bid = dbSet.dbAdapter->getLatestBlockId();
    write_function = [&](SetOfKeyValuePairs &data) {
      try {
        auto blockId = dbSet.dbAdapter->addBlock(std::forward<SetOfKeyValuePairs>(data));
        assert(blockId == bid + 1);
        ++bid;
        blocks_for_added_read_keys.push_back(blockId);
        LOG_DEBUG(logger,
                  "added new block " << blockId
                                     << ", blocks_for_added_read_keys.size() == " << blocks_for_added_read_keys.size());
      } catch (std::exception &ex) {
        LOG_ERROR(logger, "merkle_write_fn " << ex.what());
      }
      return Status::OK();
    };
    rf = new KvbcKeyReaderFactory(dbSet.dbAdapter.get());
  } else {
    dbclient = make_shared<concord::storage::rocksdb::Client>(rocksdb_path);
    dbclient->init();
    write_function = [&](SetOfKeyValuePairs &data) {
      return dbclient->multiPut(std::forward<SetOfKeyValuePairs>(data));
    };
    rf = new DirectKeysReaderFactory(dbclient);
  }

  LOG_INFO(logger, "starting benchmark");
  auto start = chrono::steady_clock::now();
  switch (benchmark) {
    case Benchmark::FillDb:
      write_duration = create_and_fill(write_hist, write_size, write_function);
      break;
    case Benchmark::ReadWriteStress:
      write_duration = stress_test(write_function, *rf, write_hist, readers_hist, read_count, read_size, write_size);
      break;
  }
  auto end = chrono::steady_clock::now();
  LOG_INFO(logger,
           "Done. Duration: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count() << " ms.");

  if (WriteType::Merkle == write_type) {
    const Sliver last_agreed_prunable_block_id_key_{std::string{0x24}};
    const auto block = SetOfKeyValuePairs{
        std::make_pair(last_agreed_prunable_block_id_key_, concordUtils::toBigEndianStringBuffer(0L))};
  }

  auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count();
  cout << "Test duration: " << dur << " milliseconds" << endl;
  cout << "Net writers duration: " << write_duration / 1000 << " milliseconds" << endl;
  cout << "Delta: " << dur - write_duration / 1000 << endl;
  cout << "Read rate: " << setprecision(15) << floor((double)read_count / dur * 1000) << " reads/sec => ";
  cout << setprecision(15) << floor((double)read_size / dur * 1000) << " bytes/sec"
       << ", total bytes read: " << read_size << endl;
  cout << "Readers details (microseconds): " << endl << readers_hist.ToString() << endl;
  cout << "Overall write rate: " << setprecision(15) << floor((double)num_of_blocks / dur * 1000) << " writes/sec => ";
  cout << setprecision(15) << floor((double)write_size / dur * 1000) << " bytes/sec"
       << ", total bytes written: " << write_size << endl;
  cout << "Net write rate: " << setprecision(15) << floor((double)num_of_blocks / write_duration * 1000000)
       << " writes/sec => ";
  cout << setprecision(15) << floor((double)write_size / write_duration * 1000) << " bytes/sec"
       << ", total bytes written: " << write_size << endl;
  cout << "Writer details (microseconds):" << endl << write_hist.ToString() << endl;
  return 0;
}