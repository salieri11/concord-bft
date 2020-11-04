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
#include "storage_factory_interface.h"
#include "merkle_tree_storage_factory.h"
#include "histogram.hpp"
#include <memory>
#include <thread>
#include <atomic>
#include <random>
#include <iomanip>

using namespace std;
using namespace concord::kvbc;
using namespace concord::storage;
using namespace concord::kvbc::v2MerkleTree;
using namespace concordUtils;

uint num_of_reader_threads = 4;
atomic<uint> num_of_requests = 100000;
atomic<uint> curr_request = 0;

uint dataset_size = num_of_requests / 10;
vector<Key> read_keys;
vector<Value> read_values;
vector<Key> write_keys;
vector<Key> write_values;

// these values should be aligned to multiplies of 4!!!!
uint key_length = 112;
uint read_kv_count = 36;
uint read_key_length = 124;
uint read_value_length = 420;
uint write_kv_count = 31;
uint write_key_length = 20;
uint write_value_length = 800;
uint reader_sleep_milli = 0;

bool check_output = true;

class Reader {
 private:
  Histogram h;
  IStorageFactory::DatabaseSet *dbset_ptr = nullptr;
  bool done = false;
  uint32_t read_count = 0;
  uint32_t read_size = 0;

 public:
  Reader(IStorageFactory::DatabaseSet *dbset) : dbset_ptr{dbset} { h.Clear(); }

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
        if (check_output) assert(read_values_batch[i] == read_values[indices[i]]);
        read_size += read_values[indices[i]].length();
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
    char *read_key = new char[read_key_length];
    for (uint j = 0; j < read_key_length / sizeof(int); j++) {
      auto r = distribution(generator);
      memcpy(read_key + j * sizeof(int), &r, sizeof(int));
    }
    read_keys.emplace_back(read_key, read_key_length);

    char *read_value = new char[read_value_length];
    for (uint j = 0; j < read_value_length / sizeof(int); j++) {
      auto r = distribution(generator);
      memcpy(read_value + j * sizeof(int), &r, sizeof(int));
    }
    read_values.emplace_back(read_value, read_value_length);

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

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;
  IStorageFactory *factory = new RocksDBStorageFactory("rocksdb_benchmark");
  auto dbset = factory->newDatabaseSet();

  cout << "generating data..." << endl;
  generate_data();
  create_db(&dbset);

  Histogram writer_hist;
  writer_hist.Clear();
  vector<thread> threads;
  threads.reserve(num_of_reader_threads);
  vector<Reader> workers;
  workers.reserve(num_of_reader_threads);
  for (uint i = 0; i < num_of_reader_threads; ++i) {
    workers.emplace_back(&dbset);
  }
  for (auto &w : workers) {
    threads.emplace_back(std::ref(w));
  }

  std::default_random_engine generator;
  std::uniform_int_distribution<int> distribution(0, dataset_size);
  SetOfKeyValuePairs write_batch;
  cout << "Started test" << endl;
  auto start = chrono::steady_clock::now();
  uint64_t write_size = 0;
  while (curr_request < num_of_requests) {
    uint ind = distribution(generator);
    write_batch.clear();
    for (uint i = ind, j = 0; j < write_kv_count; ++j, ++i) {
      if (i == dataset_size) i = 0;
      write_batch[write_keys[i]] = write_values[i];
      write_size += write_values[i].length();
    }
    auto write_start = chrono::steady_clock::now();
    Status s = dbset.dataDBClient->multiPut(write_batch);
    assert(s.isOK());
    auto write_end = chrono::steady_clock::now();
    auto dur = chrono::duration_cast<chrono::microseconds>(write_end - write_start).count();
    writer_hist.Add(dur);
    curr_request++;
  }
  auto end = chrono::steady_clock::now();
  cout << "Finished test" << endl;

  Histogram readers_hist;
  readers_hist.Clear();
  uint64_t read_count = 0;
  int read_size = 0;
  for (auto &w : workers) {
    w.stop();
    readers_hist.Merge(w.get_histogram());
    auto sizes = w.get_sizes();
    read_count += sizes.first;
    read_size += sizes.second;
  }

  for (auto &t : threads) t.join();

  auto dur = chrono::duration_cast<chrono::milliseconds>(end - start).count();
  cout << "Test duration: " << dur << " milliseconds" << endl;
  cout << "Read rate: " << setprecision(15) << floor((double)read_count / dur * 1000) << " reads/sec => ";
  cout << setprecision(15) << floor((double)read_size / dur * 1000) << " bytes/sec"
       << ", total bytes read: " << read_size << endl;
  cout << "Readers details (microseconds): " << endl << readers_hist.ToString() << endl;
  cout << "Write rate: " << setprecision(15) << floor((double)num_of_requests / dur * 1000) << " writes/sec => ";
  cout << setprecision(15) << floor((double)write_size / dur * 1000) << " bytes/sec"
       << ", total bytes written: " << write_size << endl;
  cout << "Writer details (microseconds):" << endl << writer_hist.ToString() << endl;
  return 0;
}
