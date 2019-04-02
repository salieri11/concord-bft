// Copyright 2018 VMware, all rights reserved
//
// Basic ineraction with RocksDb. This tool is used by SimpleStateTransferTest
// to compare raw rocksdb data from replicas.
// It support 2 operation (OpType) : getRaw - returns full block data in hex
// string format and getDigest - returns hash of the block which is short.
// The -p parameter maybe single block number or range in to:from format
#define USE_ROCKSDB 1
#include <keccak.h>
#include <log4cplus/configurator.h>
#include <iomanip>
#include <iostream>
#include <string>
#include "consensus/kvb/BlockchainDBAdapter.h"
#include "consensus/kvb/Comparators.h"
#include "consensus/kvb/RocksDBClient.h"

using namespace std;
using namespace Blockchain;

enum class OpType { GetBlockRaw, GetBlockDigest };

unordered_map<string, OpType> opTypes = {
    {"getRaw", OpType::GetBlockRaw},
    {"getDigest", OpType::GetBlockDigest},
};

string get_arg_value(string arg) {
  int idx = arg.find("=");
  return arg.substr(idx + 1, arg.length() - idx - 1);
}

bool get_block(BlockId id, BlockchainDBAdapter adapter, Sliver &res) {
  bool found = false;
  adapter.getBlockById(id, res, found);
  return found;
}

std::vector<Sliver> get_data(BlockId from, BlockId to,
                             BlockchainDBAdapter adapter) {
  std::vector<Sliver> result;
  for (BlockId i = from; i <= to; i++) {
    Sliver res;
    bool found = get_block(i, adapter, res);
    if (found) {
      result.push_back(res);
    }
  }

  return result;
}

void compute_digest(uint8_t *data, size_t length, uint8_t *output,
                    size_t outputLenght, size_t &actualOutputLength) {
  assert(output);
  assert(outputLenght >= CryptoPP::Keccak_256::DIGESTSIZE);
  CryptoPP::Keccak_256 keccak;
  actualOutputLength = CryptoPP::Keccak_256::DIGESTSIZE;
  keccak.CalculateDigest(output, data, length);
}

void print_result(vector<Sliver> &results,
                  void (*transform)(uint8_t *, size_t, uint8_t *, size_t,
                                    size_t &) = nullptr) {
  if (results.size() == 0) {
    cout << "Not found" << endl << "Total size :0" << endl;
  } else {
    int totalSize = 0;
    for (auto &data : results) {
      totalSize += data.length();
      cout << endl
           << "------- start, data size " << data.length() << "---------";
      cout << endl;

      uint8_t printData[1024];
      uint8_t *printPtr = nullptr;
      size_t printLength = 0;
      if (transform) {
        printPtr = printData;
        transform(data.data(), data.length(), printData, 1024, printLength);
      } else {
        printPtr = data.data();
        printLength = data.length();
      }
      for (size_t i = 0; i < printLength; i++) {
        cout << hex << setfill('0') << setw(2) << (int)printPtr[i] << " ";
      }

      cout << dec;
      cout << endl << "------- end, data size " << data.length() << "---------";
      cout << endl;
    }
    cout << "Total size :" << totalSize << endl;
  }
}

int main(int argc, char **argv) {
  using namespace log4cplus;
  initialize();
  BasicConfigurator config;
  config.configure();
  log4cplus::Logger::getRoot().setLogLevel(OFF_LOG_LEVEL);

  if (argc < 2) {
    cout << "Usage: conc_rocksdb_adp"
         << " -path={RocksDbFolder}"
         << " -op={getRaw|getDigest} -p={blockNum|fromBlockNum:toBlockNum}"
         << endl;
    return 0;
  }

  string path = get_arg_value(string(argv[1]));
  string op = get_arg_value(string(argv[2]));
  string p = get_arg_value(string(argv[3]));
  BlockId from, to;
  auto idx = p.find(":");
  if (idx != string::npos) {
    from = stoul(p.substr(0, idx));
    to = stoul(p.substr(idx + 1, p.length() - idx - 1));
  } else {
    from = to = stoul(p);
  }

  assert(path.length() > 0);
  assert(op.length() > 0);
  assert(p.length() > 0);

  auto comp = new Blockchain::RocksKeyComparator();
  auto client = RocksDBClient(path, comp);
  auto adapter = BlockchainDBAdapter(&client);

  if (opTypes.find(op) == opTypes.end()) {
    cout << "Error, operation not supported" << endl;
    goto exit;
  }

  if (opTypes[op] == OpType::GetBlockRaw ||
      opTypes[op] == OpType::GetBlockDigest) {
    client.init(true);
  } else {
    client.init();
  }

  switch (opTypes[op]) {
    case OpType::GetBlockDigest: {
      std::vector<Sliver> results = get_data(from, to, adapter);
      print_result(results, compute_digest);
      break;
    }
    case OpType::GetBlockRaw: {
      std::vector<Sliver> results = get_data(from, to, adapter);
      print_result(results);
      break;
    }
  }

exit:
  client.close();
  delete comp;
  return 0;
}
