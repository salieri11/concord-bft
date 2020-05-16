// Copyright 2020 VMware, all rights reserved

#include <log4cplus/configurator.h>
#include "gtest/gtest.h"

#include "trc_hash.hpp"

using com::vmware::concord::thin_replica::Data;
using std::make_pair;
using std::to_string;
using thin_replica_client::HashUpdate;
using thin_replica_client::Update;

namespace {

TEST(trc_hash, hash_update) {
  Update update;
  update.block_id = 1337;
  for (int i = 0; i < 3; ++i) {
    update.kv_pairs.push_back(make_pair(to_string(i), to_string(i)));
  }

  EXPECT_EQ(HashUpdate(update), 122335372282450226);
}

TEST(trc_hash, hash_data) {
  Data update;
  update.set_block_id(1337);
  for (int i = 0; i < 3; ++i) {
    auto data = update.add_data();
    data->set_key(to_string(i));
    data->set_value(to_string(i));
  }

  EXPECT_EQ(HashUpdate(update), 122335372282450226);
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  log4cplus::BasicConfigurator config;
  config.configure();
  return RUN_ALL_TESTS();
}
