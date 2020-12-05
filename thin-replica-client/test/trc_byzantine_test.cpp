// Copyright 2020 VMware, all rights reserved

#include "thin_replica_client.hpp"
#include "trs_connection.hpp"

#include <log4cplus/configurator.h>
#include "gtest/gtest.h"
#include "thin_replica_client_mocks.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::KVPair;
using std::make_shared;
using std::make_unique;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::vector;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::TrsConnection;
using thin_replica_client::Update;
using thin_replica_client::UpdateQueue;

const string kTestingClientID = "mock_client_id";
const string kUnimplementedPrivateKey = "";
const uint16_t kTestingMaxDataReadTimeout = 1;
const uint16_t kTestingMaxHashReadTimeout = 1;
const string kTestingJaegerAddress = "127.0.0.1:6831";

namespace {

TEST(trc_byzantine_test, test_read_state_fabricated_state) {
  vector<Data> update_data;
  for (size_t i = 0; i < 5; ++i) {
    Data update;
    update.set_block_id(i);
    KVPair* kvp = update.add_data();
    kvp->set_key("key" + to_string(i));
    kvp->set_value("value" + to_string(i));
    update_data.push_back(update);
  }
  size_t num_initial_updates = 3;

  size_t update_with_fabrication_index = 1;
  vector<Data> update_data_with_fabrication = update_data;
  KVPair* fabricated_entry =
      update_data_with_fabrication[update_with_fabrication_index].add_data();
  fabricated_entry->set_key("key2");
  fabricated_entry->set_value("value1");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new VectorMockDataStreamPreparer(update_data, num_initial_updates));
  shared_ptr<MockDataStreamPreparer> stream_preparer_with_fabrication(
      new VectorMockDataStreamPreparer(update_data_with_fabrication,
                                       num_initial_updates));
  auto hasher = make_shared<MockOrderedDataStreamHasher>(stream_preparer);

  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;
  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  shared_ptr<ByzantineMockThinReplicaServerPreparer::ByzantineServerBehavior>
      byzantine_behavior(
          new InitialStateFabricator(stream_preparer_with_fabrication));
  ByzantineMockThinReplicaServerPreparer server_preparer(
      stream_preparer, hasher, byzantine_behavior);
  auto mock_servers = CreateByzantineMockServers(num_replicas, server_preparer);
  vector<unique_ptr<TrsConnection>> mock_connections = CreateTrsConnections<
      ByzantineMockThinReplicaServerPreparer::ByzantineMockServer>(
      mock_servers);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      std::move(mock_connections), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);

  trc->Subscribe("");
  for (size_t i = 0; i < update_with_fabrication_index; ++i) {
    update_queue->Pop();
  }
  unique_ptr<Update> received_update = update_queue->Pop();
  ASSERT_TRUE((bool)received_update)
      << "ThinReplicaClient failed to fetch an expected update from the "
         "initial state in the presence of a faulty server injecting "
         "fabricated data to the initial state.";
  EXPECT_EQ(received_update->kv_pairs.size(),
            update_data[update_with_fabrication_index].data_size())
      << "ThinReplicaClient fetched an update with an incorrect number of KV "
         "pairs in the presence of a faulty server injecting a fabricated KV "
         "pair to the initial state.";
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  log4cplus::BasicConfigurator config;
  config.configure();
  return RUN_ALL_TESTS();
}
