// Copyright 2020 VMware, all rights reserved

#include "thin_replica_client.hpp"

#include <log4cplus/configurator.h>
#include "gtest/gtest.h"
#include "thin_replica_client_mocks.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::MockThinReplicaStub;
using std::chrono::operator""ms;
using std::condition_variable;
using std::make_shared;
using std::make_unique;
using std::mutex;
using std::pair;
using std::shared_ptr;
using std::string;
using std::thread;
using std::to_string;
using std::unique_ptr;
using std::vector;
using std::chrono::milliseconds;
using std::this_thread::sleep_for;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::Update;
using thin_replica_client::UpdateQueue;

const string kTestingClientID = "0";
const string kUnimplementedPrivateKey = "";
const uint16_t kTestingMaxDataReadTimeout = 1;
const uint16_t kTestingMaxHashReadTimeout = 1;
const string kTestingJaegerAddress = "127.0.0.1:6831";
const milliseconds kBriefDelayDuration = 10ms;

namespace {

TEST(thin_replica_client_test, test_destructor_always_successful) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_NO_THROW(trc.reset()) << "ThinReplicaClient destructor failed.";
  update_queue->Clear();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  trc->Subscribe("");
  update_queue->Pop();
  update_queue->Pop();
  EXPECT_NO_THROW(trc.reset())
      << "ThinReplicaClient destructor failed when destructing a "
         "ThinReplicaClient with an active subscription.";
  update_queue->Clear();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  trc->Subscribe("");
  update_queue->Pop();
  update_queue->Pop();
  trc->Unsubscribe();
  EXPECT_NO_THROW(trc.reset())
      << "ThinReplicaClient destructor failed when destructing a "
         "ThinReplicaClient after ending its subscription.";
}

TEST(thin_replica_client_test, test_1_parameter_subscribe_success_cases) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_NO_THROW(trc->Subscribe(""))
      << "ThinReplicaClient::Subscribe's 1-parameter overload failed.";

  trc->Unsubscribe();
  EXPECT_NO_THROW(trc->Subscribe(""))
      << "ThinReplicaClient::Subscribe's 1-parameter overload failed when "
         "subscribing after closing a subscription.";
  EXPECT_NO_THROW(trc->Subscribe(""))
      << "ThinReplicaClient::Subscribe's 1-parameter overload failed when "
         "subscribing while there is an ongoing subscription.";

  EXPECT_NO_THROW(trc->Subscribe("k"))
      << "ThinReplicaClient::Subscribe's 1-parameter overload failed when "
         "subscribing with a non-empty key prefix of an existing key.";
  EXPECT_NO_THROW(trc->Subscribe("key"))
      << "ThinReplicaClient::Subscribe's 1-parameter overload failed when "
         "subscribing with a key prefix entirely matching an existing key.";
  EXPECT_NO_THROW(trc->Subscribe("There should be no key with this prefix."))
      << "ThinReplicaClient::Subscribe's 1-parameter overload failed when "
         "subscribing with a key prefix not matching any existing key.";
}

TEST(thin_replica_client_test, test_2_parameter_subscribe_success_cases) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  trc->Subscribe("");
  unique_ptr<Update> update_received = update_queue->Pop();
  uint64_t block_id = update_received->block_id;
  trc->Unsubscribe();
  EXPECT_NO_THROW(trc->Subscribe("", block_id))
      << "ThinReplicaClient::Subscribe's 2-parameter overload failed.";
  trc->Unsubscribe();

  trc->Unsubscribe();
  EXPECT_NO_THROW(trc->Subscribe("", block_id))
      << "ThinReplicaClient::Subscribe's 2-parameter overload failed after "
         "closing an existing subscription.";
  EXPECT_NO_THROW(trc->Subscribe("", block_id))
      << "ThinReplicaClient::Subscribe's 2-parameter overload failed when "
         "there is already an existing subscription.";

  EXPECT_NO_THROW(trc->Subscribe("k", block_id))
      << "ThinReplicaClient::Subscribe's 2-parameter overload failed when "
         "subscribing with a non-empty prefix of an existing key.";
  EXPECT_NO_THROW(trc->Subscribe("key", block_id))
      << "ThinReplicaClient::Subscribe's 2-parameter overload failed failed "
         "when subscribing with a prefix that entirely matches an existing "
         "key.";
  EXPECT_NO_THROW(
      trc->Subscribe("There should be no key with this prefix.", block_id))
      << "ThinReplicaClient::Subscribe's 2-parameter overload failed when "
         "subscribing with a prefix that matches no existing key.";

  for (size_t i = 0; i < 8; ++i) {
    trc->Unsubscribe();
    update_queue->Clear();
    uint64_t previous_block_id = block_id;
    EXPECT_NO_THROW(trc->Subscribe("", block_id))
        << "ThinReplicaClient::Subscribe's 2-parameter overload failed when "
           "subscribing with a Block ID from a previously received block.";
    update_received = update_queue->Pop();
    block_id = update_received->block_id;
    EXPECT_GT(block_id, previous_block_id)
        << "ThinReplicaClient::Subscribe's 2-parameter overload appears to be "
           "repeating already received blocks even when specifying where to "
           "start the subscription to avoid them.";
  }
}

TEST(thin_replica_client_test,
     test_1_parameter_subscribe_to_unresponsive_servers_fails) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;
  size_t num_unresponsive = num_replicas - max_faulty;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  SetSomeMockServersUnresponsive(mock_servers, num_unresponsive);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_ANY_THROW(trc->Subscribe(""))
      << "ThinReplicaClient::Subscribe's 1-parameter overload doesn't throw an "
         "exception when trying to subscribe to a cluster with only max_faulty "
         "servers responsive.";
  trc.reset();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerUnresponsive(mock_servers);
  trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_ANY_THROW(trc->Subscribe(""))
      << "ThinReplicaClient::Subscribe's 1-parameter overload doesn't throw an "
         "exception when trying to subscribe to a cluster with no responsive "
         "servers.";
}

TEST(thin_replica_client_test, test_unsubscribe_successful) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_NO_THROW(trc->Unsubscribe())
      << "ThinReplicaClient::Unsubscribe failed for a newly-constructed "
         "ThinReplicaClient.";
  trc->Subscribe("");
  update_queue->Pop();
  update_queue->Pop();
  EXPECT_NO_THROW(trc->Unsubscribe())
      << "ThinReplicaClient::Unsubscribe failed for a ThinReplicaClient with "
         "an active subscription.";
  EXPECT_NO_THROW(trc->Unsubscribe())
      << "ThinReplicaClient::Unsubscribe failed for a ThinReplicaClient with a "
         "subscription that had already been cancelled.";
}

TEST(thin_replica_client_test, test_pop_fetches_updates_) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> base_stream_preparer(
      new RepeatedMockDataStreamPreparer(update, 1));
  auto delay_condition = make_shared<condition_variable>();
  auto spurious_wakeup_indicator = make_shared<bool>(true);
  auto delay_condition_mutex = make_shared<mutex>();
  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new DelayedMockDataStreamPreparer(base_stream_preparer, delay_condition,
                                        spurious_wakeup_indicator,
                                        delay_condition_mutex));
  MockOrderedDataStreamHasher hasher(base_stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  trc->Subscribe("");
  unique_ptr<Update> update_received = update_queue->Pop();
  EXPECT_TRUE((bool)update_received)
      << "ThinReplicaClient failed to publish update from initial state.";

  thread delay_thread([&]() {
    sleep_for(kBriefDelayDuration);
    *spurious_wakeup_indicator = false;
    delay_condition->notify_one();
  });
  update_received = update_queue->Pop();
  EXPECT_TRUE((bool)update_received)
      << "ThinReplicaClient failed to publish update received from servers "
         "while the application is already waiting on the update queue.";
  delay_thread.join();

  // The current implementation of the ThinReplicaClient may block on Read calls
  // trying to join threads before it completes its destructor, so we unblock
  // any such calls here.
  *spurious_wakeup_indicator = false;
  sleep_for(kBriefDelayDuration);
  delay_condition->notify_all();
}

TEST(thin_replica_client_test, test_acknowledge_block_id_success) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key");
  update_data->set_value("value");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_NO_THROW(trc->AcknowledgeBlockID(1))
      << "ThinReplicaClient::AcknowledgeBlockID fails when called on a "
         "freshly-constructed ThinReplicaClient.";
  trc->Subscribe("");
  update_queue->Pop();
  update_queue->Pop();
  EXPECT_NO_THROW(trc->AcknowledgeBlockID(2))
      << "ThinReplicaClient::AcknowledgeBlockID fails when called on a "
         "ThinReplicaClient with an active subscription.";
  trc->Unsubscribe();
  EXPECT_NO_THROW(trc->AcknowledgeBlockID(3))
      << "ThinReplicaClient::AcknowledgeBlockID fails when called on a "
         "ThinReplicaClient with an ended subscription.";
  for (uint64_t block_id = 0; block_id <= UINT32_MAX;
       block_id += (block_id + 1)) {
    EXPECT_NO_THROW(trc->AcknowledgeBlockID(block_id))
        << "ThinReplicaClient::AcknowledgeBlockID fails when called with "
           "arbitrary block IDs.";
  }
}

TEST(thin_replica_client_test, test_correct_data_returned_) {
  vector<Data> update_data;
  for (size_t i = 0; i <= 60; ++i) {
    Data update;
    update.set_block_id(i);
    for (size_t j = 1; j <= 12; ++j) {
      if (i % j == 0) {
        KVPair* kvp = update.add_data();
        kvp->set_key("key" + to_string(j));
        kvp->set_value("value" + to_string(i / j));
      }
    }
    update_data.push_back(update);
  }
  size_t num_initial_updates = 6;

  shared_ptr<MockDataStreamPreparer> base_stream_preparer(
      new VectorMockDataStreamPreparer(update_data, num_initial_updates));
  auto delay_condition = make_shared<condition_variable>();
  auto spurious_wakeup_indicator = make_shared<bool>(true);
  auto delay_condition_mutex = make_shared<mutex>();
  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new DelayedMockDataStreamPreparer(base_stream_preparer, delay_condition,
                                        spurious_wakeup_indicator,
                                        delay_condition_mutex));
  MockOrderedDataStreamHasher hasher(base_stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);
  EXPECT_FALSE((bool)(update_queue->TryPop()))
      << "ThinReplicaClient appears to have published state to update queue "
         "prior to subscription.";

  trc->Subscribe("");
  trc->Unsubscribe();
  for (size_t i = 0; i < num_initial_updates; ++i) {
    unique_ptr<Update> received_update = update_queue->TryPop();
    Data& expected_update = update_data[i];
    EXPECT_TRUE((bool)received_update)
        << "ThinReplicaClient failed to fetch an expected update included in "
           "the initial state.";
    EXPECT_EQ(received_update->block_id, expected_update.block_id())
        << "An update the ThinReplicaClient fetched in the initial state has "
           "an incorrect block ID.";
    EXPECT_EQ(received_update->kv_pairs.size(), expected_update.data_size())
        << "An update the ThinReplicaClient fetched in the initial state has "
           "an incorrect number of KV-pair updates.";
    for (size_t j = 0; j < received_update->kv_pairs.size() &&
                       j < expected_update.data_size();
         ++j) {
      EXPECT_EQ(received_update->kv_pairs[j].first,
                expected_update.data(j).key())
          << "A key in an update the ThinReplicaClient fetched in the initial "
             "state does not match its expected value.";
      EXPECT_EQ(received_update->kv_pairs[j].second,
                expected_update.data(j).value())
          << "A value in an update the ThinReplicaClient fetched in the "
             "initial state does not match its expected value.";
    }
  }

  EXPECT_FALSE((bool)(update_queue->TryPop()))
      << "ThinReplicaClient appears to have collected an unexpected number of "
         "updates in its initial state.";
  *spurious_wakeup_indicator = false;
  delay_condition->notify_all();
  sleep_for(kBriefDelayDuration);
  EXPECT_FALSE((bool)(update_queue->TryPop()))
      << "ThinReplicaClient appears to have received an update after "
         "unsubscribing.";
  *spurious_wakeup_indicator = true;

  trc->Subscribe("", num_initial_updates - 1);
  for (size_t i = num_initial_updates; i < update_data.size(); ++i) {
    *spurious_wakeup_indicator = false;
    delay_condition->notify_one();
    unique_ptr<Update> received_update = update_queue->Pop();
    *spurious_wakeup_indicator = true;
    Data& expected_update = update_data[i];

    EXPECT_TRUE((bool)received_update)
        << "ThinReplicaClient failed to fetch an expected update from an "
           "ongoing subscription.";
    EXPECT_EQ(received_update->block_id, expected_update.block_id())
        << "An update the ThinReplicaClient received from an ongoing "
           "subscription has an incorrect Block ID.";
    EXPECT_EQ(received_update->kv_pairs.size(), expected_update.data_size())
        << "An update the ThinReplicaClient received in an ongoing "
           "subscription has an incorrect number of KV-pair updates.";
    for (size_t j = 0; j < received_update->kv_pairs.size() &&
                       j < expected_update.data_size();
         ++j) {
      EXPECT_EQ(received_update->kv_pairs[j].first,
                expected_update.data(j).key())
          << "A key in an update the ThinReplicaClient received in an ongoing "
             "subscription does not match its expected value.";
      EXPECT_EQ(received_update->kv_pairs[j].second,
                expected_update.data(j).value())
          << "A value in an update the ThinReplicaClient received in an "
             "ongoing subscription does not match its expected value.";
    }
  }

  // The current implementation of the ThinReplicaClient may block on Read calls
  // trying to join threads before it completes its destructor, so we unblock
  // any such calls here.
  *spurious_wakeup_indicator = false;
  sleep_for(kBriefDelayDuration);
  delay_condition->notify_all();
}

TEST(thin_replica_client_test, test_key_filtration) {
  Data update;
  update.set_block_id(0);
  KVPair* update_data = update.add_data();
  update_data->set_key("key0");
  update_data->set_value("value0");
  update_data = update.add_data();
  update_data->set_key("key1");
  update_data->set_value("value1");
  update_data = update.add_data();
  update_data->set_key("identifier");
  update_data->set_value("value2");

  shared_ptr<MockDataStreamPreparer> stream_preparer(
      new RepeatedMockDataStreamPreparer(update));
  MockOrderedDataStreamHasher hasher(stream_preparer);

  vector<pair<string, unique_ptr<MockThinReplicaStub>>> mock_servers;
  uint16_t max_faulty = 1;
  size_t num_replicas = 3 * max_faulty + 1;

  shared_ptr<UpdateQueue> update_queue = make_shared<BasicUpdateQueue>();

  InstantiateMockServers(mock_servers, num_replicas);
  SetMockServerBehavior(mock_servers, stream_preparer, hasher);
  auto trc = make_unique<ThinReplicaClient>(
      kTestingClientID, update_queue, max_faulty, kUnimplementedPrivateKey,
      mock_servers.begin(), mock_servers.end(), kTestingMaxDataReadTimeout,
      kTestingMaxHashReadTimeout, kTestingJaegerAddress);

  trc->Subscribe("");
  unique_ptr<Update> update_received = update_queue->Pop();
  EXPECT_EQ(update_received->kv_pairs[0].first, "key0")
      << "ThinReplicaClient fails to include a key-value pair in an update "
         "when subscribing with an empty prefix.";
  EXPECT_EQ(update_received->kv_pairs[1].first, "key1")
      << "ThinReplicaClient fails to include a key-value pair in an update "
         "when subscribing with an empty prefix.";
  EXPECT_EQ(update_received->kv_pairs[2].first, "identifier")
      << "ThinReplicaClient fails to include a key-value pair in an update "
         "when subscribing with an empty prefix.";

  trc->Unsubscribe();
  trc->Subscribe("key");
  update_received = update_queue->Pop();
  EXPECT_EQ(update_received->kv_pairs[0].first, "key0")
      << "ThinReplicaClient fails to include a key-value pair in an update "
         "when subscribing with a key prefix that matches that pair.";
  EXPECT_EQ(update_received->kv_pairs[1].first, "key1")
      << "ThinReplicaClient fails to include a key-value pair in an update "
         "when subscribing with a key prefix that matches that pair.";
  EXPECT_EQ(update_received->kv_pairs.size(), 2)
      << "ThinReplicaClient appears to include key-value pair(s) in an update "
         "that do not match the requested prefix.";

  trc->Unsubscribe();
  trc->Subscribe("key1");
  update_received = update_queue->Pop();
  EXPECT_EQ(update_received->kv_pairs[0].first, "key1")
      << "ThinReplicaClient fails to include a key-value pair in an update "
         "when subscribing with a key prefix that matches the full length of "
         "the key.";
  EXPECT_EQ(update_received->kv_pairs.size(), 1)
      << "ThinReplicaClient appears to include key-value pair(s) in an update "
         "that do not match the requested prefix.";

  trc->Unsubscribe();
  trc->Subscribe("inexistant_key");
  update_received = update_queue->Pop();
  EXPECT_EQ(update_received->kv_pairs.size(), 0)
      << "ThinReplicaClient appears to include key-value pair(s) in an update "
         "that do not match the requested prefix.";
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  log4cplus::BasicConfigurator config;
  config.configure();
  return RUN_ALL_TESTS();
}
