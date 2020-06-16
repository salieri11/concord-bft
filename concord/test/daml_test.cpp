// Copyright 2020 VMware, all rights reserved

#include <concord.pb.h>
#include <daml_commit.pb.h>
#include <google/protobuf/util/time_util.h>
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <daml/daml_kvb_commands_handler.hpp>
#include <daml/daml_validator_client.hpp>
#include "OpenTracing.hpp"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "mocks.hpp"
#include "time/time_contract.hpp"

using namespace std::placeholders;  // For _1, _2, etc.

using ::testing::_;
using ::testing::AtLeast;
using ::testing::Cardinality;
using ::testing::DoAll;
using ::testing::Exactly;
using ::testing::Return;
using ::testing::ReturnRef;
using ::testing::SetArgPointee;
using ::testing::SetArgReferee;

using namespace concord::kvbc;
using namespace concord::daml;
using namespace concordUtils;

namespace da_kvbc = com::digitalasset::kvbc;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::PreExecutionResult;
using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;

using google::protobuf::Timestamp;
using google::protobuf::util::TimeUtil;

namespace {

constexpr size_t OUT_BUFFER_SIZE = 512000;

const auto NEVER = Exactly(0);

ConcordRequest build_commit_request();
std::shared_ptr<MockPrometheusRegistry> build_mock_prometheus_registry();
std::unique_ptr<MockDamlValidatorClient> build_mock_daml_validator_client(
    bool expect_never = false,
    const std::optional<Timestamp>& max_record_time = std::nullopt);
ConcordResponse build_pre_execution_response(
    BlockId pre_execution_block_id,
    const std::optional<Timestamp>& max_record_time = std::nullopt,
    const std::vector<std::string>& reads = {"read-key"},
    const std::map<std::string, std::string>& writes = {{"write-key",
                                                         "write-value"}},
    const std::map<std::string, std::string>& timeout_writes = {{"timeout",
                                                                 "detected"}},
    const std::map<std::string, std::string>& conflict_writes = {
        {"conflict", "detected"}});

TEST(daml_test, validate_commit_command_type) {
  da_kvbc::Command daml_commit_command;
  da_kvbc::CommitRequest commit_request;
  commit_request.set_participant_id("participant_id");
  commit_request.set_correlation_id("correlation_id");
  daml_commit_command.mutable_commit()->MergeFrom(commit_request);

  EXPECT_EQ(daml_commit_command.cmd_case(), da_kvbc::Command::kCommit);
}

TEST(daml_test, validate_read_command_type) {
  da_kvbc::Command daml_read_command;
  da_kvbc::ReadCommand read_command;
  read_command.set_participant_id("participant_id");
  daml_read_command.mutable_read()->MergeFrom(read_command);

  EXPECT_EQ(daml_read_command.cmd_case(), da_kvbc::Command::kRead);
}

TEST(daml_test, successful_commit_creates_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, false);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client = build_mock_daml_validator_client();

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry};

  ConcordRequest concord_request = build_commit_request();

  std::string req_string;
  concord_request.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  concordUtils::SpanWrapper span;
  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::EMPTY_FLAGS, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 0);
  ASSERT_TRUE(concord_response.has_daml_response());
}

TEST(daml_test, pre_execute_commit_no_new_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _)).Times(NEVER);

  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, false);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client =
      build_mock_daml_validator_client(false, TimeUtil::GetCurrentTime());

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry};

  ConcordRequest concord_request = build_commit_request();

  std::string req_string;
  concord_request.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  concordUtils::SpanWrapper span;
  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::PRE_PROCESS_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 0);
  ASSERT_FALSE(concord_response.has_daml_response());
  ASSERT_TRUE(concord_response.has_pre_execution_result());
  ASSERT_FALSE(concord_response.pre_execution_result().has_max_record_time());
  ASSERT_EQ(
      concord_response.pre_execution_result().write_set().kv_writes_size(), 2);
  ASSERT_EQ(concord_response.pre_execution_result()
                .conflict_write_set()
                .kv_writes_size(),
            0);
  ASSERT_EQ(concord_response.pre_execution_result()
                .timeout_write_set()
                .kv_writes_size(),
            0);
  ASSERT_EQ(concord_response.pre_execution_result().read_set().keys_size(), 3);
  ASSERT_EQ(concord_response.pre_execution_result().read_set_version(),
            last_block_id);
  ASSERT_EQ(concord_response.pre_execution_result().request_correlation_id(),
            "correlation_id");
}

TEST(daml_test, valid_pre_execution_creates_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));
  EXPECT_CALL(ro_storage, mayHaveConflictBetween(_, _, _, _))
      .WillRepeatedly(DoAll(SetArgReferee<3>(false), Return(Status::OK())));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, false);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client = build_mock_daml_validator_client(true);

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry};

  ConcordResponse pre_execution_response = build_pre_execution_response(50);

  std::string req_string;
  pre_execution_response.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  concordUtils::SpanWrapper span;
  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 0);
}

TEST(daml_test, conflicting_pre_execution_fails_but_adds_conflict_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));

  EXPECT_CALL(ro_storage, mayHaveConflictBetween(_, _, _, _))
      .WillRepeatedly(DoAll(SetArgReferee<3>(true), Return(Status::OK())));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, false);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client = build_mock_daml_validator_client(true);

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry};

  ConcordResponse pre_execution_response = build_pre_execution_response(50);

  std::string req_string;
  pre_execution_response.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  concordUtils::SpanWrapper span;
  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 1);
}

TEST(daml_test, timed_out_pre_execution_fails_but_adds_timeout_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));

  EXPECT_CALL(ro_storage, mayHaveConflictBetween(_, _, _, _))
      .WillRepeatedly(DoAll(SetArgReferee<3>(false), Return(Status::OK())));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, true);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client = build_mock_daml_validator_client(true);

  auto* time_contract =
      new MockTimeContract{ro_storage, config, TimeUtil::GetCurrentTime()};

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry,
                                               time_contract};

  Timestamp long_expired_max_record_time;
  TimeUtil::FromString("2019-01-01T10:00:20.021Z",
                       &long_expired_max_record_time);
  ConcordResponse pre_execution_response =
      build_pre_execution_response(50, long_expired_max_record_time);

  std::string req_string;
  pre_execution_response.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  concordUtils::SpanWrapper span;
  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 1);
}

TEST(daml_test, conflicting_write_set_fails_but_adds_conflict_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));

  const Sliver rk{std::string{"read-key"}};
  const Sliver wk{std::string{"write-key"}};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));
  EXPECT_CALL(ro_storage, mayHaveConflictBetween(rk, _, _, _))
      .WillOnce(DoAll(SetArgReferee<3>(false), Return(Status::OK())));
  EXPECT_CALL(ro_storage, mayHaveConflictBetween(wk, _, _, _))
      .WillOnce(DoAll(SetArgReferee<3>(true), Return(Status::OK())));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, false);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client = build_mock_daml_validator_client(true);

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry};

  ConcordResponse pre_execution_response = build_pre_execution_response(50);

  std::string req_string;
  pre_execution_response.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  concordUtils::SpanWrapper span;
  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 1);
}

TEST(daml_test, conflicting_write_of_new_key) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  MockLocalKeyValueStorageReadOnly ro_storage{};
  EXPECT_CALL(ro_storage, getLastBlock()).WillRepeatedly(Return(last_block_id));

  SubBufferList subscriber_list{};

  const auto replicas = 4;
  const auto client_proxies = 4;
  const auto config =
      TestConfiguration(replicas, client_proxies, 0, 0, false, false);

  auto prometheus_registry = build_mock_prometheus_registry();
  auto daml_validator_client = build_mock_daml_validator_client(true);

  DamlKvbCommandsHandler daml_commands_handler{config,
                                               GetNodeConfig(config, 1),
                                               ro_storage,
                                               blocks_appender,
                                               subscriber_list,
                                               std::move(daml_validator_client),
                                               prometheus_registry};

  ConcordResponse pre_execution_response_1 =
      build_pre_execution_response(90, std::nullopt, {}, {{"k", "v1"}});
  ConcordResponse pre_execution_response_2 =
      build_pre_execution_response(91, std::nullopt, {}, {{"k", "v2"}});

  const Sliver k{std::string{"k"}};
  EXPECT_CALL(ro_storage, mayHaveConflictBetween(k, 90 + 1, _, _))
      .WillOnce(DoAll(SetArgReferee<3>(false), Return(Status::OK())));

  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(2)
      .WillRepeatedly(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  EXPECT_CALL(ro_storage, mayHaveConflictBetween(k, 91 + 1, _, _))
      .WillOnce(DoAll(SetArgReferee<3>(true), Return(Status::OK())));

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  std::string req_string;
  int result;
  ConcordResponse concord_response;

  pre_execution_response_1.SerializeToString(&req_string);
  concordUtils::SpanWrapper span;
  result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);
  concord_response.ParseFromArray(reply_buffer, reply_size);
  ASSERT_EQ(result, 0);

  memset(reply_buffer, 0, OUT_BUFFER_SIZE);
  pre_execution_response_2.SerializeToString(&req_string);

  result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size, span);

  concord_response.ParseFromArray(reply_buffer, reply_size);
  ASSERT_EQ(result, 1);
}

class MockReadingFromStorage {
 public:
  MockReadingFromStorage() = default;

  MOCK_METHOD1(Read,
               std::map<std::string, std::string>(
                   const google::protobuf::RepeatedPtrField<std::string>&));
};

TEST(write_collecting_storage_operations, delegates_read) {
  MockReadingFromStorage mock_reader;
  std::map<std::string, std::string> expected_result = {{"key1", "value1"},
                                                        {"key2", "value2"}};
  EXPECT_CALL(mock_reader, Read(_)).WillOnce(Return(expected_result));
  DamlKvbReadFunc kvb_read = [&](const auto& keys) {
    return mock_reader.Read(keys);
  };
  auto ops = new WriteCollectingStorageOperations(kvb_read);
  google::protobuf::RepeatedPtrField<std::string> requested_keys;
  requested_keys.Add("key1");
  requested_keys.Add("key2");

  EXPECT_EQ(expected_result, ops->Read(requested_keys));
}

TEST(write_collecting_storage_operations, reads_first_from_updates) {
  MockReadingFromStorage mock_reader;
  EXPECT_CALL(mock_reader, Read(_)).Times(NEVER);
  DamlKvbReadFunc kvb_read = [&](const auto& keys) {
    return mock_reader.Read(keys);
  };
  auto ops = new WriteCollectingStorageOperations(kvb_read);
  google::protobuf::RepeatedPtrField<std::string> requested_key;
  requested_key.Add("key");

  ops->Write("key", "value", {});
  std::map<std::string, std::string> expected_result = {{"key", "value"}};
  EXPECT_EQ(expected_result, ops->Read(requested_key));
}

TEST(write_collecting_storage_operations, merges_reads_with_updates) {
  MockReadingFromStorage mock_reader;
  std::map<std::string, std::string> expected_result = {{"key2", "value2"}};
  EXPECT_CALL(mock_reader, Read(_)).WillOnce(Return(expected_result));
  DamlKvbReadFunc kvb_read = [&](const auto& keys) {
    return mock_reader.Read(keys);
  };
  auto ops = new WriteCollectingStorageOperations(kvb_read);
  google::protobuf::RepeatedPtrField<std::string> requested_keys;
  requested_keys.Add("key1");

  ops->Write("key1", "value1", {});
  std::map<std::string, std::string> expected_one_value = {{"key1", "value1"}};
  EXPECT_EQ(expected_one_value, ops->Read(requested_keys));
  requested_keys.Add("key2");
  std::map<std::string, std::string> expected_two_values = {{"key1", "value1"},
                                                            {"key2", "value2"}};
  EXPECT_EQ(expected_two_values, ops->Read(requested_keys));
}

std::unique_ptr<MockDamlValidatorClient> build_mock_daml_validator_client(
    bool expect_never, const std::optional<Timestamp>& max_record_time) {
  std::unique_ptr<MockDamlValidatorClient> daml_validator_client =
      std::make_unique<MockDamlValidatorClient>();

  if (expect_never) {
    EXPECT_CALL(*daml_validator_client, Validate(_, _, _, _, _, _, _))
        .Times(Exactly(0));
  } else {
    EXPECT_CALL(*daml_validator_client, Validate(_, _, _, _, _, _, _))
        .Times(1)
        .WillOnce(testing::Invoke(
            [](const std::string& submission,
               const google::protobuf::Timestamp& record_time,
               const std::string& participant_id,
               const std::string& correlation_id,
               opentracing::Span& parent_span,
               std::vector<std::string>& out_read_set,
               KeyValueStorageOperations& storage_operations) -> grpc::Status {
              out_read_set.push_back("rk1");
              out_read_set.push_back("rk2");
              out_read_set.push_back("rk3");
              std::vector<std::string> replicas;
              replicas.push_back("replica1");
              storage_operations.Write("wk1", "wv1", replicas);
              storage_operations.Write("wk2", "wv2", replicas);
              return grpc::Status();
            }));
  }

  return daml_validator_client;
}

std::shared_ptr<MockPrometheusRegistry> build_mock_prometheus_registry() {
  auto prometheus_registry = std::make_shared<MockPrometheusRegistry>();
  auto default_counter_family =
      prometheus::Family<prometheus::Counter>("a", "b", {});
  auto default_counter = prometheus::Counter();

  EXPECT_CALL(*prometheus_registry, createCounterFamily(_, _, _))
      .WillRepeatedly(ReturnRef(default_counter_family));

  EXPECT_CALL(*prometheus_registry, createCounter(_, _))
      .WillRepeatedly(ReturnRef(default_counter));
  return prometheus_registry;
}

ConcordRequest build_commit_request() {
  ConcordRequest concord_request;
  DamlRequest daml_request;

  da_kvbc::Command daml_commit_command;
  da_kvbc::CommitRequest commit_request;
  commit_request.set_participant_id("participant_id");
  commit_request.set_correlation_id("correlation_id");
  daml_commit_command.mutable_commit()->MergeFrom(commit_request);
  std::string cmd_string;
  daml_commit_command.SerializeToString(&cmd_string);

  daml_request.set_command(cmd_string);

  concord_request.mutable_daml_request()->MergeFrom(daml_request);
  return concord_request;
}

ConcordResponse build_pre_execution_response(
    BlockId pre_execution_block_id,
    const std::optional<Timestamp>& max_record_time,
    const std::vector<std::string>& reads,
    const std::map<std::string, std::string>& writes,
    const std::map<std::string, std::string>& timeout_writes,
    const std::map<std::string, std::string>& conflict_writes) {
  ConcordResponse pre_execution_response;
  PreExecutionResult pre_execution_result;

  pre_execution_result.set_request_correlation_id("correlation_id");
  pre_execution_result.set_read_set_version(pre_execution_block_id);

  if (max_record_time) {
    pre_execution_result.mutable_max_record_time()->CopyFrom(
        max_record_time.value());
  }
  auto* read_set = pre_execution_result.mutable_read_set();
  for (const auto& r_key : reads) {
    read_set->add_keys(r_key.c_str(), r_key.size());
  }

  auto* write_set = pre_execution_result.mutable_write_set();
  for (const auto& kv : writes) {
    auto* new_kv = write_set->add_kv_writes();
    new_kv->set_key(kv.first.c_str(), kv.first.size());
    new_kv->set_value(kv.second.c_str(), kv.second.size());
  }

  auto* timeout_write_set = pre_execution_result.mutable_timeout_write_set();
  for (const auto& kv : timeout_writes) {
    auto* new_kv = timeout_write_set->add_kv_writes();
    new_kv->set_key(kv.first.c_str(), kv.first.size());
    new_kv->set_value(kv.second.c_str(), kv.second.size());
  }

  auto* conflict_write_set = pre_execution_result.mutable_conflict_write_set();
  for (const auto& kv : conflict_writes) {
    auto* new_kv = conflict_write_set->add_kv_writes();
    new_kv->set_key(kv.first.c_str(), kv.first.size());
    new_kv->set_value(kv.second.c_str(), kv.second.size());
  }

  pre_execution_response.mutable_pre_execution_result()->MergeFrom(
      pre_execution_result);

  return pre_execution_response;
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  auto& hierarchy = log4cplus::Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  log4cplus::BasicConfigurator config{hierarchy, false};
  config.configure();
  return RUN_ALL_TESTS();
}
