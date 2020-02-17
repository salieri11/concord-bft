// Copyright 2020 VMware, all rights reserved

#include <concord.pb.h>
#include <daml_commit.pb.h>
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <daml/daml_kvb_commands_handler.hpp>
#include <daml/daml_validator_client.hpp>
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "hash_defs.h"
#include "mocks.hpp"

using ::testing::_;
using ::testing::AtLeast;
using ::testing::Cardinality;
using ::testing::DoAll;
using ::testing::Exactly;
using ::testing::Return;
using ::testing::ReturnRef;
using ::testing::SetArgPointee;
using ::testing::SetArgReferee;

using namespace concord::storage::blockchain;
using namespace concord::daml;
using namespace concordUtils;

namespace da_kvbc = com::digitalasset::kvbc;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::PreExecutionResult;
using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;

namespace {

constexpr size_t OUT_BUFFER_SIZE = 512000;

const auto NEVER = Exactly(0);

ConcordRequest build_commit_request();
std::shared_ptr<MockPrometheusRegistry> build_mock_prometheus_registry();
std::unique_ptr<MockDamlValidatorClient> build_mock_daml_validator_client(
    bool expect_never = false);
ConcordRequest build_pre_executed_request(
    BlockId pre_execution_block_id,
    const std::vector<std::string>& reads = {"read-key"},
    const std::map<std::string, std::string>& writes = {
        {"write-key", "write-value"}});

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

  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::EMPTY_FLAGS, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);

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

  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::PRE_PROCESS_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 0);
  ASSERT_FALSE(concord_response.has_daml_response());
  ASSERT_TRUE(concord_response.has_pre_execution_result());
  ASSERT_EQ(
      concord_response.pre_execution_result().write_set().kv_writes_size(), 2);
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

  ConcordRequest concord_request = build_pre_executed_request(50);

  std::string req_string;
  concord_request.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 0);
}

TEST(daml_test, conflicting_pre_execution_no_block) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _)).Times(NEVER);

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

  ConcordRequest concord_request = build_pre_executed_request(50);

  std::string req_string;
  concord_request.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer, reply_size);

  ASSERT_EQ(result, 1);
}

TEST(daml_test, conflicting_write_set) {
  const BlockId last_block_id = 100;

  MockBlockAppender blocks_appender{};
  EXPECT_CALL(blocks_appender, addBlock(_, _)).Times(NEVER);

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

  ConcordRequest concord_request = build_pre_executed_request(50);

  std::string req_string;
  concord_request.SerializeToString(&req_string);

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  int result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);

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

  ConcordRequest concord_request_1 =
      build_pre_executed_request(90, {}, {{"k", "v1"}});
  ConcordRequest concord_request_2 =
      build_pre_executed_request(91, {}, {{"k", "v2"}});

  const Sliver k{std::string{"k"}};
  EXPECT_CALL(ro_storage, mayHaveConflictBetween(k, 90 + 1, _, _))
      .WillOnce(DoAll(SetArgReferee<3>(false), Return(Status::OK())));

  EXPECT_CALL(blocks_appender, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(last_block_id + 1), Return(Status::OK())));

  EXPECT_CALL(ro_storage, mayHaveConflictBetween(k, 91 + 1, _, _))
      .WillOnce(DoAll(SetArgReferee<3>(true), Return(Status::OK())));

  uint32_t reply_size = 0;
  char reply_buffer[OUT_BUFFER_SIZE];
  memset(reply_buffer, 0, OUT_BUFFER_SIZE);

  std::string req_string;
  int result;
  ConcordResponse concord_response;

  concord_request_1.SerializeToString(&req_string);
  result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);
  concord_response.ParseFromArray(reply_buffer, reply_size);
  ASSERT_EQ(result, 0);

  memset(reply_buffer, 0, OUT_BUFFER_SIZE);
  concord_request_2.SerializeToString(&req_string);

  result = daml_commands_handler.execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer, reply_size);

  concord_response.ParseFromArray(reply_buffer, reply_size);
  ASSERT_EQ(result, 1);
}

std::unique_ptr<MockDamlValidatorClient> build_mock_daml_validator_client(
    bool expect_never) {
  std::unique_ptr<MockDamlValidatorClient> daml_validator_client =
      std::make_unique<MockDamlValidatorClient>();

  if (expect_never) {
    EXPECT_CALL(*daml_validator_client, ValidateSubmission(_, _, _, _, _, _, _))
        .Times(Exactly(0));
    EXPECT_CALL(*daml_validator_client,
                ValidatePendingSubmission(_, _, _, _, _))
        .Times(Exactly(0));
  } else {
    auto mock_response = std::make_unique<da_kvbc::ValidateResponse>();
    auto need_state = std::make_unique<da_kvbc::ValidateResponse_NeedState>();
    mock_response->mutable_need_state()->MergeFrom(*need_state);
    EXPECT_CALL(*daml_validator_client, ValidateSubmission(_, _, _, _, _, _, _))
        .Times(1)
        .WillOnce(
            DoAll(SetArgPointee<6>(*mock_response), Return(grpc::Status())));

    auto mock_response2 =
        std::make_unique<da_kvbc::ValidatePendingSubmissionResponse>();
    auto result2 = std::make_unique<da_kvbc::Result>();
    mock_response2->mutable_result()->MergeFrom(*result2);

    auto* result = mock_response2->mutable_result();
    da_kvbc::ProtectedKeyValuePair* u1 = result->add_updates();
    u1->set_key("wk1");
    u1->set_value("wk1");
    da_kvbc::ProtectedKeyValuePair* u2 = result->add_updates();
    u2->set_key("wk2");
    u2->set_value("wv2");

    result->add_read_set("rk1");
    result->add_read_set("rk2");
    result->add_read_set("rk3");

    EXPECT_CALL(*daml_validator_client,
                ValidatePendingSubmission(_, _, _, _, _))
        .Times(1)
        .WillOnce(
            DoAll(SetArgPointee<4>(*mock_response2), Return(grpc::Status())));
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

ConcordRequest build_pre_executed_request(
    BlockId pre_execution_block_id, const std::vector<std::string>& reads,
    const std::map<std::string, std::string>& writes) {
  ConcordRequest concord_request;
  PreExecutionResult pre_execution_result;

  pre_execution_result.set_request_correlation_id("correlation_id");
  pre_execution_result.set_read_set_version(pre_execution_block_id);

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

  concord_request.mutable_pre_execution_result()->MergeFrom(
      pre_execution_result);

  return concord_request;
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
