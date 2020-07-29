// Copyright 2020 VMware, all rights reserved

#include <concord.pb.h>
#include <daml_commit.pb.h>
#include <google/protobuf/util/message_differencer.h>
#include <google/protobuf/util/time_util.h>
#include <daml/daml_kvb_commands_handler.hpp>
#include <daml/daml_validator_client.hpp>
#include <memory>
#include "Logger.hpp"
#include "OpenTracing.hpp"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "mocks.hpp"
#include "time/time_contract.hpp"

using namespace std::placeholders;  // For _1, _2, etc.

using ::testing::_;
using ::testing::AtLeast;
using ::testing::Cardinality;
using ::testing::Contains;
using ::testing::DoAll;
using ::testing::Eq;
using ::testing::Exactly;
using ::testing::Return;
using ::testing::ReturnRef;
using ::testing::SetArgPointee;
using ::testing::SetArgReferee;
using ::testing::WithArg;

using namespace concord::kvbc;
using namespace concord::daml;
using namespace concordUtils;

namespace da_kvbc = com::digitalasset::kvbc;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::PreExecutionResult;
using com::vmware::concord::WriteSet;
using com::vmware::concord::kvb::ValueWithTrids;
using concord::config::ConcordConfiguration;
using concord::config::ConfigurationPath;
using concord::consensus::ConcordCommandsHandler;
using concord::daml::CreateDamlKvbKey;
using concord::daml::CreateDamlKvbValue;
using concord::daml::CreateSliver;
using concord::time::TimeContract;

using google::protobuf::RepeatedPtrField;
using google::protobuf::Timestamp;
using google::protobuf::util::MessageDifferencer;
using google::protobuf::util::TimeUtil;

namespace {

ACTION_P(CopyKeyValuePairs, write_set) {
  write_set->insert(arg0.begin(), arg0.end());
}

constexpr size_t OUT_BUFFER_SIZE = 512000;
const int64_t kCurrentTimeMillis = 12345678;
google::protobuf::Timestamp kCurrentTime =
    TimeUtil::MillisecondsToTimestamp(kCurrentTimeMillis);

const auto NEVER = Exactly(0);

auto kTestSpan = opentracing::Tracer::Global()
                     -> StartSpan("daml_kvb_commands_handler_test");

class DamlKvbCommandsHandlerTest : public ::testing::Test {
 protected:
  typedef std::map<std::string, std::string> StringStringMap;

  void SetUp() override {
    configuration_ = TestConfiguration(4, 4, 0, 0, false, false);
    mock_ro_storage_ = std::make_unique<MockLocalKeyValueStorageReadOnly>();
    EXPECT_CALL(*mock_ro_storage_, getLastBlock())
        .WillRepeatedly(Return(kLastBlockId));
    mock_block_appender_ = std::make_unique<MockBlockAppender>();
    mock_time_contract_ = std::make_unique<MockTimeContract>(
        *mock_ro_storage_, configuration_, kCurrentTime);
    reply_size_ = 0;
    memset(reply_buffer_, 0, OUT_BUFFER_SIZE);
    time_update_key_ = CreateDamlKvbKey(DamlKvbCommandsHandler::kTimeUpdateKey);
  }

  void TearDown() override {
    instance_.reset();
    // Make sure expectations are verified on the mocks.
    mock_time_contract_.reset();
    mock_ro_storage_.reset();
    mock_block_appender_.reset();
  }

  DamlKvbCommandsHandler* CreateInstance() { return CreateInstance(false); }

  DamlKvbCommandsHandler* CreateInstance(bool expect_call_on_validator_client) {
    auto daml_validator_client = CreateMockDamlValidatorClientForPreExecution(
        expect_call_on_validator_client);
    return CreateInstance(daml_validator_client);
  }

  DamlKvbCommandsHandler* CreateInstance(
      std::unique_ptr<MockDamlValidatorClient>& daml_validator_client) {
    prometheus_registry_ = CreatePrometheusRegistry();
    instance_ = std::make_unique<DamlKvbCommandsHandler>(
        configuration_, GetNodeConfig(configuration_, 1), *mock_ro_storage_,
        *mock_block_appender_, subscriber_list_,
        std::move(daml_validator_client), prometheus_registry_);
    return instance_.get();
  }

  std::shared_ptr<PrometheusRegistry> CreatePrometheusRegistry() {
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

  std::unique_ptr<MockDamlValidatorClient>
  CreateMockDamlValidatorClientForPreExecution(
      bool expect_call_on_validator_client) {
    std::unique_ptr<MockDamlValidatorClient> daml_validator_client =
        std::make_unique<MockDamlValidatorClient>();

    if (!expect_call_on_validator_client) {
      EXPECT_CALL(*daml_validator_client, PreExecute(_, _, _, _, _, _))
          .Times(NEVER);
    } else {
      EXPECT_CALL(*daml_validator_client, PreExecute(_, _, _, _, _, _))
          .Times(1)
          .WillOnce(testing::Invoke(
              [this](const std::string& submission,
                     const std::string& participant_id,
                     const std::string& correlation_id,
                     const opentracing::Span& parent_span,
                     KeyValueWithFingerprintReaderFunc read_from_storage,
                     PreExecutionResult* pre_execution_result) -> grpc::Status {
                da_kvbc::PreExecutionOutput pre_execution_output;
                pre_execution_output.mutable_min_record_time()->CopyFrom(
                    this->mock_time_contract_->GetTime());
                StringStringMap expected_key_value_pairs = {
                    {"write-key1", "write-value1"},
                    {"write-key2", "write-value2"}};
                WriteSet expected_write_set;
                PopulateWriteSet(expected_key_value_pairs, &expected_write_set);
                pre_execution_output.mutable_success_write_set()->CopyFrom(
                    expected_write_set);
                expected_read_set_.Clear();
                for (const auto& read_key : this->expected_read_keys_) {
                  auto* key_with_fingerprint =
                      expected_read_set_.add_keys_with_fingerprints();
                  key_with_fingerprint->set_key(read_key);
                  key_with_fingerprint->set_fingerprint(read_key);
                }
                pre_execution_result->mutable_read_set()->CopyFrom(
                    expected_read_set_);
                pre_execution_result->set_output(
                    pre_execution_output.SerializeAsString());
                pre_execution_result->set_request_correlation_id(
                    correlation_id);
                return grpc::Status();
              }));
    }

    return daml_validator_client;
  }

  std::unique_ptr<MockDamlValidatorClient> CreateMockDamlValidatorClient(
      bool expect_never) {
    std::unique_ptr<MockDamlValidatorClient> daml_validator_client =
        std::make_unique<MockDamlValidatorClient>();

    if (expect_never) {
      EXPECT_CALL(*daml_validator_client, Validate(_, _, _, _, _, _, _, _))
          .Times(NEVER);
    } else {
      EXPECT_CALL(*daml_validator_client, Validate(_, _, _, _, _, _, _, _))
          .Times(1)
          .WillOnce(testing::Invoke(
              [](const std::string& submission,
                 const google::protobuf::Timestamp& record_time,
                 const std::string& participant_id,
                 const std::string& correlation_id,
                 const opentracing::Span& parent_span,
                 DamlKvbReadFunc read_from_storage,
                 std::vector<std::string>* read_set,
                 std::vector<KeyValuePairWithThinReplicaIds>* write_set)
                  -> grpc::Status {
                read_set->push_back("rk1");
                read_set->push_back("rk2");
                read_set->push_back("rk3");
                std::vector<std::string> replicas;
                replicas.push_back("replica1");
                write_set->push_back(CreateKeyValuePairWithThinReplicaIds(
                    "wk1", "wv1", replicas));
                write_set->push_back(CreateKeyValuePairWithThinReplicaIds(
                    "wk2", "wv2", replicas));
                return grpc::Status();
              }));
    }

    return daml_validator_client;
  }

  std::string BuildCommitRequest() {
    auto concord_request = BuildConcordRequest();
    std::string request_string;
    concord_request.SerializeToString(&request_string);
    return request_string;
  }

  ConcordRequest BuildConcordRequest() {
    da_kvbc::CommitRequest commit_request;
    commit_request.set_participant_id("participant_id");
    commit_request.set_correlation_id(kCorrelationId);
    da_kvbc::Command daml_commit_command;
    daml_commit_command.mutable_commit()->MergeFrom(commit_request);
    std::string cmd_string;
    daml_commit_command.SerializeToString(&cmd_string);

    DamlRequest daml_request;
    daml_request.set_command(cmd_string);

    ConcordRequest concord_request;
    concord_request.mutable_daml_request()->MergeFrom(daml_request);
    return concord_request;
  }

  ConcordResponse BuildPreExecutionResponse(BlockId read_set_block_height) {
    ConcordResponse concord_response;
    PreExecutionResult pre_execution_result;

    pre_execution_result.set_request_correlation_id(kCorrelationId);

    const std::string read_key = "read_key";

    auto* read_set = pre_execution_result.mutable_read_set();
    auto* key_with_fingerprint = read_set->add_keys_with_fingerprints();
    key_with_fingerprint->set_key(read_key);
    key_with_fingerprint->set_fingerprint(
        ConcordCommandsHandler::SerializeFingerprint(read_set_block_height));

    concord_response.mutable_pre_execution_result()->MergeFrom(
        pre_execution_result);

    return concord_response;
  }

  Sliver ExpectedTimeUpdateValue(const std::list<string>& thin_replica_ids) {
    ValueWithTrids expected_time_update_value;
    for (const auto& id : thin_replica_ids) {
      expected_time_update_value.add_trid(id);
    }
    da_kvbc::TimeUpdateLogEntry time_update_log_entry;
    time_update_log_entry.mutable_record_time()->CopyFrom(
        mock_time_contract_->GetTime());
    expected_time_update_value.set_value(
        time_update_log_entry.SerializeAsString());
    return CreateSliver(expected_time_update_value.SerializeAsString());
  }

  static KeyValuePairWithThinReplicaIds CreateKeyValuePairWithThinReplicaIds(
      const std::string& key, const std::string& value,
      const std::vector<std::string>& replicas) {
    ValueWithTrids value_with_thin_replica_ids;
    value_with_thin_replica_ids.set_value(value);
    for (auto replica_id : replicas) {
      value_with_thin_replica_ids.add_trid(replica_id);
    }
    return {key, value_with_thin_replica_ids};
  }

  static void PopulateWriteSet(const std::map<std::string, std::string>& writes,
                               WriteSet* output) {
    for (const auto& entry : writes) {
      auto added_pair = output->add_kv_writes();
      added_pair->set_key(entry.first);
      added_pair->set_value(entry.second);
    }
  }

  const BlockId kLastBlockId = 100;
  const std::string kCorrelationId = "correlation_id";

  std::vector<std::string> expected_read_keys_;
  com::vmware::concord::ReadSet expected_read_set_;
  Sliver time_update_key_;

  uint32_t reply_size_;
  char reply_buffer_[OUT_BUFFER_SIZE];

  uint32_t replica_specific_info_size_ = 0;
  concordUtils::SpanWrapper span_wrapper_;
  ConcordConfiguration configuration_;
  std::shared_ptr<PrometheusRegistry> prometheus_registry_;
  std::unique_ptr<DamlKvbCommandsHandler> instance_;
  std::unique_ptr<MockLocalKeyValueStorageReadOnly> mock_ro_storage_;
  std::unique_ptr<MockBlockAppender> mock_block_appender_;
  std::unique_ptr<TimeContract> mock_time_contract_;
  SubBufferList subscriber_list_;
};

TEST_F(DamlKvbCommandsHandlerTest, ExecuteCommitHappyPathCreatesBlock) {
  EXPECT_CALL(*mock_block_appender_, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(kLastBlockId + 1), Return(Status::OK())));
  auto daml_validator_client = CreateMockDamlValidatorClient(false);
  auto instance = CreateInstance(daml_validator_client);
  std::string request_string = BuildCommitRequest();

  ASSERT_EQ(0, instance->execute(1, 1, bftEngine::MsgFlag::EMPTY_FLAGS,
                                 request_string.size(), request_string.c_str(),
                                 OUT_BUFFER_SIZE, reply_buffer_, reply_size_,
                                 replica_specific_info_size_, span_wrapper_));

  ConcordResponse concord_response;
  concord_response.ParseFromArray(reply_buffer_, reply_size_);
  ASSERT_TRUE(concord_response.has_daml_response());
}

TEST_F(DamlKvbCommandsHandlerTest,
       PostExecuteFailsWithInvalidPreExecutionOutput) {
  PreExecutionResult pre_execution_result;
  pre_execution_result.set_output("invalid");
  auto instance = CreateInstance();

  ConcordResponse actual_response;
  ASSERT_FALSE(instance->PostExecute(pre_execution_result,
                                     mock_time_contract_.get(), *kTestSpan,
                                     actual_response));
  EXPECT_FALSE(actual_response.has_daml_response());
}

TEST_F(DamlKvbCommandsHandlerTest, PostExecutionConflictNoBlock) {
  auto daml_validator_client = std::make_unique<MockDamlValidatorClient>();
  auto instance = CreateInstance(daml_validator_client);

  const BlockId outdated_block_height = 90;
  const BlockId current_block_height = outdated_block_height + 5;

  const ConcordResponse concord_response =
      BuildPreExecutionResponse(outdated_block_height);

  const Sliver expected_key{std::string{"read_key"}};
  EXPECT_CALL(*mock_ro_storage_, get(_, Eq(expected_key), _, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<3>(current_block_height), Return(Status::OK())));

  EXPECT_CALL(*mock_block_appender_, addBlock(_, _)).Times(NEVER);

  std::string req_string;
  concord_response.SerializeToString(&req_string);

  ASSERT_EQ(1,
            instance->execute(1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG,
                              req_string.size(), req_string.c_str(),
                              OUT_BUFFER_SIZE, reply_buffer_, reply_size_,
                              replica_specific_info_size_, span_wrapper_));

  ConcordResponse actual_response;
  ASSERT_TRUE(reply_size_ > 0);
  EXPECT_FALSE(actual_response.has_daml_response());
  EXPECT_FALSE(actual_response.has_pre_execution_result());
}

TEST_F(DamlKvbCommandsHandlerTest, PreExecutionReadSetUnknownKey) {
  auto daml_validator_client = std::make_unique<MockDamlValidatorClient>();
  auto instance = CreateInstance(daml_validator_client);

  const ConcordResponse concord_response =
      BuildPreExecutionResponse(kLastBlockId);

  const Sliver expected_key{std::string{"read_key"}};
  EXPECT_CALL(*mock_ro_storage_, get(_, Eq(expected_key), _, _))
      .Times(1)
      .WillOnce(Return(Status::NotFound("read_key")));

  EXPECT_CALL(*mock_block_appender_, addBlock(_, _)).Times(NEVER);

  std::string req_string;
  concord_response.SerializeToString(&req_string);

  EXPECT_ANY_THROW(instance->execute(
      1, 1, bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG, req_string.size(),
      req_string.c_str(), OUT_BUFFER_SIZE, reply_buffer_, reply_size_,
      replica_specific_info_size_, span_wrapper_));
}

TEST_F(DamlKvbCommandsHandlerTest, PostExecuteCreatesNewBlockSuccessfulCase) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  pre_execution_output.mutable_min_record_time()->CopyFrom(
      mock_time_contract_->GetTime());
  pre_execution_output.mutable_max_record_time()->CopyFrom(
      mock_time_contract_->GetTime());
  StringStringMap expected_key_value_pairs = {{"write-key1", "write-value1"},
                                              {"write-key2", "write-value2"}};
  WriteSet expected_write_set;
  PopulateWriteSet(expected_key_value_pairs, &expected_write_set);
  pre_execution_output.mutable_success_write_set()->CopyFrom(
      expected_write_set);
  std::list<std::string> expected_thin_replica_ids = {"id1", "id2"};
  for (const auto& id : expected_thin_replica_ids) {
    *pre_execution_output.mutable_informee_success_thin_replica_ids()->Add() =
        id;
  }
  PreExecutionResult pre_execution_result;
  pre_execution_result.set_output(pre_execution_output.SerializeAsString());

  SetOfKeyValuePairs actual_raw_write_set;
  EXPECT_CALL(*mock_block_appender_, addBlock(_, _))
      .Times(1)
      .WillOnce(DoAll(WithArg<0>(CopyKeyValuePairs(&actual_raw_write_set)),
                      SetArgReferee<1>(kLastBlockId + 1),
                      Return(Status::OK())));
  auto instance = CreateInstance();
  ConcordResponse actual_response;

  ASSERT_TRUE(instance->PostExecute(pre_execution_result,
                                    mock_time_contract_.get(), *kTestSpan,
                                    actual_response));

  // 2 key-value pairs added by ConcordCommandsHandler + the time update.
  EXPECT_EQ(expected_key_value_pairs.size() + 2 + 1,
            actual_raw_write_set.size());
  // Check contents of write-set.
  for (const auto& entry : expected_key_value_pairs) {
    auto expected_key = CreateDamlKvbKey(entry.first);
    auto expected_value = CreateSliver(entry.second);
    ASSERT_THAT(actual_raw_write_set, Contains(::testing::Key(expected_key)));
    EXPECT_EQ(expected_value, actual_raw_write_set[expected_key]);
  }
  ASSERT_THAT(actual_raw_write_set, Contains(::testing::Key(time_update_key_)));
  auto expected_time_update_value =
      ExpectedTimeUpdateValue(expected_thin_replica_ids);
  EXPECT_EQ(expected_time_update_value, actual_raw_write_set[time_update_key_]);
  EXPECT_TRUE(actual_response.has_daml_response());
}

TEST_F(DamlKvbCommandsHandlerTest,
       PostExecuteCreatesNewBlockOutOfTimeBoundsCase) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  pre_execution_output.mutable_min_record_time()->CopyFrom(
      TimeUtil::MillisecondsToTimestamp(kCurrentTimeMillis + 1));
  StringStringMap expected_key_value_pairs = {{"reject-key1", "reject-value1"},
                                              {"reject-key2", "reject-value2"}};
  WriteSet expected_write_set;
  PopulateWriteSet(expected_key_value_pairs, &expected_write_set);
  pre_execution_output.mutable_out_of_time_bounds_write_set()->CopyFrom(
      expected_write_set);
  const std::string expected_participant_id("an ID");
  pre_execution_output.set_submitting_participant_id(expected_participant_id);
  PreExecutionResult pre_execution_result;
  pre_execution_result.set_output(pre_execution_output.SerializeAsString());

  SetOfKeyValuePairs actual_raw_write_set;
  EXPECT_CALL(*mock_block_appender_, addBlock(_, _))
      .Times(1)
      .WillOnce(DoAll(WithArg<0>(CopyKeyValuePairs(&actual_raw_write_set)),
                      SetArgReferee<1>(kLastBlockId + 1),
                      Return(Status::OK())));
  auto instance = CreateInstance();
  ConcordResponse actual_response;

  ASSERT_TRUE(instance->PostExecute(pre_execution_result,
                                    mock_time_contract_.get(), *kTestSpan,
                                    actual_response));

  // 2 key-value pairs added by ConcordCommandsHandler + the time update.
  EXPECT_EQ(expected_key_value_pairs.size() + 2 + 1,
            actual_raw_write_set.size());
  // Check contents of write-set.
  for (const auto& entry : expected_key_value_pairs) {
    auto expected_key = CreateDamlKvbKey(entry.first);
    auto expected_value = CreateSliver(entry.second);
    ASSERT_THAT(actual_raw_write_set, Contains(::testing::Key(expected_key)));
    EXPECT_EQ(expected_value, actual_raw_write_set[expected_key]);
  }
  ASSERT_THAT(actual_raw_write_set, Contains(::testing::Key(time_update_key_)));
  auto expected_time_update_value =
      ExpectedTimeUpdateValue({expected_participant_id});
  EXPECT_EQ(expected_time_update_value, actual_raw_write_set[time_update_key_]);
  EXPECT_TRUE(actual_response.has_daml_response());
}

TEST_F(DamlKvbCommandsHandlerTest, PostExecutePopulatesResponse) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  PreExecutionResult pre_execution_result;
  pre_execution_result.set_output(pre_execution_output.SerializeAsString());

  EXPECT_CALL(*mock_block_appender_, addBlock(_, _))
      .Times(1)
      .WillOnce(
          DoAll(SetArgReferee<1>(kLastBlockId + 1), Return(Status::OK())));
  auto instance = CreateInstance();
  ConcordResponse actual_response;

  ASSERT_TRUE(instance->PostExecute(pre_execution_result,
                                    mock_time_contract_.get(), *kTestSpan,
                                    actual_response));

  ASSERT_TRUE(actual_response.has_daml_response());
  da_kvbc::CommandReply actual_command_reply;
  actual_command_reply.ParseFromString(
      actual_response.daml_response().command_reply());
  ASSERT_TRUE(actual_command_reply.has_commit());
  EXPECT_EQ(da_kvbc::CommitResponse_CommitStatus_OK,
            actual_command_reply.commit().status());
  EXPECT_EQ(kLastBlockId + 1, actual_command_reply.commit().block_id());
}

TEST_F(DamlKvbCommandsHandlerTest, PreExecuteHappyPath) {
  std::string request_string = BuildCommitRequest();
  auto instance = CreateInstance(true);

  ASSERT_EQ(0, instance->execute(1, 1, bftEngine::MsgFlag::PRE_PROCESS_FLAG,
                                 request_string.size(), request_string.c_str(),
                                 OUT_BUFFER_SIZE, reply_buffer_, reply_size_,
                                 replica_specific_info_size_, span_wrapper_));

  ConcordResponse actual_response;
  actual_response.ParseFromArray(reply_buffer_, reply_size_);
  ASSERT_TRUE(actual_response.has_pre_execution_result());
  EXPECT_TRUE(MessageDifferencer::Equals(
      expected_read_set_, actual_response.pre_execution_result().read_set()));
  EXPECT_NE("", actual_response.pre_execution_result().output());
  EXPECT_EQ(kCorrelationId,
            actual_response.pre_execution_result().request_correlation_id());
  EXPECT_FALSE(actual_response.has_daml_response());
}

TEST_F(DamlKvbCommandsHandlerTest, PreExecuteExecutionEngineReturnsFailure) {
  std::string request_string = BuildCommitRequest();
  auto daml_validator_client = std::make_unique<MockDamlValidatorClient>();
  EXPECT_CALL(*daml_validator_client, PreExecute(_, _, _, _, _, _))
      .Times(1)
      .WillOnce(
          Return(grpc::Status(grpc::StatusCode::INTERNAL, "Internal error")));
  auto instance = CreateInstance(daml_validator_client);

  ASSERT_EQ(1, instance->execute(1, 1, bftEngine::MsgFlag::PRE_PROCESS_FLAG,
                                 request_string.size(), request_string.c_str(),
                                 OUT_BUFFER_SIZE, reply_buffer_, reply_size_,
                                 replica_specific_info_size_, span_wrapper_));

  ConcordResponse actual_response;
  actual_response.ParseFromArray(reply_buffer_, reply_size_);
  ASSERT_TRUE(actual_response.has_pre_execution_result());
  EXPECT_FALSE(actual_response.pre_execution_result().has_read_set());
  EXPECT_EQ(kCorrelationId,
            actual_response.pre_execution_result().request_correlation_id());
  EXPECT_FALSE(actual_response.has_daml_response());
}

TEST_F(DamlKvbCommandsHandlerTest, ReadKeysPopulatesFingerprint) {
  const std::string expected_key = "a key";
  const std::string expected_value = "a value";
  const BlockId expected_block_id = 1234;
  EXPECT_CALL(*mock_ro_storage_,
              get(_, Eq(CreateDamlKvbKey(expected_key)), _, _))
      .Times(1)
      .WillOnce(DoAll(SetArgReferee<2>(CreateDamlKvbValue(
                          expected_value, std::vector<string>{})),
                      SetArgReferee<3>(expected_block_id),
                      Return(Status::OK())));
  RepeatedPtrField<string> read_keys;
  *read_keys.Add() = expected_key;
  auto instance = CreateInstance();

  auto actual = instance->ReadKeys(read_keys);

  ASSERT_EQ(1, actual.size());
  ASSERT_THAT(actual, Contains(::testing::Key(expected_key)));
  EXPECT_EQ(expected_value, actual[expected_key].first);
  EXPECT_EQ(expected_block_id, ConcordCommandsHandler::DeserializeFingerprint(
                                   actual[expected_key].second));
}

TEST_F(DamlKvbCommandsHandlerTest, ReadKeysThrowsKeyNotAvailableInStorage) {
  const std::string expected_key = "a key";
  EXPECT_CALL(*mock_ro_storage_,
              get(_, Eq(CreateDamlKvbKey(expected_key)), _, _))
      .Times(1)
      .WillOnce(Return(Status::NotFound("n/a")));
  RepeatedPtrField<string> read_keys;
  *read_keys.Add() = expected_key;
  auto instance = CreateInstance();

  EXPECT_ANY_THROW({ instance->ReadKeys(read_keys); });
}

TEST(CheckIfWithinTimeBoundsTest, LessThanMin) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  pre_execution_output.mutable_min_record_time()->CopyFrom(
      TimeUtil::MillisecondsToTimestamp(kCurrentTimeMillis + 1));
  EXPECT_FALSE(concord::daml::CheckIfWithinTimeBounds(pre_execution_output,
                                                      kCurrentTime));
}

TEST(CheckIfWithinTimeBoundsTest, MoreThanMax) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  pre_execution_output.mutable_max_record_time()->CopyFrom(
      TimeUtil::MillisecondsToTimestamp(kCurrentTimeMillis - 1));
  EXPECT_FALSE(concord::daml::CheckIfWithinTimeBounds(pre_execution_output,
                                                      kCurrentTime));
}

TEST(CheckIfWithinTimeBoundsTest, OnBoundary) {
  {
    da_kvbc::PreExecutionOutput pre_execution_output;
    pre_execution_output.mutable_min_record_time()->CopyFrom(kCurrentTime);
    EXPECT_TRUE(concord::daml::CheckIfWithinTimeBounds(pre_execution_output,
                                                       kCurrentTime));
  }
  {
    da_kvbc::PreExecutionOutput pre_execution_output;
    pre_execution_output.mutable_max_record_time()->CopyFrom(kCurrentTime);
    EXPECT_TRUE(concord::daml::CheckIfWithinTimeBounds(pre_execution_output,
                                                       kCurrentTime));
  }
}

TEST(CheckIfWithinTimeBoundsTest, WithinBounds) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  pre_execution_output.mutable_min_record_time()->CopyFrom(
      TimeUtil::MillisecondsToTimestamp(kCurrentTimeMillis - 1));
  pre_execution_output.mutable_max_record_time()->CopyFrom(
      TimeUtil::MillisecondsToTimestamp(kCurrentTimeMillis + 1));
  EXPECT_TRUE(concord::daml::CheckIfWithinTimeBounds(pre_execution_output,
                                                     kCurrentTime));
}

TEST(CheckIfWithinTimeBoundsTest, NoBoundSpecified) {
  da_kvbc::PreExecutionOutput pre_execution_output;
  EXPECT_TRUE(concord::daml::CheckIfWithinTimeBounds(pre_execution_output,
                                                     kCurrentTime));
}

}  // anonymous namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
