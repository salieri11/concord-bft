// Copyright 2020 VMware, all rights reserved

#include "daml/daml_validator_client.hpp"
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <opentracing/tracer.h>
#include "Logger.hpp"
#include "daml_validator_mock.grpc.pb.h"

using ::std::placeholders::_1;
using ::testing::_;
using ::testing::AtLeast;
using ::testing::DoAll;
using ::testing::Eq;
using ::testing::InSequence;
using ::testing::Return;
using ::testing::SaveArg;
using ::testing::WithArg;

using com::digitalasset::kvbc::EventFromValidator;
using com::digitalasset::kvbc::EventToValidator;
using com::digitalasset::kvbc::MockValidationServiceStub;
using com::digitalasset::kvbc::PreprocessorFromEngine;
using com::digitalasset::kvbc::PreprocessorToEngine;
using com::digitalasset::kvbc::ValidationService;
using com::vmware::concord::PreExecutionResult;
using com::vmware::concord::kvb::ValueWithTrids;
using concord::daml::DamlValidatorClient;
using concord::daml::KeyValuePairWithThinReplicaIds;
using concord::daml::ValueFingerprintPair;

namespace {

ACTION_P(copy, msg) { arg0->CopyFrom(*msg); }

template <class W, class R>
class MockClientReaderWriter
    : public ::grpc::ClientReaderWriterInterface<W, R> {
 public:
  MockClientReaderWriter() = default;

  /// ClientStreamingInterface
  MOCK_METHOD0_T(Finish, ::grpc::Status());

  /// ReaderInterface
  MOCK_METHOD1_T(NextMessageSize, bool(uint32_t*));
  MOCK_METHOD1_T(Read, bool(R*));

  /// WriterInterface
  MOCK_METHOD2_T(Write, bool(const W&, const ::grpc::WriteOptions));

  /// ClientReaderWriterInterface
  MOCK_METHOD0_T(WaitForInitialMetadata, void());
  MOCK_METHOD0_T(WritesDone, bool());
};

class MockReadingFromStorage {
 public:
  MockReadingFromStorage() = default;

  MOCK_METHOD1(Read,
               std::map<std::string, std::string>(
                   const google::protobuf::RepeatedPtrField<std::string>&));

  MOCK_METHOD1(ReadWithFingerprints,
               std::map<std::string, ValueFingerprintPair>(
                   const google::protobuf::RepeatedPtrField<std::string>&));
};

auto test_span =
    opentracing::Tracer::Global() -> StartSpan("daml_validator_client_test");
google::protobuf::Timestamp test_record_time;
uint16_t test_replica_id = 1234;

class DamlValidatorClientTest : public ::testing::Test {
 protected:
  void SetUp() override {
    mock_reader_.reset(new MockReadingFromStorage());
    mock_stream_ = new MockClientReaderWriter<PreprocessorToEngine,
                                              PreprocessorFromEngine>();
  }

  std::unique_ptr<DamlValidatorClient> CreateClient() {
    auto mock_validation_service_stub = new MockValidationServiceStub();
    EXPECT_CALL(*mock_validation_service_stub, PreexecuteRaw(_))
        .Times(AtLeast(1))
        .WillOnce(Return(mock_stream_));
    return std::make_unique<DamlValidatorClient>(test_replica_id,
                                                 mock_validation_service_stub);
  }

  std::unique_ptr<PreExecutionResult> CreateExpectedPreExecutionResult() {
    std::unique_ptr<PreExecutionResult> expected_preexecution_result(
        new PreExecutionResult());
    auto read_key = expected_preexecution_result->mutable_read_set()
                        ->add_keys_with_fingerprints();
    read_key->set_key("1");
    read_key->set_fingerprint("2");
    expected_preexecution_result->set_output("some result");
    expected_preexecution_result->set_request_correlation_id("some ID");
    return expected_preexecution_result;
  }

  void CallValidate(DamlValidatorClient* client,
                    std::vector<std::string>* read_set,
                    std::vector<KeyValuePairWithThinReplicaIds>* write_set) {
    client->Validate("ignored submission", test_record_time, "participant ID",
                     "correlation ID", *test_span,
                     std::bind(&MockReadingFromStorage::Read, mock_reader_, _1),
                     read_set, write_set);
  }

  MockClientReaderWriter<PreprocessorToEngine, PreprocessorFromEngine>*
      mock_stream_;
  std::shared_ptr<MockReadingFromStorage> mock_reader_;
};

TEST_F(DamlValidatorClientTest, ValidateSendsSubmissionFirst) {
  auto mock_stream =
      new MockClientReaderWriter<EventToValidator, EventFromValidator>();
  EventToValidator first_message;
  // => submission
  EXPECT_CALL(*mock_stream, Write(_, _))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&first_message), Return(true)));
  // <== finished
  EventFromValidator done;
  done.mutable_done();
  EXPECT_CALL(*mock_stream, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&done)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream, Finish()).WillOnce(Return(grpc::Status()));

  MockValidationServiceStub* validation_service_stub =
      new MockValidationServiceStub();
  EXPECT_CALL(*validation_service_stub, ValidateRaw(_))
      .Times(AtLeast(1))
      .WillOnce(Return(mock_stream));
  DamlValidatorClient client(test_replica_id, validation_service_stub);
  std::vector<std::string> ignored_read_set;
  std::vector<KeyValuePairWithThinReplicaIds> ignored_write_set;
  std::string expected_submission = "a submission";
  std::string expected_participant_id = "participant ID";
  std::string expected_correlation_id = "correlation ID";
  int expected_replica_id = test_replica_id;
  client.Validate(expected_submission, test_record_time,
                  expected_participant_id, expected_correlation_id, *test_span,
                  std::bind(&MockReadingFromStorage::Read, mock_reader_, _1),
                  &ignored_read_set, &ignored_write_set);

  EXPECT_TRUE(first_message.has_validate_request());
  const ::com::digitalasset::kvbc::ValidateRequest& actual_validate_request =
      first_message.validate_request();
  EXPECT_EQ(expected_submission, actual_validate_request.submission());
  EXPECT_EQ(expected_replica_id, actual_validate_request.replica_id());
  EXPECT_EQ(expected_participant_id, actual_validate_request.participant_id());
  EXPECT_EQ(expected_correlation_id, actual_validate_request.correlation_id());
  EXPECT_EQ(test_record_time.seconds(),
            actual_validate_request.record_time().seconds());
  EXPECT_EQ(test_record_time.nanos(),
            actual_validate_request.record_time().nanos());
}

TEST_F(DamlValidatorClientTest, DeathOnNonGrpcOK) {
  auto mock_stream =
      new MockClientReaderWriter<EventToValidator, EventFromValidator>();
  EventToValidator first_message;
  // => submission
  EXPECT_CALL(*mock_stream, Write(_, _))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&first_message), Return(true)));
  // <== finished
  EventFromValidator done;
  done.mutable_done();
  EXPECT_CALL(*mock_stream, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&done)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream, Finish()).WillOnce(Return(grpc::Status::CANCELLED));

  MockValidationServiceStub* validation_service_stub =
      new MockValidationServiceStub();
  DamlValidatorClient client(test_replica_id, validation_service_stub);
  std::vector<std::string> ignored_read_set;
  std::vector<KeyValuePairWithThinReplicaIds> ignored_write_set;
  std::string expected_submission = "a submission";
  std::string expected_participant_id = "participant ID";
  std::string expected_correlation_id = "correlation ID";
  int expected_replica_id = test_replica_id;
  testing::Mock::AllowLeak(mock_stream);
  testing::Mock::AllowLeak(validation_service_stub);
  ASSERT_DEATH(client.Validate(
                   expected_submission, test_record_time,
                   expected_participant_id, expected_correlation_id, *test_span,
                   std::bind(&MockReadingFromStorage::Read, mock_reader_, _1),
                   &ignored_read_set, &ignored_write_set),
               ".*");
}

TEST_F(DamlValidatorClientTest, ValidateCopiesReadAndWriteSetFromDone) {
  auto mock_stream =
      new MockClientReaderWriter<EventToValidator, EventFromValidator>();
  EventToValidator first_message;
  // => submission
  EXPECT_CALL(*mock_stream, Write(_, _)).Times(1).WillOnce(Return(true));
  // <== finished
  EventFromValidator done;
  std::vector<std::string> expected_read_set = {"1", "2", "3"};
  for (auto key : expected_read_set) {
    done.mutable_done()->add_read_set(key);
  }
  ValueWithTrids value_with_thin_replica_ids;
  value_with_thin_replica_ids.set_value("a value");
  value_with_thin_replica_ids.add_trid("a");
  value_with_thin_replica_ids.add_trid("b");
  value_with_thin_replica_ids.add_trid("c");
  std::vector<KeyValuePairWithThinReplicaIds> expected_write_set = {
      {"1234", value_with_thin_replica_ids}};
  for (auto expected_key_value_pair : expected_write_set) {
    ::com::digitalasset::kvbc::ProtectedKeyValuePair* added_key_value_pair =
        done.mutable_done()->add_write_set();
    added_key_value_pair->set_key(expected_key_value_pair.first);
    added_key_value_pair->set_value(expected_key_value_pair.second.value());
    for (auto thin_replica_id : expected_key_value_pair.second.trid()) {
      added_key_value_pair->add_trids(thin_replica_id);
    }
  }
  EXPECT_CALL(*mock_stream, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&done)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream, Finish()).WillOnce(Return(grpc::Status()));

  MockValidationServiceStub* validation_service_stub =
      new MockValidationServiceStub();
  EXPECT_CALL(*validation_service_stub, ValidateRaw(_))
      .Times(AtLeast(1))
      .WillOnce(Return(mock_stream));
  DamlValidatorClient client(test_replica_id, validation_service_stub);
  std::vector<std::string> actual_read_set;
  std::vector<KeyValuePairWithThinReplicaIds> actual_write_set;
  CallValidate(&client, &actual_read_set, &actual_write_set);

  EXPECT_EQ(expected_read_set, actual_read_set);
  ASSERT_EQ(1, actual_write_set.size());
  EXPECT_EQ(expected_write_set[0].first, actual_write_set[0].first);
  EXPECT_EQ(expected_write_set[0].second.value(),
            actual_write_set[0].second.value());
  auto expected_thin_replica_ids = expected_write_set[0].second.trid();
  auto actual_thin_replica_ids = actual_write_set[0].second.trid();
  ASSERT_EQ(expected_thin_replica_ids.size(), actual_thin_replica_ids.size());
  for (int i = 0; i < expected_thin_replica_ids.size(); ++i) {
    EXPECT_EQ(expected_thin_replica_ids.Get(i), actual_thin_replica_ids.Get(i));
  }
}

TEST_F(DamlValidatorClientTest, ValidateRespondsToReadEventWithMultipleKeys) {
  auto mock_stream =
      new MockClientReaderWriter<EventToValidator, EventFromValidator>();
  MockValidationServiceStub* validation_service_stub =
      new MockValidationServiceStub();
  EXPECT_CALL(*validation_service_stub, ValidateRaw(_))
      .Times(AtLeast(1))
      .WillOnce(Return(mock_stream));

  InSequence expected_validation_sequence;
  // => submission
  EXPECT_CALL(*mock_stream, Write(_, _)).Times(1).WillOnce(Return(true));
  // <= read request
  EventFromValidator request_read;
  std::string expected_tag = "requested tag";
  request_read.mutable_read()->set_tag(expected_tag);
  std::vector<std::string> expected_keys = {"1", "2"};
  for (auto expected_key : expected_keys) {
    request_read.mutable_read()->add_keys(expected_key);
  }
  EXPECT_CALL(*mock_stream, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&request_read)), Return(true)));
  // => read response
  google::protobuf::RepeatedPtrField<std::string> actual_requested_keys;
  std::string expected_value = "some value";
  std::map<std::string, std::string> read_result;
  for (auto expected_key : expected_keys) {
    read_result[expected_key] = expected_value;
  }
  EXPECT_CALL(*mock_reader_, Read(_))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&actual_requested_keys), Return(read_result)));
  EventToValidator read_completed;
  EXPECT_CALL(*mock_stream, Write(_, _))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&read_completed), Return(true)));
  // <= finished
  EventFromValidator done;
  done.mutable_done();
  EXPECT_CALL(*mock_stream, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&done)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream, Finish()).WillOnce(Return(grpc::Status()));

  DamlValidatorClient client(test_replica_id, validation_service_stub);
  std::vector<std::string> ignored_read_set;
  std::vector<KeyValuePairWithThinReplicaIds> ignored_write_set;
  CallValidate(&client, &ignored_read_set, &ignored_write_set);

  EXPECT_EQ(expected_keys.size(), actual_requested_keys.size());
  for (int i = 0; i < actual_requested_keys.size(); ++i) {
    EXPECT_EQ(expected_keys[i], actual_requested_keys.Get(i));
  }
  EXPECT_TRUE(read_completed.has_read_result());
  EXPECT_EQ(expected_tag, read_completed.read_result().tag());
  EXPECT_EQ(expected_keys.size(),
            read_completed.read_result().key_value_pairs_size());
  for (int i = 0; i < expected_keys.size(); ++i) {
    EXPECT_EQ(expected_keys[i],
              read_completed.read_result().key_value_pairs(i).key());
    EXPECT_EQ(expected_value,
              read_completed.read_result().key_value_pairs(i).value());
  }
}

TEST_F(DamlValidatorClientTest, PreExecuteSendsSubmissionFirst) {
  PreprocessorToEngine first_message;
  // => submission
  EXPECT_CALL(*mock_stream_, Write(_, _))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&first_message), Return(true)));
  // <== finished
  PreprocessorFromEngine finished;
  finished.mutable_preexecution_result();
  EXPECT_CALL(*mock_stream_, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&finished)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream_, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream_, Finish()).WillOnce(Return(grpc::Status()));

  std::string expected_submission = "a submission";
  std::string expected_participant_id = "participant ID";
  std::string expected_correlation_id = "correlation ID";
  int expected_replica_id = test_replica_id;
  auto client = CreateClient();
  com::vmware::concord::PreExecutionResult ignored_result;

  client->PreExecute(expected_submission, expected_participant_id,
                     expected_correlation_id, *test_span,
                     std::bind(&MockReadingFromStorage::ReadWithFingerprints,
                               mock_reader_, _1),
                     &ignored_result);

  EXPECT_TRUE(first_message.has_preexecution_request());
  const ::com::digitalasset::kvbc::PreprocessorToEngine_PreExecutionRequest&
      actual_request = first_message.preexecution_request();
  EXPECT_EQ(expected_submission, actual_request.submission());
  EXPECT_EQ(expected_participant_id,
            actual_request.submitting_participant_id());
  EXPECT_EQ(expected_replica_id, actual_request.replica_id());
  EXPECT_EQ(expected_correlation_id, actual_request.correlation_id());
}

TEST_F(DamlValidatorClientTest,
       PreExecuteRespondsToReadRequestWithMultipleKeys) {
  auto client = CreateClient();
  InSequence expected_events_sequence;
  // => submission
  EXPECT_CALL(*mock_stream_, Write(_, _)).Times(1).WillOnce(Return(true));
  // <= read request
  PreprocessorFromEngine read_request;
  std::string expected_tag = "requested tag";
  read_request.mutable_read_request()->set_tag(expected_tag);
  std::vector<std::string> expected_keys = {"1", "2"};
  for (auto expected_key : expected_keys) {
    read_request.mutable_read_request()->add_keys(expected_key);
  }
  EXPECT_CALL(*mock_stream_, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&read_request)), Return(true)));
  // => read response
  google::protobuf::RepeatedPtrField<std::string> actual_requested_keys;
  std::string expected_value = "some value";
  std::map<std::string, ValueFingerprintPair> read_result;
  for (auto expected_key : expected_keys) {
    read_result[expected_key] = std::make_pair(expected_value, expected_key);
  }
  EXPECT_CALL(*mock_reader_, ReadWithFingerprints(_))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&actual_requested_keys), Return(read_result)));
  PreprocessorToEngine read_completed;
  EXPECT_CALL(*mock_stream_, Write(_, _))
      .Times(1)
      .WillOnce(DoAll(SaveArg<0>(&read_completed), Return(true)));
  // <== finished
  PreprocessorFromEngine finished;
  finished.mutable_preexecution_result();
  EXPECT_CALL(*mock_stream_, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&finished)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream_, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream_, Finish()).WillOnce(Return(grpc::Status()));
  com::vmware::concord::PreExecutionResult ignored_result;

  client->PreExecute("ignored", "ignored", "ignored", *test_span,
                     std::bind(&MockReadingFromStorage::ReadWithFingerprints,
                               mock_reader_, _1),
                     &ignored_result);

  EXPECT_EQ(expected_keys.size(), actual_requested_keys.size());
  for (int i = 0; i < actual_requested_keys.size(); ++i) {
    EXPECT_EQ(expected_keys[i], actual_requested_keys.Get(i));
  }
  EXPECT_TRUE(read_completed.has_read_result());
  auto actual_result = read_completed.read_result();
  EXPECT_EQ(expected_tag, actual_result.tag());
  EXPECT_EQ(expected_keys.size(), actual_result.key_value_pairs_size());
  for (int i = 0; i < expected_keys.size(); ++i) {
    auto actual_key_value_pair = actual_result.key_value_pairs(i);
    EXPECT_EQ(expected_keys[i], actual_key_value_pair.key());
    EXPECT_EQ(expected_value, actual_key_value_pair.value());
    EXPECT_EQ(expected_keys[i], actual_key_value_pair.fingerprint());
  }
}

TEST_F(DamlValidatorClientTest, PreExecuteCopiesResultAsIs) {
  // => submission
  EXPECT_CALL(*mock_stream_, Write(_, _)).Times(1).WillOnce(Return(true));
  // <== finished
  PreprocessorFromEngine finished;
  auto expected_preexecution_result = CreateExpectedPreExecutionResult();
  finished.mutable_preexecution_result()->CopyFrom(
      *expected_preexecution_result);
  EXPECT_CALL(*mock_stream_, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&finished)), Return(true)));
  // End of stream.
  EXPECT_CALL(*mock_stream_, WritesDone()).WillOnce(Return(true));
  EXPECT_CALL(*mock_stream_, Finish()).WillOnce(Return(grpc::Status()));
  auto client = CreateClient();
  com::vmware::concord::PreExecutionResult actual_result;

  client->PreExecute("ignored", "ignored", "ignored", *test_span,
                     std::bind(&MockReadingFromStorage::ReadWithFingerprints,
                               mock_reader_, _1),
                     &actual_result);

  EXPECT_EQ(expected_preexecution_result->SerializeAsString(),
            actual_result.SerializeAsString());
}

}  // anonymous namespace
