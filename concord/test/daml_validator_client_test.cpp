// Copyright 2020 VMware, all rights reserved

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <opentracing/tracer.h>

#include "daml/daml_validator_client.hpp"
#include "daml_validator_mock.grpc.pb.h"

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
using com::digitalasset::kvbc::ValidationService;
using concord::daml::DamlValidatorClient;
using concord::daml::KeyValueStorageOperations;

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

class MockKeyValueStorageOperations : public KeyValueStorageOperations {
 public:
  MockKeyValueStorageOperations() = default;

  MOCK_METHOD1(Read,
               std::map<std::string, std::string>(
                   const google::protobuf::RepeatedPtrField<std::string>&));
  MOCK_METHOD3(Write, void(const std::string&, const std::string&,
                           const std::vector<std::string>&));
};

auto test_span =
    opentracing::Tracer::Global() -> StartSpan("daml_validator_client_test");
google::protobuf::Timestamp test_record_time;
uint16_t test_replica_id = 1234;

void call_validate(DamlValidatorClient* client,
                   KeyValueStorageOperations* storage_ops,
                   std::vector<std::string>* read_set) {
  client->Validate("ignored submission", test_record_time, "participant ID",
                   "correlation ID", *test_span, *read_set, *storage_ops);
}

TEST(DamlValidatorTest, validate_sends_the_submission_first) {
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
  auto ops = new MockKeyValueStorageOperations();
  std::string expected_submission = "a submission";
  std::string expected_participant_id = "participant ID";
  std::string expected_correlation_id = "correlation ID";
  int expected_replica_id = test_replica_id;
  client.Validate(expected_submission, test_record_time,
                  expected_participant_id, expected_correlation_id, *test_span,
                  ignored_read_set, *ops);

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

TEST(DamlValidatorTest, validate_copies_read_set_from_done) {
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
  MockKeyValueStorageOperations ops;
  call_validate(&client, &ops, &actual_read_set);

  EXPECT_EQ(expected_read_set, actual_read_set);
}

TEST(DamlValidatorTest, validate_responds_to_read_event_with_multiple_keys) {
  auto mock_stream =
      new MockClientReaderWriter<EventToValidator, EventFromValidator>();
  MockValidationServiceStub* validation_service_stub =
      new MockValidationServiceStub();
  EXPECT_CALL(*validation_service_stub, ValidateRaw(_))
      .Times(AtLeast(1))
      .WillOnce(Return(mock_stream));
  MockKeyValueStorageOperations mock_key_value_storage_operations;

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
  EXPECT_CALL(mock_key_value_storage_operations, Read(_))
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
  call_validate(&client, &mock_key_value_storage_operations, &ignored_read_set);

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

TEST(DamlValidatorTest, validate_responds_to_write_event) {
  auto mock_stream =
      new MockClientReaderWriter<EventToValidator, EventFromValidator>();
  MockValidationServiceStub* validation_service_stub =
      new MockValidationServiceStub();
  EXPECT_CALL(*validation_service_stub, ValidateRaw(_))
      .Times(AtLeast(1))
      .WillOnce(Return(mock_stream));
  MockKeyValueStorageOperations mock_key_value_storage_operations;

  InSequence expected_validation_sequence;
  // => submission
  EXPECT_CALL(*mock_stream, Write(_, _)).Times(1).WillOnce(Return(true));
  // <= write request
  EventFromValidator request_write;
  ::com::digitalasset::kvbc::ProtectedKeyValuePair* update =
      request_write.mutable_write()->add_updates();
  std::string expected_key = "1234";
  std::string expected_value = "a value";
  std::vector<std::string> expected_thin_replica_ids = {"a", "b", "c"};
  update->set_key(expected_key);
  update->set_value(expected_value);
  for (auto thin_replica_id : expected_thin_replica_ids) {
    update->add_trids(thin_replica_id);
  }
  EXPECT_CALL(*mock_stream, Read(_))
      .WillOnce(DoAll(WithArg<0>(copy(&request_write)), Return(true)));
  // => no response
  EXPECT_CALL(mock_key_value_storage_operations,
              Write(expected_key, expected_value, expected_thin_replica_ids))
      .Times(1);
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
  call_validate(&client, &mock_key_value_storage_operations, &ignored_read_set);
}

}  // anonymous namespace
