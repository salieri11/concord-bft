// Copyright 2020 VMware, all rights reserved

#include <google/protobuf/util/time_util.h>
#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <consensus/commands_recorder.hpp>

namespace fs = boost::filesystem;
using google::protobuf::util::TimeUtil;
using namespace concord::consensus;
using namespace com::vmware::concord::performance;

namespace {
auto const cid = "test-cid";
auto const outputDir = "./testDir";

void VerifyKeyValueSetEqual(const std::map<std::string, std::string>& kvSet1,
                            const KVSet& kvSet2) {
  EXPECT_EQ(kvSet1.size(), kvSet2.pairs_size());
  for (auto const& kv : kvSet1) {
    EXPECT_NE(std::find_if(kvSet2.pairs().begin(), kvSet2.pairs().end(),
                           [&kv](const KeyValuePair& kvPair) {
                             return kvPair.key() == kv.first &&
                                    kvPair.value() == kv.second;
                           }),
              kvSet2.pairs().end());
  }
}

}  // namespace

class CommandsRecorderTest : public ::testing::Test {
 protected:
  void TearDown() override {
    fs::path p(outputDir);
    if (fs::exists(p)) {
      fs::remove_all(p);
    }
  }
};

TEST_F(CommandsRecorderTest, CreateCommandTest) {
  CommandsRecorder commands_recorder_(outputDir);

  commands_recorder_.NewCommand(cid);
  auto command = commands_recorder_.GetCommand(cid);
  EXPECT_EQ(cid, command.cid());
}

TEST_F(CommandsRecorderTest, AddCommandExecutionDurationTest) {
  CommandsRecorder commands_recorder_(outputDir);

  commands_recorder_.NewCommand(cid);
  std::chrono::steady_clock::duration dur(3000);
  commands_recorder_.AddCommandExecutionDuration(cid, dur);
  auto command = commands_recorder_.GetCommand(cid);

  EXPECT_EQ(dur, std::chrono::steady_clock::duration(
                     TimeUtil::DurationToNanoseconds(command.exec_duration())));
}

TEST_F(CommandsRecorderTest, AddPreExecutionReadsTest) {
  CommandsRecorder commands_recorder_(outputDir);

  commands_recorder_.NewCommand(cid);
  std::map<std::string, std::string> testKvSet{{"1k", "1v"}, {"2k", "2v"}};
  for (const auto& kv : testKvSet) {
    commands_recorder_.AddPreExecutionRead(cid, kv.first, kv.second);
  }
  VerifyKeyValueSetEqual(
      testKvSet, commands_recorder_.GetCommand(cid).pre_execution_reads());
}

TEST_F(CommandsRecorderTest, AddPostExecutionReadsTest) {
  CommandsRecorder commands_recorder_(outputDir);

  commands_recorder_.NewCommand(cid);
  std::map<std::string, std::string> testKvSet{{"1k", "1v"}, {"2k", "2v"}};
  for (const auto& kv : testKvSet) {
    commands_recorder_.AddPostExecutionRead(cid, kv.first, kv.second);
  }
  VerifyKeyValueSetEqual(
      testKvSet, commands_recorder_.GetCommand(cid).post_execution_reads());
}

TEST_F(CommandsRecorderTest, AddPostExecutionWritesTest) {
  CommandsRecorder commands_recorder_(outputDir);

  commands_recorder_.NewCommand(cid);
  std::map<std::string, std::string> testKvSet{{"1k", "1v"}, {"2k", "2v"}};
  for (const auto& kv : testKvSet) {
    commands_recorder_.AddPostExecutionWrite(cid, kv.first, kv.second);
  }
  VerifyKeyValueSetEqual(
      testKvSet, commands_recorder_.GetCommand(cid).post_execution_writes());
}

TEST_F(CommandsRecorderTest, SaveCommandTest) {
  CommandsRecorder commands_recorder_(outputDir);
  commands_recorder_.NewCommand(cid);
  std::chrono::steady_clock::duration dur(3000);
  commands_recorder_.AddCommandExecutionDuration(cid, dur);
  std::map<std::string, std::string> testKvSet{{"1k", "1v"}, {"2k", "2v"}};
  for (const auto& kv : testKvSet) {
    commands_recorder_.AddPreExecutionRead(cid, kv.first, kv.second);
    commands_recorder_.AddPostExecutionRead(cid, kv.first, kv.second);
    commands_recorder_.AddPostExecutionWrite(cid, kv.first, kv.second);
  }

  EXPECT_TRUE(commands_recorder_.SaveCommand(cid));
  EXPECT_FALSE(commands_recorder_.SaveCommand(cid));

  fs::path p(outputDir);
  p /= cid;
  std::ifstream ifs(p.c_str(), std::ios::binary);

  RecordedCommand cmd;
  cmd.ParseFromIstream(&ifs);

  EXPECT_EQ(cmd.cid(), cid);
  EXPECT_EQ(dur, std::chrono::steady_clock::duration(
                     TimeUtil::DurationToNanoseconds(cmd.exec_duration())));
  VerifyKeyValueSetEqual(testKvSet, cmd.pre_execution_reads());
  VerifyKeyValueSetEqual(testKvSet, cmd.post_execution_reads());
  VerifyKeyValueSetEqual(testKvSet, cmd.post_execution_writes());
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}