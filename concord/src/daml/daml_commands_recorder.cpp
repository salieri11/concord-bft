// Copyright 2020 VMware, all rights reserved

#include "daml_commands_recorder.hpp"

#include <google/protobuf/util/time_util.h>
#include <boost/filesystem.hpp>
#include <fstream>

namespace fs = boost::filesystem;

using google::protobuf::util::TimeUtil;
using namespace com::vmware::concord::performance;

namespace concord {
namespace daml {

DamlCommandsRecorder::DamlCommandsRecorder(const std::string& outputDir)
    : output_dir_(outputDir) {}

bool DamlCommandsRecorder::NewCommand(const std::string& cid) {
  RecordedCommand record;
  record.set_cid(cid);
  return recorded_commands_.emplace(cid, record).second;
}

const RecordedCommand& DamlCommandsRecorder::GetCommand(
    const std::string& cid) const {
  return recorded_commands_.at(cid);
}

void DamlCommandsRecorder::AddCommandExecutionDuration(
    const std::string& cid, std::chrono::steady_clock::duration duration) {
  auto d = TimeUtil::NanosecondsToDuration(duration.count());
  auto dur = recorded_commands_[cid].mutable_exec_duration();
  dur->set_seconds(d.seconds());
  dur->set_nanos(d.nanos());
}

void DamlCommandsRecorder::AddPreExecutionRead(const std::string& cid,
                                               std::string key,
                                               std::string value) {
  auto pre_exec_reads = recorded_commands_[cid].mutable_pre_execution_reads();

  auto kv_pair = pre_exec_reads->add_pairs();
  kv_pair->set_key(std::move(key));
  kv_pair->set_value(std::move(value));
}

void DamlCommandsRecorder::AddPostExecutionRead(const std::string& cid,
                                                std::string key,
                                                std::string value) {
  auto post_exec_reads = recorded_commands_[cid].mutable_post_execution_reads();

  auto kv_pair = post_exec_reads->add_pairs();
  kv_pair->set_key(std::move(key));
  kv_pair->set_value(std::move(value));
}

void DamlCommandsRecorder::AddPostExecutionWrite(const std::string& cid,
                                                 std::string key,
                                                 std::string value) {
  auto post_exec_writes =
      recorded_commands_[cid].mutable_post_execution_writes();

  auto kv_pair = post_exec_writes->add_pairs();
  kv_pair->set_key(std::move(key));
  kv_pair->set_value(std::move(value));
}

bool DamlCommandsRecorder::SaveCommand(const std::string& cid) {
  auto command = recorded_commands_.find(cid);

  if (command == recorded_commands_.end()) {
    return false;
  }

  if (command->second.cid().empty()) {
    command->second.set_cid(cid);
  }

  fs::path p(output_dir_);
  if (!fs::exists(p)) {
    fs::create_directory(p);
  };

  fs::path file = p / cid;
  std::ofstream ofs(file.c_str(), std::ios::binary);
  return command->second.SerializeToOstream(&ofs) &&
         recorded_commands_.erase(cid) == 1;
}
}  // namespace daml
}  // namespace concord