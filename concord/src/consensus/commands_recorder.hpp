// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_COMMANDS_RECORDER_HPP_
#define CONCORD_COMMANDS_RECORDER_HPP_

#include <chrono>
#include <map>

#include "performance.pb.h"

namespace concord {
namespace consensus {
class CommandsRecorder {
 public:
  CommandsRecorder(const std::string& outputDir);

  bool NewCommand(const std::string& cid);

  const com::vmware::concord::performance::RecordedCommand& GetCommand(
      const std::string& cid) const;

  void AddCommandExecutionDuration(
      std::string const& cid, std::chrono::steady_clock::duration duration);

  void AddPreExecutionRead(const std::string& cid, std::string key,
                           std::string value);
  void AddPostExecutionRead(const std::string& cid, std::string key,
                            std::string value);

  void AddPostExecutionWrite(const std::string& cid, std::string key,
                             std::string value);

  bool SaveCommand(const std::string& cid);

 private:
  std::map<std::string, com::vmware::concord::performance::RecordedCommand>
      recorded_commands_;
  std::string output_dir_;
};
}  // namespace consensus

}  // namespace concord

#endif  // CONCORD_COMMANDS_RECORDER_HPP_