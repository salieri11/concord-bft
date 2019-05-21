// Copyright 2019 VMware, all rights reserved
//
// Time pusher is a thread to make sure that this Concord node publishes its
// time at least once within some configured period.
//
// Initial implementation: just send a transactions once/period.
//
// Future implementation: wake up once/period, and check if any transactions
// were sent while this thread was sleeping, and only send a transaction if no
// others were.

#ifndef TIME_TIME_PUSHER_HPP
#define TIME_TIME_PUSHER_HPP

#include <log4cplus/loggingmacros.h>
#include <mutex>
#include <thread>

#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/kvb_client.hpp"

namespace concord {
namespace time {

class TimePusher {
 public:
  explicit TimePusher(const concord::config::ConcordConfiguration &config,
                      const concord::config::ConcordConfiguration &nodeConfig,
                      concord::consensus::KVBClientPool &clientPool);

  void Start();
  void Stop();

  bool IsTimeServiceEnabled() const;
  void AddTimeToCommand(com::vmware::concord::ConcordRequest &command);

 private:
  void AddTimeToCommand(com::vmware::concord::ConcordRequest &command,
                        uint64_t time);

 private:
  log4cplus::Logger logger_;
  concord::consensus::KVBClientPool &clientPool_;
  bool stop_;
  std::atomic_uint64_t lastPublishTimeMs_;

  bool timeServiceEnabled_;
  int periodMilliseconds_;
  std::string timeSourceId_;

  std::thread pusherThread_;
  std::mutex threadMutex_;

  void ThreadFunction();
};

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_PUSHER_HPP
