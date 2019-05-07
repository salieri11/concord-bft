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

#include <log4cplus/loggingmacros.h>
#include <thread>

#include "config/configuration_manager.hpp"
#include "consensus/kvb_client.hpp"

namespace concord {
namespace time {

class TimePusher {
 public:
  explicit TimePusher(const concord::config::ConcordConfiguration &nodeConfig,
                      concord::consensus::KVBClientPool &clientPool);

  void Start();
  void Stop();

 private:
  log4cplus::Logger logger_;
  const concord::config::ConcordConfiguration &nodeConfig_;
  concord::consensus::KVBClientPool &clientPool_;
  bool stop_;
  int periodMilliseconds_;
  std::thread pusherThread_;

  void ThreadFunction();
};

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_PUSHER_HPP
