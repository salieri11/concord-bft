// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_THIN_REPLICA_SUBSCRIPTION_BUFFER_HPP_
#define CONCORD_THIN_REPLICA_SUBSCRIPTION_BUFFER_HPP_

#include <boost/circular_buffer.hpp>
#include <chrono>
#include <condition_variable>
#include <iostream>
#include <list>
#include <thread>
#include "hash_defs.h"
#include "kv_types.hpp"

namespace concord {
namespace thin_replica {

// A single update from the commands handler
typedef std::pair<concordUtils::BlockId, concordUtils::SetOfKeyValuePairs>
    SubUpdate;

// Each subscriber creates its own ring buffer and puts it into the shared list
// of subscriber buffers. This is a thread-safe implementation around boost's
// circular buffer. We expect a single producer (the commands handler) and a
// single consumer (the subscriber thread in the thin replica gRPC service).
class SubUpdateBuffer {
 private:
  std::mutex buffer_mutex_;
  boost::circular_buffer<SubUpdate> cb_;

  std::mutex cv_mutex_;
  std::condition_variable cv_;

 public:
  SubUpdateBuffer(size_t buffer_size) {
    cb_ = boost::circular_buffer<SubUpdate>(buffer_size);
  }

  // Let's help ourselves and make sure we don't copy this buffer
  SubUpdateBuffer(const SubUpdateBuffer&) = delete;
  SubUpdateBuffer& operator=(const SubUpdateBuffer&) = delete;

  // Add an update to the ring buffer and notify waiting subscribers
  void Push(SubUpdate update) {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    cb_.push_back(update);
    cv_.notify_one();
  };

  // Return the oldest update from the ring buffer and block if no update is
  // available
  SubUpdate Pop() {
    if (cb_.empty()) {
      // Wait until an update is added to the buffer
      std::unique_lock<std::mutex> lock(cv_mutex_);
      cv_.wait(lock);
    }
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    auto out = cb_.front();
    cb_.pop_front();
    return out;
  };

  bool Empty() { return cb_.empty(); }
};

// Thread-safe list implementation which manages subscribers' ring buffers. You
// can think of this list as the list of subscribers whereby each subscriber is
// represented by its ring buffer. The presence or absence of a buffer
// determines whether a subscriber is subscribed or unsubscribed respectively.
class SubBufferList {
 private:
  std::list<std::shared_ptr<SubUpdateBuffer>> subscriber_;
  std::mutex mutex_;

 public:
  SubBufferList() {}

  // Let's help ourselves and make sure we don't copy this list
  SubBufferList(const SubBufferList&) = delete;
  SubBufferList& operator=(const SubBufferList&) = delete;

  // Add a subscriber
  void AddBuffer(std::shared_ptr<SubUpdateBuffer> elem) {
    std::lock_guard<std::mutex> lock(mutex_);
    subscriber_.push_back(elem);
  }

  // Remove a subscriber
  void RemoveBuffer(std::shared_ptr<SubUpdateBuffer> elem) {
    std::lock_guard<std::mutex> lock(mutex_);
    subscriber_.remove(elem);
  }

  // Populate updates to all subscribers
  void UpdateSubBuffers(SubUpdate update) {
    std::lock_guard<std::mutex> lock(mutex_);
    for (const auto& it : subscriber_) {
      it->Push(update);
    }
  }
};

}  // namespace thin_replica
}  // namespace concord

#endif  // CONCORD_THIN_REPLICA_SUBSCRIPTION_BUFFER_HPP_
