// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_THIN_REPLICA_SUBSCRIPTION_BUFFER_HPP_
#define CONCORD_THIN_REPLICA_SUBSCRIPTION_BUFFER_HPP_

#include <boost/circular_buffer.hpp>
#include <chrono>
#include <condition_variable>
#include <iostream>
#include <list>
#include <thread>
#include "kv_types.hpp"

namespace concord {
namespace thin_replica {

// A single update from the commands handler
typedef std::pair<kvbc::BlockId, kvbc::SetOfKeyValuePairs> SubUpdate;

// Each subscriber creates its own ring buffer and puts it into the shared list
// of subscriber buffers. This is a thread-safe implementation around boost's
// circular buffer. We expect a single producer (the commands handler) and a
// single consumer (the subscriber thread in the thin replica gRPC service).
class SubUpdateBuffer {
 private:
  std::mutex buffer_mutex_;
  std::condition_variable cv_;
  boost::circular_buffer<SubUpdate> cb_;

 public:
  explicit SubUpdateBuffer(size_t buffer_size) : cb_(buffer_size) {}

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
    // NOTE(DD): We do not use WaitUntilNonEmpty to wait
    // because we want to leave the buffer_mutex_ locked
    // while extracting an item
    std::unique_lock<std::mutex> lock(buffer_mutex_);
    cv_.wait(lock, [this] { return !cb_.empty(); });
    auto out = cb_.front();
    cb_.pop_front();
    return out;
  };

  void WaitUntilNonEmpty() {
    std::unique_lock<std::mutex> lock(buffer_mutex_);
    cv_.wait(lock, [this] { return !cb_.empty(); });
  }

  template <typename RepT, typename PeriodT>
  [[nodiscard]] bool WaitUntilNonEmpty(
      const std::chrono::duration<RepT, PeriodT>& duration) {
    std::unique_lock<std::mutex> lock(buffer_mutex_);
    return cv_.wait_for(lock, duration, [this] { return !cb_.empty(); });
  }

  void RemoveAllUpdates() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    cb_.erase_begin(cb_.size());
  }

  kvbc::BlockId NewestBlockId() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return cb_.back().first;
  }

  kvbc::BlockId OldestBlockId() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return cb_.front().first;
  }

  bool Empty() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return cb_.empty();
  }

  bool Full() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return cb_.full();
  }

  size_t Size() {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return cb_.size();
  }
};

// Thread-safe list implementation which manages subscribers' ring buffers. You
// can think of this list as the list of subscribers whereby each subscriber is
// represented by its ring buffer. The presence or absence of a buffer
// determines whether a subscriber is subscribed or unsubscribed respectively.
class SubBufferList {
 protected:
  std::list<std::shared_ptr<SubUpdateBuffer>> subscriber_;
  std::mutex mutex_;

 public:
  SubBufferList() {}

  // Let's help ourselves and make sure we don't copy this list
  SubBufferList(const SubBufferList&) = delete;
  SubBufferList& operator=(const SubBufferList&) = delete;

  // Add a subscriber
  virtual void AddBuffer(std::shared_ptr<SubUpdateBuffer> elem) {
    std::lock_guard<std::mutex> lock(mutex_);
    subscriber_.push_back(elem);
  }

  // Remove a subscriber
  virtual void RemoveBuffer(std::shared_ptr<SubUpdateBuffer> elem) {
    std::lock_guard<std::mutex> lock(mutex_);
    subscriber_.remove(elem);
  }

  // Populate updates to all subscribers
  virtual void UpdateSubBuffers(SubUpdate update) {
    std::lock_guard<std::mutex> lock(mutex_);
    for (const auto& it : subscriber_) {
      it->Push(update);
    }
  }
  virtual ~SubBufferList() = default;
};

}  // namespace thin_replica
}  // namespace concord

#endif  // CONCORD_THIN_REPLICA_SUBSCRIPTION_BUFFER_HPP_
