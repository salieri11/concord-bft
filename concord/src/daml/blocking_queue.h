
#pragma once
#include <condition_variable>
#include <deque>
#include <mutex>

namespace concord {
namespace daml {

template <typename T>
class BlockingPersistentQueueReader {
  std::deque<T>& deque_;
  std::mutex& mutex_;
  std::condition_variable& cond_;

  // Last position read from the deque.
  size_t position_;

  typedef std::unique_lock<std::mutex> ulock;

 public:
  BlockingPersistentQueueReader(size_t offset, std::deque<T>& deque,
                                std::mutex& mutex,
                                std::condition_variable& cond)
      : deque_(deque), mutex_(mutex), cond_(cond), position_(offset) {}

  void discard(size_t n) { position_ += n; }

  T pop() {
    ulock u(mutex_);

    // Wait until a new element (from this reader's perspective)
    // is pushed to the deque.
    while (deque_.size() <= position_) {
      cond_.wait(u);
    }

    T v = deque_.at(position_);
    position_ += 1;
    return v;
  }
};

template <typename T>
class BlockingPersistentQueue {
  std::deque<T> deque_;
  std::mutex mutex_;
  std::condition_variable cond_;

  typedef std::lock_guard<std::mutex> lock;

 public:
  void push(T const& v) {
    lock l(mutex_);
    deque_.push_back(v);
    cond_.notify_all();
  }

  BlockingPersistentQueueReader<T> newReader(size_t offset) {
    return BlockingPersistentQueueReader<T>(offset, deque_, mutex_, cond_);
  }
};

}  // namespace daml
}  // namespace concord
