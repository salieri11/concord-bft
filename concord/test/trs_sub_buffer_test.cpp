// Copyright 2020 VMware, all rights reserved

#include "gtest/gtest.h"

#include <atomic>
#include <future>
#include "Logger.hpp"
#include "thin_replica/subscription_buffer.hpp"

namespace {

using std::make_pair;

using concord::kvbc::Key;
using concord::kvbc::Value;
using concord::thin_replica::ConsumerTooSlow;
using concord::thin_replica::SubBufferList;
using concord::thin_replica::SubUpdate;
using concord::thin_replica::SubUpdateBuffer;

// A producer should be able to "add" updates whether there are consumers or
// not. Meaning, the producer does not get interrupted/disturbed if no
// consumers are present.
TEST(trs_sub_buffer_test, no_consumer) {
  SubBufferList sub_list;
  SubUpdate update{1337, {{Key("key"), Value("value")}}};
  for (unsigned i = 0; i < 100; ++i) {
    EXPECT_NO_THROW(sub_list.UpdateSubBuffers(update));
  }
}

// A producer should be able to "add" an update even if the underlying queue is
// full. Practically, this means that an update gets dropped. This will result
// in updates with gaps for the consumer. Eventually, the TRC will receive this
// gap and terminate the subscription due to a mismatch. Test that we throw an
// exception to inform the consumer.
TEST(trs_sub_buffer_test, throw_exception_if_consumer_too_slow) {
  SubBufferList sub_list;
  SubUpdate update{1337, {{Key("key"), Value("value")}}};
  auto updates = std::make_shared<SubUpdateBuffer>(10);

  sub_list.AddBuffer(updates);
  for (unsigned i = 0; i < 11; ++i) {
    sub_list.UpdateSubBuffers(update);
  }

  SubUpdate consumer_update;
  EXPECT_THROW(updates->Pop(consumer_update), ConsumerTooSlow);
}

TEST(trs_sub_buffer_test, block_consumer_if_no_updates_available) {
  SubBufferList sub_list;
  std::atomic_bool reader_started;
  auto updates = std::make_shared<SubUpdateBuffer>(10);
  auto reader = std::async(std::launch::async, [&] {
    SubUpdate update;
    reader_started = true;
    updates->Pop(update);
    ASSERT_EQ(update.first, 1337);
  });

  sub_list.AddBuffer(updates);

  // Let's make sure that the reader thread starts first
  while (!reader_started)
    ;

  SubUpdate update{1337, {{Key("key"), Value("value")}}};
  sub_list.UpdateSubBuffers(update);
}

TEST(trs_sub_buffer_test, happy_path_w_two_consumers) {
  SubBufferList sub_list;
  SubUpdate update{0, {{Key("key"), Value("value")}}};
  auto updates1 = std::make_shared<SubUpdateBuffer>(10);
  auto updates2 = std::make_shared<SubUpdateBuffer>(10);
  int num_updates = 10;

  auto reader_fn = [](std::shared_ptr<SubUpdateBuffer> q, int max) {
    SubUpdate update;
    int counter = 0;
    do {
      q->Pop(update);
      ASSERT_EQ(update.first, counter);
    } while (++counter < max);
  };
  auto reader1 =
      std::async(std::launch::async, reader_fn, updates1, num_updates);
  auto reader2 =
      std::async(std::launch::async, reader_fn, updates2, num_updates);

  sub_list.AddBuffer(updates1);
  sub_list.AddBuffer(updates2);

  for (int i = 0; i < num_updates; ++i) {
    update.first = i;
    sub_list.UpdateSubBuffers(update);
  }
}

}  // namespace

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  auto logger = logging::getLogger("trs_sub_buffer_test");
  return RUN_ALL_TESTS();
}
