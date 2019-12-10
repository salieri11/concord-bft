// Copyright 2019 VMware, all rights reserved
//
// Primary Thin Replica Client Library header file; you should include this file
// to use the Thin Replica Client Library.
//
// Several components of the thin replica client library are declared in this
// file:
//  - class UpdateQueue - A synchronized queue type to be used in transferring
//    updates between the Thin Replica Client and an application.
//  - class RingBufferUpdateQueue - Basic implementation of UpdateQueue
//    provided by this library.
//  - class ThinReplicaClient - Primary interface and implementation of the thin
//    replica client.
//
// Some general notes about the Thin Replica Client Library:
//  - std::string is used for passing byte strings to and from the library; this
//    is considered a cleaner solution than passing pointer/length pairs.
//    Note std::string is appropriate for byte strings because it is defined to
//    be std::basic_string<char> and chars are guaranteed to be one byte.
//  - The thin replica client mechanism associates each update with a specific
//    Block ID. Block IDs are 64-bit unsigned integer quantities. Updates a Thin
//    Replica Client receives from the Thin Replica Servers will have
//    monotonically increasing block numbers. An application using the
//    ThinReplicaClient Library should persist at least the block number for the
//    most recent update it received, as that block number can be used to resume
//    a subscription without having to first stream all the non-pruned updates
//    preceding that block number. An application using the ThinReplicaClient
//    library should use the ThinReplicaClient::AcknowledgeBlock to acknowledge
//    the block ID for each update it receives, as this is useful to the thin
//    replica servers for pruning decisions.
//  - It should be noted that the ThinReplicaClient itself logs via log4cplus;
//    specifically, it will log any noteworthy abnormalities it observes but
//    which it is its job to abstract out for the application using the library
//    rather than programatically expose to the application (ex: disagreement
//    among Thin Replica Servers).

#ifndef THIN_REPLICA_CLIENT_HPP_
#define THIN_REPLICA_CLIENT_HPP_

#include "thin_replica.grpc.pb.h"

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>

namespace thin_replica_client {

// Type for updates the Thin Replica Client streams from Thin Replica Servers.
struct Update {
  // Block ID for this update; Block IDs can be expected to be monotonically
  // increasing with each update received in order. It is recommended that
  // applications receiving updates persist at least the Block ID of the most
  // recently received update (though only after any data from the update to be
  // persisted has been persisted), as Block IDs can be used to resume a
  // subscription without having to stream updates preceding this Block ID.
  uint64_t block_id;

  // Actual data for this update, structured as updates to key value pairs.
  std::vector<std::pair<std::string, std::string>> kv_pairs;
};

// Interface for a synchronized queue to be used to transfer updates between a
// ThinReplicaClient and an application. The Thin Replica Client Library
// provides RingBufferUpdateQueue as an implementation of this interface,
// though applications are also free to provide their own implementation of
// UpdateQueue if they have reason to do so (for example, an application could
// provide its own implementation of an UpdateQueue if it wanted to handle how
// memory is allocated for the queue differently). An implementation of
// UpdateQueue should guarantee that Clear, Push, Pop, and TryPop are thread
// safe.
class UpdateQueue {
 public:
  virtual ~UpdateQueue() {}

  // Synchronously clear all current updates in the queue. May block the calling
  // thread as necessary to obtain any lock(s) needed for this operation.
  virtual void Clear() = 0;

  // Synchronously push a new update to the back of the queue. May block the
  // calling thread as necessary to obtain any lock(s) needed for this
  // operation. May throw an exception if space cannot be found or allocated in
  // the queue for the update being pushed. Note the update is passed by
  // unique_ptr, as this operation gives ownership of the allocated update to
  // the queue. UpdateQueue implementations may choose whether they keep this
  // allocated Update or free it after storing the data from the update by some
  // other means.
  virtual void Push(std::unique_ptr<Update> update) = 0;

  // Synchronously pop and return the update at the front of the queue. If there
  // are no updates available in the queue, this function should block the
  // calling thread and wait until an update that can be popped is available.
  // This function may also block the calling thread for purposes of obtaining
  // any lock(s) needed for these operations. This function gives ownership of
  // an allocated Update to the calller; implementations of UpdateQueue may
  // choose whether they give ownership of an allocated Update they own to the
  // caller or whether they dynamically allocate memory (via
  // malloc/new/std::allocator(s)) to construct the Update object from data the
  // Queue has stored by some other means.
  virtual std::unique_ptr<Update> Pop() = 0;

  // Synchronously pop an update from the front of the queue if one is
  // available, but do not block the calling thread to wait on one if one is not
  // immediately found. Returns a unique_ptr to nullptr if no update is
  // immediately found, and a unique_ptr giving ownership of an allocated Update
  // otherwise (this may involve dynamic memory allocation at the discretion of
  // the UpdateQueue implementation).
  virtual std::unique_ptr<Update> TryPop() = 0;
};

// Basic UpdateQueue implementation that stores keeps unique_ptrs to updates
// allocated by the Push caller in a ring buffer of a size set at queue
// construction time.
class RingBufferUpdateQueue : public UpdateQueue {
 private:
 public:
  // Construct a RingBufferUpdateQueue with a RingBuffer for unique_ptrs of the
  // given size.
  RingBufferUpdateQueue(size_t buffer_size);

  virtual ~RingBufferUpdateQueue() override;
  RingBufferUpdateQueue(const RingBufferUpdateQueue& other);
  RingBufferUpdateQueue(const RingBufferUpdateQueue&& other);
  RingBufferUpdateQueue& operator=(const RingBufferUpdateQueue& other);
  RingBufferUpdateQueue& operator=(const RingBufferUpdateQueue&& other);

  // Implementation of UpdateQueue interface.
  virtual void Clear() override;
  virtual void Push(std::unique_ptr<Update> update) override;
  virtual std::unique_ptr<Update> Pop() override;
  virtual std::unique_ptr<Update> TryPop() override;
};

// Thin Replica Client implementation; used to subscribe to and stream updates
// from thin replica servers. Note the ThinReplicaClient is intended to
// error-handle Byzantine failures among the thin replica servers; a
// ThinReplicaClient object will not accept an update unless it can find at
// least (max_faulty + 1) servers in agreement about the update. The
// ThinReplicaClient should also handle moving its subscription away from faulty
// servers in the event a server becomes faulty (assuming some set of
// (max_faulty + 1) agreeing servers still exists); this fail-over process
// should be automatic and tranparent to code consuming the ThinReplicaClient.
class ThinReplicaClient final {
 private:
  log4cplus::Logger logger_;
  std::vector<
      std::unique_ptr<com::vmware::concord::thin_replica::ThinReplica::Stub>>
      server_stubs_;

 public:
  // Constructor for ThinReplicaClient. Note that, as the ThinReplicaClient
  // protocol allows only one active subscription at a time for a given client,
  // the ThinReplicaClient library only allows the use of a single
  // ThinReplicaClient at a time; an exception will be thrown if  this
  // constructor is called when there already exists a ThinReplicaClient in the
  // calling process that has been constructed but not destructed. Parameters:
  // - update_queue: shared pointer to an UpdateQueue object to be used to
  //   transfer updates from this ThinReplicaClient to the application.
  // - max_faulty: Maximum number of simultaneously Byzantine-faulty servers
  //   that must be tolerated (this is equivalent to the F value for the Concord
  //   cluster the servers are from).
  // - private_key: Private cryptographic key identifying and authenticating the
  //   ThinReplicaClient being constructed to the Thin Replica Servers.
  // - begin_servers: Iterator returning elements of type
  //   std::pair<std::string, std::shared_ptr<grpc::Channel>> representing each
  //   server available for this ThinReplicaClient to connect to; the string in
  //   each pair should be the public key used to identify and validate messages
  //   from this server, and the pointer to a grpc::Channel should provide a
  //   gRPC connection to that Thin Replica Server. Furthermore, the order this
  //   iterator returns the server will be used by this ThinReplicaClient as
  //   the preferred order to try connecting to the servers.
  // - end_servers: End iterator corresponding to begin_servers.
  template <class Iterator>
  ThinReplicaClient(std::shared_ptr<UpdateQueue> update_queue,
                    uint16_t max_faulty, const std::string& private_key,
                    Iterator begin_servers, Iterator end_servers)
      : logger_(
            log4cplus::Logger::getInstance("com.vmware.thin_replica_client")),
        server_stubs_() {
    while (begin_servers != end_servers) {
      const auto& server_info = *begin_servers;
      server_stubs_.push_back(
          com::vmware::concord::thin_replica::ThinReplica::NewStub(
              server_info.second));
      ++begin_servers;
    }

    if (server_stubs_.size() < (3 * (size_t)max_faulty + 1)) {
      size_t num_servers = server_stubs_.size();
      server_stubs_.clear();
      throw std::invalid_argument(
          "Too few servers (" + std::to_string(num_servers) +
          ") given to ThinReplicaClient constructor to tolerate requested "
          "maximum faulty servers (" +
          std::to_string(max_faulty) +
          "). The number of servers must be at least (3 * max_faulty + 1).");
    }

    // TODO (Alex): Enforce that, as far as this constructor can see (likely the
    //              virtual memory for the process it is running in), only one
    //              ThinReplicaClient is allowed to exist at a time.
  }

  ~ThinReplicaClient();

  // Copying or moving of a ThinReplicaClient object is explicitly disallowed as
  // only 1 ThinReplicaClient at a time is allowed to exist by this library.
  ThinReplicaClient(const ThinReplicaClient& other) = delete;
  ThinReplicaClient(const ThinReplicaClient&& other) = delete;
  ThinReplicaClient& operator=(const ThinReplicaClient& other) = delete;
  ThinReplicaClient& operator=(const ThinReplicaClient&& other) = delete;

  // Register a condition variable for this ThinReplicaClient to notify in the
  // event it has an active subscription and becomes unable to receive or
  // validate a new update, given that condition variable and a mutex
  // controlling access to that condition variable. Reasons the
  // ThinReplicaClient may become unable to receive or validate updates include
  // disconnection from or unresponsiveness of Thin Replica Server(s),
  // disagreement among Thin Replica Server(s), or some combination thereof.
  // Note the ThinReplicaClient will notify all waiting threads (not just one
  // waiting thread) on the failure conditioin. Note ThinReplicaClient only
  // supports the registration of a single condition variable for subscription
  // failure; calling this function when a condition variable is already
  // registered will overwrite the existing condition (without notifying any
  // threads waiting on it).
  void RegisterSubscriptionFailureCondition(
      std::shared_ptr<std::mutex> failure_mutex,
      std::shared_ptr<std::condition_variable> failure_condition);

  // Subscribe to updates from the Thin Replica Servers. key_prefix_bytes should
  // be a byte string to request the thin replica servers filter updates on
  // before sending them. If a value for last_known_block_id is given, the
  // ThinReplicaClient will begin the subscription at the point specifed by that
  // Block ID, otherwise, subscription will begin by attempting to read all
  // current state. Once the subscription is successfully setup, each
  // update received will be pushed to the update_queue this ThinReplicaClient
  // was constructed with. If this ThinReplicaClient already has an active
  // subscription open, that subscription will be ended when Subscribe is
  // called. If there are any updates leftover in update_queue when Subscribe is
  // called, the queue will be cleared. Any initial state received when opening
  // a new subscription will be treated the same as regular updates and pushed
  // to the queue. The subscribe call should be expected to block the calling
  // thread until the subscription is successfully completed and any initial
  // state is read; Subscribe will throw an exception if it cannot successfully
  // make the requested subscription.
  void Subscribe(const std::string& key_prefix_bytes);
  void Subscribe(const std::string& key_prefix_bytes,
                 uint64_t last_known_block_id);

  // End any currently open subscription this ThinReplicaClient has. Note this
  // function will not automatically clear any updates still in the
  // update_queue. Note Unsubscribe will effectively do nothing if called for a
  // ThinReplicaClient that has no active subscription.
  void Unsubscribe();

  // Acknowledge receipt of the update for a given Block ID to the thin replica
  // servers. This function should not be called unless the application has
  // persisted whatever it needs for the update as, after calling
  // AcknowledgeBlockID, the Thin Replica Servers are free to slate the update
  // for pruning.
  void AcknowledgeBlockID(uint64_t block_id);
};

}  // namespace thin_replica_client

#endif  // THIN_REPLICA_CLIENT_HPP_
