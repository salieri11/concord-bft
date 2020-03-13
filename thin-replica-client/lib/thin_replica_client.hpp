// Copyright 2019-2020 VMware, all rights reserved
//
// Primary Thin Replica Client Library header file; you should include this file
// to use the Thin Replica Client Library.
//
// Several components of the thin replica client library are declared in this
// file:
//  - class UpdateQueue - A synchronized queue type to be used in transferring
//    updates between the Thin Replica Client and an application.
//  - class BasicUpdateQueue - Basic implementation of UpdateQueue
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
#include "update.hpp"

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>
#include <thread>

namespace thin_replica_client {

// The default message size for incoming data is 4MiB but certain workloads
// demand a higher limit. With the tested workloads, the incoming message size
// from the TRS is less than 16MiB. This correlates with the maximum message
// size that we specify for the SBFT protocol in Concord's configuration file.
// Note: We can set the upper bound to unlimited (-1) but we do want to know
// when & why the message size increases.
const int kGrpcMaxInboundMsgSizeInBytes = 1 << 24;

// Interface for a synchronized queue to be used to transfer updates between a
// ThinReplicaClient and an application. The Thin Replica Client Library
// provides BasicUpdateQueue as an implementation of this interface, though
// applications are also free to provide their own implementation of UpdateQueue
// if they have reason to do so (for example, an application could provide its
// own implementation of an UpdateQueue if it wanted to handle how memory is
// allocated for the queue differently). An implementation of UpdateQueue should
// guarantee that ReleaseConsumers, Clear, Push, Pop, and TryPop are all
// mutually thread safe.
class UpdateQueue {
 public:
  // Destructor for UpdateQueue (which UpdateQueue implementations should
  // override). Note the UpdateQueue interface does NOT require that
  // implementations guarantee the desctructor be thread safe the UpdateQueue
  // functions; behavior may be undefined if any other function of an
  // UpdateQueue executes at all concurrently with that queue's destructor after
  // that destructor has begun executing, or at any point after the destructor
  // has begun running (including after the destructor has completed).
  // Furthermore, behavior is undefined if any thread is still waiting on the
  // blocking UpdateQueue:Pop call when the destructor begins running. Code
  // owning UpdateQueue instances should guarantee that there are no outstanding
  // calls to the UpdateQueue's functions and that no thread will start new ones
  // before destroying an instance. Note the ReleaseConsumers function can be
  // used to have the queue unblock and release any threads still waiting on
  // UpdateQueue::Pop calls.
  virtual ~UpdateQueue() {}

  // Release any threads currently waiting on blocking UpdateQueue::Pop calls
  // made to this UpdateQueue, making those calls return pointers to null;
  // making this call also puts the UpdateQueue into a state where any new calls
  // to the UpdateQueue::Pop function will return a pointer to null instead of
  // waiting on new updates to become available. This function may block the
  // caller to obtain locks if that is necessary for this operation under the
  // UpdateQueue's implementation.
  virtual void ReleaseConsumers() = 0;

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

  // Synchronously pop and return the update at the front of the queue.
  // Normally, if there are no updates available in the queue, this function
  // should block the calling thread and wait until an update that can be popped
  // is available. This function may also block the calling thread for purposes
  // of obtaining any lock(s) needed for these operations. This function gives
  // ownership of an allocated Update to the calller; implementations of
  // UpdateQueue may choose whether they give ownership of an allocated Update
  // they own to the caller or whether they dynamically allocate memory (via
  // malloc/new/std::allocator(s)) to construct the Update object from data the
  // Queue has stored by some other means. If ReleaseConsumers is called, any
  // currently waiting Pop calls will be unblocked, and will return a unique_ptr
  // to null rather than continuing to wait for new updates. Furthermore, a call
  // to ReleaseConsumers will cause any subsequent calls to Pop to return
  // nullptr and will prevent them from blocking their caller.
  virtual std::unique_ptr<Update> Pop() = 0;

  // Synchronously pop an update from the front of the queue if one is
  // available, but do not block the calling thread to wait on one if one is not
  // immediately found. Returns a unique_ptr to nullptr if no update is
  // immediately found, and a unique_ptr giving ownership of an allocated Update
  // otherwise (this may involve dynamic memory allocation at the discretion of
  // the UpdateQueue implementation).
  virtual std::unique_ptr<Update> TryPop() = 0;
};

// Basic UpdateQueue implementation provided by this library. This class can be
// expected to adhere to the UpdateQueue interface, but details of this class's
// implementation should be considered subject to change as we may revise it as
// we implement, harden, and test the Thin Replica Client Library and develop a
// more complete understanding of the needs of this library.
class BasicUpdateQueue : public UpdateQueue {
 private:
  std::list<std::unique_ptr<Update>> queue_data_;
  std::mutex mutex_;
  std::condition_variable condition_;
  bool release_consumers_;

 public:
  // Construct a BasicUpdateQueue.
  BasicUpdateQueue();

  // Copying or moving a BasicUpdateQueue is explicitly disallowed, as we do not
  // know of a compelling use case requiring copying or moving
  // BasicUpdateQueues, we believe semantics for these operations are likely to
  // be messy in some caess, and we believe implementation may be non-trivial.
  // We may revisit the decision to disallow these operations should compelling
  // use cases for them be found in the future.
  BasicUpdateQueue(const BasicUpdateQueue& other) = delete;
  BasicUpdateQueue(const BasicUpdateQueue&& other) = delete;
  BasicUpdateQueue& operator=(const BasicUpdateQueue& other) = delete;
  BasicUpdateQueue& operator=(const BasicUpdateQueue&& other) = delete;

  // Implementation of UpdateQueue interface
  virtual ~BasicUpdateQueue() override;
  virtual void ReleaseConsumers() override;
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
  std::string client_id_;
  std::vector<std::unique_ptr<
      com::vmware::concord::thin_replica::ThinReplica::StubInterface>>
      server_stubs_;
  std::shared_ptr<UpdateQueue> update_queue_;
  uint16_t max_faulty_;

  // Condition variable to notify if an active subscription can no longer
  // progress.
  std::shared_ptr<std::condition_variable> subscription_failure_condition_;

  // Additional mutex needed to prevent race conditions that could arise if
  // ThinReplicaClient::RegisterSubscriptionFailureCondition runs concurrently
  // with an attempt from subscription worker thread(s) to notify the failure
  // condition.
  std::mutex failure_condition_mutex_;

  std::string key_prefix_;
  uint64_t latest_verified_block_id_;

  std::unique_ptr<std::thread> subscription_thread_;
  std::atomic_bool stop_subscription_thread_;

  std::unique_ptr<
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Data>>
      subscription_data_stream_;
  std::unique_ptr<grpc::ClientContext> sub_data_context_;
  size_t current_data_source_;
  std::vector<std::unique_ptr<
      grpc::ClientReaderInterface<com::vmware::concord::thin_replica::Hash>>>
      subscription_hash_streams_;
  std::vector<std::unique_ptr<grpc::ClientContext>> sub_hash_contexts_;

  // Function(s) for computing hashes as we anticipate they exist according to
  // non-faulty Thin Replica Servers.
  // XXX: We anticipate it is very possible the data type for hashes and hash
  //      function implementation(s) are likely to change between the time of
  //      this comment's writing and the time the Thin Replica mechanism is
  //      hardened for production.
  typedef size_t StateHashType;
  StateHashType AppendToReadStateHash(
      StateHashType preceding_hash,
      const std::pair<std::string, std::string>& kvp) const;
  typedef size_t UpdateHashType;
  UpdateHashType AppendToSubscribeToUpdatesHash(
      UpdateHashType preceding_hash,
      const std::pair<std::string, std::string>& kvp) const;

  // Thread function to start subscription_thread_ with.
  void ReceiveUpdates();

  // Helper functions to ReceiveUpdates.
  UpdateHashType ComputeUpdateDataHash(
      const com::vmware::concord::thin_replica::Data& data) const;
  void RecordCollectedHash(
      size_t update_source, uint64_t block_id, UpdateHashType update_hash,
      std::map<std::pair<uint64_t, UpdateHashType>, std::unordered_set<size_t>>&
          server_indexes_by_reported_update,
      size_t& maximal_agreeing_subset_size,
      std::pair<uint64_t, UpdateHashType>& maximally_agreed_on_update);
  void ReadUpdateHashFromStream(
      size_t server_index,
      std::map<std::pair<uint64_t, UpdateHashType>, std::unordered_set<size_t>>&
          server_indexes_by_reported_update,
      size_t& maximal_agreeing_subset_size,
      std::pair<uint64_t, UpdateHashType>& maximally_agreed_on_update);
  template <class ReaderType>
  void CloseStream(std::unique_ptr<ReaderType>& stream,
                   std::unique_ptr<grpc::ClientContext>& context);

 public:
  // Constructor for ThinReplicaClient. Note that, as the ThinReplicaClient
  // protocol allows only one active subscription at a time for a given client,
  // the ThinReplicaClient library only allows the use of a single
  // ThinReplicaClient at a time; an exception will be thrown if  this
  // constructor is called when there already exists a ThinReplicaClient in the
  // calling process that has been constructed but not destructed. Parameters:
  // - update_queue: shared pointer to an UpdateQueue object to be used to
  //   transfer updates from this ThinReplicaClient to the application. Note
  //   ThinRepliaClient guarantees it will only use the Clear and Push functions
  //   of this queue; the ThinReplicaClient will never call Pop, TryPop,
  //   ReleaseConsumers, or ReEnableConsumers. Furthermore, a ThinReplicaClient
  //   guarantees it will never execute the Clear or Push functions of the queue
  //   after that ThinReplicaClient's destructor has returned.
  // - max_faulty: Maximum number of simultaneously Byzantine-faulty servers
  //   that must be tolerated (this is equivalent to the F value for the Concord
  //   cluster the servers are from).
  // - private_key: Private cryptographic key identifying and authenticating the
  //   ThinReplicaClient being constructed to the Thin Replica Servers.
  // - begin_servers: Iterator returning elements of type
  //   std::pair<std::string,
  //   std::unique_ptr<
  //      com::vmware::concord::thin_replica::ThinReplica::StubInterface>>
  //   representing each server available for this ThinReplicaClient to connect
  //   to; the string in each pair should be the public key used to identify and
  //   validate messages from this server, and the unique pointer to a
  //   com::vmware::concord::thin_replica::ThinReplica::StubInterface should
  //   point to a grpc server stub connecting to that Thin Replica Server. Note
  //   this ThinReplicaClient object will take ownership of the server stub
  //   unique pointers; they will be left pointing to null when this constructor
  //   returns if it is successful, and this object will handle destroying those
  //   stubs at the time of its own destruction. If this constructor fails, no
  //   guarantees are made about which StubInterface pointers will have been
  //   left intact and which will have been moved and set to null, but the
  //   constructor does guarantee that any pointer that is set to null will be
  //   destroyed. Furthermore, the order this iterator returns the servers will
  //   be used by this ThinReplicaClient as the preferred order to try
  //   connecting to the servers.
  // - end_servers: End iterator corresponding to begin_servers.
  template <class Iterator>
  ThinReplicaClient(std::string client_id,
                    std::shared_ptr<UpdateQueue> update_queue,
                    uint16_t max_faulty, const std::string& private_key,
                    Iterator begin_servers, Iterator end_servers)
      : logger_(
            log4cplus::Logger::getInstance("com.vmware.thin_replica_client")),
        client_id_(client_id),
        server_stubs_(),
        update_queue_(update_queue),
        max_faulty_(max_faulty),
        subscription_failure_condition_(),
        failure_condition_mutex_(),
        key_prefix_(),
        latest_verified_block_id_(0),
        subscription_thread_(),
        stop_subscription_thread_(false),
        subscription_data_stream_(),
        sub_data_context_(),
        current_data_source_(0),
        subscription_hash_streams_(),
        sub_hash_contexts_() {
    while (begin_servers != end_servers) {
      auto& server_info = *begin_servers;
      if (!(server_info.second)) {
        server_stubs_.clear();
        update_queue_.reset();
        throw std::invalid_argument(
            "Null ThinReplica::StubInterface provided to ThinReplicaClient "
            "constructor (only non-null StubInterface pointers are valid for "
            "this purpose).");
      }
      server_stubs_.push_back(std::move(server_info.second));
      server_info.second.reset();
      ++begin_servers;
    }

    if (server_stubs_.size() < (3 * (size_t)max_faulty_ + 1)) {
      size_t num_servers = server_stubs_.size();
      server_stubs_.clear();
      update_queue_.reset();
      throw std::invalid_argument(
          "Too few servers (" + std::to_string(num_servers) +
          ") given to ThinReplicaClient constructor to tolerate requested "
          "maximum faulty servers (" +
          std::to_string(max_faulty_) +
          "). The number of servers must be at least (3 * max_faulty + 1).");
    }

    // TODO (Alex): Enforce that, as far as this constructor can see (likely the
    //              virtual memory for the process it is running in), only one
    //              ThinReplicaClient is allowed to exist at a time.
  }

  // Destructor for ThinReplicaClient. Calling this destructor may block the
  // calling thread as necessary to stop and join worker thread(s) owned by the
  // ThinReplicaClient if the ThinReplicaClient has an active subscription.
  //
  // Any active subscription will be stopped in the process of destroying the
  // ThinReplicaClient, but destroying a ThinReplicaClient will not cause it to
  // request a final cancellation of that subscription with the Thin Replica
  // Server(s). An application can later resume that subscription by
  // constructing a new ThinReplicaClient and calling
  // ThinReplicaClient::Subscribe. If an application has no intention of later
  // resuming a subscription, it should call ThinReplicaClient::Unsubscribe
  // before destroying the ThinReplicaClient to inform the Thin Replica
  // Server(s) that this subscripiton is being cancelled.
  ~ThinReplicaClient();

  // Copying or moving of a ThinReplicaClient object is explicitly disallowed as
  // only 1 ThinReplicaClient at a time is allowed to exist by this library.
  ThinReplicaClient(const ThinReplicaClient& other) = delete;
  ThinReplicaClient(const ThinReplicaClient&& other) = delete;
  ThinReplicaClient& operator=(const ThinReplicaClient& other) = delete;
  ThinReplicaClient& operator=(const ThinReplicaClient&& other) = delete;

  // Register a condition variable for this ThinReplicaClient to notify in the
  // event it has an active subscription and becomes unable to receive or
  // validate a new update, given a shared pointer to that condition variable.
  // Note this is the primary programatic way to learn of failure(s) in the
  // ThinReplicaClient's worker thread(s) for managing active subscriptions,
  // which run asynchronously with all ThinReplicaClient functions after their
  // initial creation by ThinReplicaClient::Subscribe.
  //
  // Reasons the ThinReplicaClient may become unable to receive or validate
  // updates include disconnection from or unresponsiveness of Thin Replica
  // Server(s), disagreement among Thin Replica Server(s), or some combination
  // thereof.
  //
  // Note the ThinReplicaClient will notify all waiting threads (not just one
  // waiting thread) on the failure conditioin. Note ThinReplicaClient only
  // supports the registration of a single condition variable for subscription
  // failure; calling this function when a condition variable is already
  // registered will overwrite the existing condition (without notifying any
  // threads waiting on it). Also note that the ThinReplicaClient::Unsubscribe
  // and ThinReplicaClient's destructor will not cause the ThinReplicaClient to
  // notify a registered condition. Note calling
  // RegisterSubscriptionFailureCondition with a null failure_condition pointer
  // will effectively de-register any currently registered failure condition.
  void RegisterSubscriptionFailureCondition(
      std::shared_ptr<std::condition_variable> failure_condition);

  // Subscribe to updates from the Thin Replica Servers. key_prefix_bytes should
  // be a byte string to request the thin replica servers filter updates on
  // before sending them. If a value for block_id is given, the
  // ThinReplicaClient will begin the subscription at and including that Block
  // ID, otherwise, subscription will begin by attempting to read all current
  // state. The subscribe call should be expected to block the calling thread
  // until the subscription is successfully completed and any initial state is
  // read.
  //
  // The Thin Replica mechanism subscription procedure begins with fetching of
  // initial state, followed by the creation of a subscription stream through
  // which further updates are pushed as they are generated. The updates
  // contained in the initial state will all be synchronously pushed to the
  // UpdateQueue this ThinReplicaClient was constructed with before the
  // Subscribe function returns. Once the subscription stream is successfully
  // setup, each update received via the stream will also be asynchronously
  // pushed to that UpdateQueue (these asynchronous updates may begin before the
  // Subscribe function returns and may continue after it returns until the
  // Unsubscribe function is called and returned, Subscribe is called again to
  // create another subscription and returns, or the ThinReplicaClient object is
  // completely destoyed). It is expected that Thin Replica Client applications
  // will make a reasonable effort to call AcknowledgeBlockID for the most
  // recent Block ID they have received when they receive new update(s); please
  // see ThinReplicaClient::AcknowledgeBlockID's comments in this header file
  // for details.
  //
  // If this ThinReplicaClient already has an active subscription open when
  // Subscribe is called, that subscription may be ended when Subscribe is
  // called, and will always be ended before Subscribe returns if no error
  // occurs. If there are any updates leftover in update_queue when Subscribe is
  // called, the queue will be cleared. Subscribe will throw an exception if it
  // cannot successfully make the requested subscription.
  void Subscribe(const std::string& key_prefix_bytes);
  void Subscribe(const std::string& key_prefix_bytes, uint64_t block_id);

  // End any currently open subscription this ThinReplicaClient has; this will
  // stop any worker thread(s) this ThinReplicaClient has for maintaining this
  // subscription, close connection(s) to the Thin Replica Server(s) specific to
  // this subscription, and send message(s) informing the server(s) that this
  // subscription is being permanently cancelled (the cancellation may be
  // considered by the server(s) in pruning decisions). For this reason,
  // ThinReplicaClient::Unsubscribe should not be used to temporarilly stop
  // subscriptions that the application intends to resume soon. (Note destroying
  // the ThinReplicaClient object can be used to stop the subscriptions it has
  // without telling the server(s) that the subscription is being permanently
  // cancelled).
  //
  // Note this function will not automatically clear any updates still in the
  // update_queue. Unsubscribe may block the thread calling it as necessary to
  // stop and join worker thread(s) created by the ThinReplicaClient for the
  // subscription being terminated. Note Unsubscribe will effectively do nothing
  // if called for a ThinReplicaClient that has no active subscription.
  void Unsubscribe();

  // Acknowledge receipt of the update for a given Block ID to the Thin Replica
  // Servers. Thin Replica Client applications should make a reasonable effort
  // to call this function every time they receive, process, and persist changes
  // from new update(s).
  //
  // If any form of blockchain pruning is supported by the Concord cluster
  // containing the Thin Replica Servers to which this ThinReplicaClient
  // subscribes, AcknowledgeBlockID may inform those servers that this Thin
  // Replica Client has received and (as applicable) persisted the update
  // referenced by block_id; the Concord cluster may use this information in its
  // decisions about state to prune. For this reason, AcknowledgeBlockID must be
  // called strictly after the Thin Replica Client application has made
  // sufficient changes to its persisted state to recover from a crash or other
  // restart without needing to retrieve the acknowledged update from the Thin
  // Replica Server(s).
  //
  // Note that, as Block IDs are monotonically increasing with respect to the
  // order updates are sent and received, acknowledgement of one Block ID may be
  // taken to imply receipt and persistence of all preceding updates. For this
  // reason, an application that processes and persists updates out of order
  // should still acknowledge updates only in order; furthermore, in
  // applications where multiple sequential updates are received and processed
  // at once, it is sufficient to only acknowledge the latest update of a batch.
  void AcknowledgeBlockID(uint64_t block_id);
};

}  // namespace thin_replica_client

#endif  // THIN_REPLICA_CLIENT_HPP_
