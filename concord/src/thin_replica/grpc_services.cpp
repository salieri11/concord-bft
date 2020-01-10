// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <atomic>
#include <boost/lockfree/spsc_queue.hpp>
#include <chrono>
#include <sstream>

#include "hash_defs.h"
#include "kv_types.hpp"
#include "storage/kvb_app_filter.h"

using namespace std::chrono_literals;

using boost::lockfree::spsc_queue;
using grpc::ServerContext;
using grpc::ServerWriter;

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using concord::storage::KvbAppFilter;
using concord::storage::KvbUpdate;
using concordUtils::BlockId;
using concordUtils::Key;
using concordUtils::KeyValuePair;
using concordUtils::SetOfKeyValuePairs;
using concordUtils::Status;
using concordUtils::Value;

namespace concord {
namespace thin_replica {

class StreamClosed : public std::exception {
 public:
  explicit StreamClosed(){};
  virtual const char* what() const noexcept override { return "Stream closed"; }
};

std::string GetClientId(ServerContext* context) {
  auto metadata = context->client_metadata();
  auto client_id = metadata.find("client_id");
  if (client_id != metadata.end()) {
    return std::string(client_id->second.data());
  }
  throw std::invalid_argument("client_id metadata is missing");
}

// Send* prepares the response object and puts it on the stream
void SendData(ServerWriter<Data>* stream,
              concord::thin_replica::SubUpdate& update) {
  Data data;
  data.set_block_id(update.first);

  for (const auto& [key, value] : update.second) {
    KVPair* kvp_out = data.add_data();
    kvp_out->set_key(key.data(), key.length());
    kvp_out->set_value(value.data(), value.length());
  }
  if (!stream->Write(data)) {
    throw StreamClosed();
  }
}

void SendHash(ServerWriter<Hash>* stream, BlockId block_id,
              size_t update_hash) {
  com::vmware::concord::thin_replica::Hash hash;
  hash.set_block_id(block_id);
  hash.set_hash(&update_hash, sizeof update_hash);
  if (!stream->Write(hash)) {
    throw StreamClosed();
  }
}

void ReadFromKvbAndSendData(
    log4cplus::Logger logger,
    ServerWriter<com::vmware::concord::thin_replica::Data>* stream,
    const concord::storage::blockchain::ILocalKeyValueStorageReadOnly* kvb,
    BlockId start, BlockId end, std::shared_ptr<KvbAppFilter> kvb_filter) {
  spsc_queue<KvbUpdate> queue{10};
  std::atomic_bool close_stream = false;

  auto kvb_reader =
      std::async(std::launch::async, &KvbAppFilter::ReadBlockRange, kvb_filter,
                 start, end, std::ref(queue), std::ref(close_stream));

  KvbUpdate kvb_update;
  while (kvb_reader.wait_for(0s) != std::future_status::ready ||
         !queue.empty()) {
    while (queue.pop(kvb_update)) {
      try {
        SendData(stream, kvb_update);
      } catch (StreamClosed& error) {
        LOG4CPLUS_WARN(logger,
                       "Data stream closed at block " << kvb_update.first);

        // Stop kvb_reader and empty queue
        close_stream = true;
        while (!queue.empty()) {
          queue.pop(kvb_update);
        }
        throw;
      }
    }
  }
  assert(queue.empty());

  // Throws exception if something went wrong
  kvb_reader.get();
}

void ReadFromKvbAndSendHashes(
    log4cplus::Logger logger,
    ServerWriter<com::vmware::concord::thin_replica::Hash>* stream,
    const concord::storage::blockchain::ILocalKeyValueStorageReadOnly* kvb,
    BlockId start, BlockId end, std::shared_ptr<KvbAppFilter> kvb_filter) {
  BlockId block_id = start;

  for (; block_id <= end; ++block_id) {
    size_t hash = kvb_filter->ReadBlockHash(block_id);
    SendHash(stream, block_id, hash);
  }
}

// Read from KVB and send to the given stream depending on the data type
template <typename T>
inline void ReadAndSend(
    log4cplus::Logger logger, ServerWriter<T>* stream,
    const concord::storage::blockchain::ILocalKeyValueStorageReadOnly* kvb,
    BlockId start, BlockId end, std::shared_ptr<KvbAppFilter> kvb_filter) {
  static_assert(std::is_same<T, Data>() || std::is_same<T, Hash>(),
                "We expect either a Data or Hash type");
  if constexpr (std::is_same<T, Data>()) {
    ReadFromKvbAndSendData(logger, stream, kvb, start, end, kvb_filter);
  } else if constexpr (std::is_same<T, Hash>()) {
    ReadFromKvbAndSendHashes(logger, stream, kvb, start, end, kvb_filter);
  }
}

// Read from KVB until we are in sync with the live updates. This function
// returns when the next update can be taken from the given live updates.
template <typename T>
void ThinReplicaImpl::SyncAndSend(BlockId start,
                                  std::shared_ptr<SubUpdateBuffer> live_updates,
                                  ServerWriter<T>* stream,
                                  std::shared_ptr<KvbAppFilter> kvb_filter) {
  SubUpdate update;
  BlockId end = rostorage_->getLastBlock();
  assert(start <= end);

  // Let's not wait for a live update yet due to there might be lots of history
  // we have to catch up with first
  LOG4CPLUS_INFO(logger_,
                 "Sync reading from KVB [" << start << ", " << end << "]");
  ReadAndSend<T>(logger_, stream, rostorage_, start, end, kvb_filter);

  // Let's wait until we have at least one live update
  // TODO: Notify instead of busy wait?
  while (live_updates->Empty())
    ;

  // We are in sync already
  if (!live_updates->Full() && live_updates->OldestBlockId() == (end + 1)) {
    return;
  }

  // Gap:
  // The ring buffer (live updates) could have filled up and we are overwriting
  // old updates already. Or the first live update is not the follow-up to the
  // last read block from KVB. In either case, we need to fill the gap. Let's
  // read from KVB starting at end + 1 up to updates that are part of the live
  // updates already. Thereby, we create an overlap between what we read from
  // KVB and what is currently in the live updates.
  if (live_updates->Full() || live_updates->OldestBlockId() > (end + 1)) {
    start = end + 1;
    end = live_updates->NewestBlockId();

    LOG4CPLUS_INFO(logger_,
                   "Sync filling gap [" << start << ", " << end << "]");
    ReadAndSend<T>(logger_, stream, rostorage_, start, end, kvb_filter);
  }

  // Overlap:
  // If we read updates from KVB that were added to the live updates already
  // then we just need to drop the overlap and return
  assert(live_updates->OldestBlockId() <= end);
  do {
    update = live_updates->Pop();
    LOG4CPLUS_INFO(logger_, "Sync dropping " << update.first);
  } while (update.first < end);
}

grpc::Status ThinReplicaImpl::ReadState(
    ServerContext* context,
    const com::vmware::concord::thin_replica::ReadStateRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Data>* stream) {
  std::shared_ptr<KvbAppFilter> kvb_filter;

  try {
    kvb_filter = std::make_shared<KvbAppFilter>(rostorage_, KvbAppFilter::kDaml,
                                                GetClientId(context),
                                                request->key_prefix());
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    std::stringstream msg;
    msg << "Failed to set up filter: " << error.what();
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  LOG4CPLUS_DEBUG(logger_, "ReadState");

  // TODO: Determine oldest block available (pruning)
  BlockId start = 1;
  BlockId end = rostorage_->getLastBlock();

  try {
    ReadFromKvbAndSendData(logger_, stream, rostorage_, start, end, kvb_filter);
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, "Failed to read and send state: " << error.what());
    return grpc::Status(grpc::StatusCode::UNKNOWN,
                        "Failed to read and send state");
  }

  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::ReadStateHash(
    ServerContext* context,
    const com::vmware::concord::thin_replica::ReadStateHashRequest* request,
    com::vmware::concord::thin_replica::Hash* hash) {
  concord::storage::KvbStateHash kvb_hash;
  std::shared_ptr<KvbAppFilter> kvb_filter;

  try {
    kvb_filter = std::make_shared<KvbAppFilter>(rostorage_, KvbAppFilter::kDaml,
                                                GetClientId(context),
                                                request->key_prefix());
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    std::stringstream msg;
    msg << "Failed to set up filter: " << error.what();
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  LOG4CPLUS_DEBUG(logger_, "ReadStateHash");

  // TODO: Determine oldest block available (pruning)
  BlockId block_id_start = 1;
  BlockId block_id_end = request->block_id();

  try {
    kvb_hash = kvb_filter->ReadBlockRangeHash(block_id_start, block_id_end);
  } catch (concord::storage::KvbReadError& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    std::stringstream msg;
    msg << "Reading StateHash for block " << block_id_end << " failed";
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  hash->set_block_id(block_id_end);
  hash->set_hash(&kvb_hash, sizeof kvb_hash);

  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::AckUpdate(
    ServerContext* context,
    const com::vmware::concord::thin_replica::BlockId* block_id,
    google::protobuf::Empty* empty) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "AckUpdate");
}

grpc::Status ThinReplicaImpl::SubscribeToUpdates(
    ServerContext* context,
    const com::vmware::concord::thin_replica::SubscriptionRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Data>* stream) {
  std::pair<BlockId, SetOfKeyValuePairs> update;
  SubUpdate live_update;
  std::shared_ptr<KvbAppFilter> kvb_filter;

  try {
    kvb_filter = std::make_shared<KvbAppFilter>(rostorage_, KvbAppFilter::kDaml,
                                                GetClientId(context),
                                                request->key_prefix());
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    std::stringstream msg;
    msg << "Failed to set up filter: " << error.what();
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  // Subscribe before we start reading from KVB
  auto live_updates = std::make_shared<SubUpdateBuffer>(100);
  subscriber_list_.AddBuffer(live_updates);

  if (request->block_id() > rostorage_->getLastBlock()) {
    subscriber_list_.RemoveBuffer(live_updates);
    live_updates->RemoveAllUpdates();
    std::stringstream msg;
    msg << "Block " << request->block_id() << " doesn't exist yet";
    return grpc::Status(grpc::StatusCode::FAILED_PRECONDITION, msg.str());
  }

  try {
    SyncAndSend<Data>(request->block_id(), live_updates, stream, kvb_filter);
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    subscriber_list_.RemoveBuffer(live_updates);
    live_updates->RemoveAllUpdates();

    std::stringstream msg;
    msg << "Couldn't transition from block id " << request->block_id()
        << " to new blocks";
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  // Read, filter, and send live updates
  while (true) {
    update = live_updates->Pop();
    SubUpdate filtered_update = kvb_filter->FilterUpdate(update);
    try {
      SendData(stream, filtered_update);
    } catch (std::exception& error) {
      LOG4CPLUS_INFO(logger_,
                     "Data subscription stream closed: " << error.what());
      break;
    }
  }

  subscriber_list_.RemoveBuffer(live_updates);
  live_updates->RemoveAllUpdates();
  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::SubscribeToUpdateHashes(
    ServerContext* context,
    const com::vmware::concord::thin_replica::SubscriptionRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Hash>* stream) {
  std::pair<BlockId, SetOfKeyValuePairs> update;
  std::shared_ptr<KvbAppFilter> kvb_filter;

  try {
    kvb_filter = std::make_shared<KvbAppFilter>(rostorage_, KvbAppFilter::kDaml,
                                                GetClientId(context),
                                                request->key_prefix());
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    std::stringstream msg;
    msg << "Failed to set up filter: " << error.what();
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  auto live_updates = std::make_shared<SubUpdateBuffer>(100);
  subscriber_list_.AddBuffer(live_updates);

  if (request->block_id() > rostorage_->getLastBlock()) {
    subscriber_list_.RemoveBuffer(live_updates);
    live_updates->RemoveAllUpdates();
    std::stringstream msg;
    msg << "Block " << request->block_id() << " doesn't exist yet";
    return grpc::Status(grpc::StatusCode::FAILED_PRECONDITION, msg.str());
  }

  try {
    SyncAndSend<Hash>(request->block_id(), live_updates, stream, kvb_filter);
  } catch (std::exception& error) {
    LOG4CPLUS_ERROR(logger_, error.what());
    subscriber_list_.RemoveBuffer(live_updates);
    live_updates->RemoveAllUpdates();

    std::stringstream msg;
    msg << "Couldn't transition from block id " << request->block_id()
        << " to new blocks";
    return grpc::Status(grpc::StatusCode::UNKNOWN, msg.str());
  }

  // Read, filter, compute hash, and send live updates
  while (true) {
    update = live_updates->Pop();
    SubUpdate filtered_update = kvb_filter->FilterUpdate(update);
    try {
      SendHash(stream, update.first, kvb_filter->HashUpdate(filtered_update));
    } catch (std::exception& error) {
      LOG4CPLUS_INFO(logger_,
                     "Hash subscription stream closed: " << error.what());
      break;
    }
  }

  subscriber_list_.RemoveBuffer(live_updates);
  live_updates->RemoveAllUpdates();
  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::Unsubscribe(
    ServerContext* context, const google::protobuf::Empty* request,
    google::protobuf::Empty* response) {
  // Note: In order to unsubscribe in a separate gRPC call, we need to connect
  // the sub buffer with the thin replica client id.
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "Unsubscribe");
}

}  // namespace thin_replica
}  // namespace concord
