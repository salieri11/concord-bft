// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain interface definition

#ifndef BLOCKCHAIN_INTERFACES_H
#define BLOCKCHAIN_INTERFACES_H

// TODO: write about thread-safety

#include <string>
#include <iterator>
#include <unordered_set>
#include "slice.h"
#include "status.h"
#include "DatabaseInterface.h"

using std::string;
using std::pair;
using std::unordered_set;

namespace Blockchain {

   // PRIMITIVE TYPES

   // Id of a block
   typedef uint64_t BlockId;

   // Key and value
   typedef Slice Key;
   typedef Slice Value;
   typedef pair<Key, Value> KeyValuePair;
   // TODO(GG): unordered_map may be inefficient (as a small map), consider to
   // write a specialized map
   typedef unordered_set<KeyValuePair> SetOfKeyValuePairs;

   // forward declarations
   class ILocalKeyValueStorageReadOnlyIterator;
   class ILocalKeyValueStorageReadOnly;
   class IBlocksAppender;
   class ICommandsHandler;

   // REPLICA

   // configuration
   // TODO(GG): should be changed !!!!!!
   // TODO(BWF): as GG said, convert this from strings representing filenames to
   // structs representing the actual configuration
   struct ReplicaConsensusConfig
   {
      std::string byzConfig;
      std::string byzPrivateConfig;
   };

   // Represents a replica of the blockchain database
   class IReplica
   {
   public:
      virtual Status start() = 0;
      virtual Status stop()  = 0;
      // TODO(BWF): document what "wait" does
      virtual Status wait()  = 0;

      // status of the replica
      enum class RepStatus
      {
         UnknownError = -1,
         Idle = 0, // Idle == the internal threads are not running now
         Starting,
         Running,
         Stopping
      };

      // returns the current status of the replica
      virtual RepStatus getReplicaStatus() const = 0;

      // this callback is called by the library every time the replica status
      // is changed
      typedef void(*StatusNotifier)(RepStatus newStatus);
      /*
       * TODO(GG): Implement:
       *  virtual Status setStatusNotifier(StatusNotifier statusNotifier);
       */

      // Used to read from storage, only when a replica is Idle. Useful for
      // initialization and maintenance.
      virtual const ILocalKeyValueStorageReadOnly& getReadOnlyStorage() = 0;

      // Used to append blocks to storage, only when a replica is Idle. Useful
      // for initialization and maintenance.
      virtual Status addBlockToIdleReplica(
         const SetOfKeyValuePairs& updates) = 0;
   };

   // TODO(BWF): It would be nice to migrate createReplica/release to
   // constructor/destructor, to make use of RAII.

   // creates a new Replica object
   IReplica* createReplica(const ReplicaConsensusConfig& consensusConfig,
                           ICommandsHandler* cmdHandler,
                           IDBClient* db);

   // deletes a Replica object
   void release(IReplica* r);


   // CLIENT

   // configuration
   // TODO(GG): should be changed !!!!!!
   // TODO(BWF): as GG said, convert this from strings representing filenames to
   // structs representing the actual configuration
   struct ClientConsensusConfig
   {
      std::string byzConfig;
      std::string byzPrivateConfig;
   };

   // Represents a client of the blockchain database
   class IClient
   {
   public:
      virtual Status start() = 0;
      virtual Status stop() = 0;

      virtual bool isRunning() = 0;

      // Status of the client
      enum ClientStatus
      {
         UnknownError = -1,
         Idle = 0, // Idle == the internal threads are not running now
         Running,
         Stopping,
      };

      typedef void(*CommandCompletion)(uint64_t completionToken,
                                       Status returnedStatus,
                                       Slice outreply);
      virtual void invokeCommandAsynch(const Slice command,
                                       bool isReadOnly,
                                       uint64_t completionToken,
                                       CommandCompletion h) = 0;

      virtual Status invokeCommandSynch(const Slice command,
                                        bool isReadOnly,
                                        Slice& outReply) = 0;

      // release memory allocated by invokeCommandSynch
      virtual Status release(Slice& slice) = 0;
   };

   //TODO(BWF): Can create/release be migrated to constructor/destructor, to
   //make use of RAII?

   // creates a new Client object
   IClient* createClient(const ClientConsensusConfig& consensusConfig);

   // deletes a Client object
   void release(IClient* r);

   // COMMANDS HANDLER

   // Upcall interface from KVBlockchain to application using it as storage.
   class ICommandsHandler
   {
   public:

      virtual bool executeCommand(
         const Slice command,
         const ILocalKeyValueStorageReadOnly& roStorage,
         IBlocksAppender& blockAppender,
         const size_t maxReplySize,
         char* outReply,
         size_t& outReplySize) const = 0;

      virtual bool executeReadOnlyCommand(
         const Slice command,
         const ILocalKeyValueStorageReadOnly& roStorage,
         const size_t maxReplySize,
         char* outReply,
         size_t& outReplySize) const = 0;

      // TODO(GG): should be supported by the BA engine
      // virtual bool isValidCommand(const Slice command) = 0;
   };

   // STORAGE MODELS

   class ILocalKeyValueStorageReadOnly
   {
   public:
      // convenience where readVersion==latest, and block is not needed?
      virtual Status get(Key key, Value& outValue) const = 0;
      virtual Status get(BlockId readVersion,
                         Slice key,
                         Slice& outValue,
                         BlockId& outBlock) const = 0;

      virtual BlockId getLastBlock() const = 0;
      virtual Status getBlockData(BlockId blockId,
                                  SetOfKeyValuePairs& outBlockData) const = 0;
       // TODO(GG): explain motivation
      virtual Status mayHaveConflictBetween(Slice key,
                                            BlockId fromBlock,
                                            BlockId toBlock,
                                            bool& outRes) const = 0;

      virtual ILocalKeyValueStorageReadOnlyIterator*
         getSnapIterator() const = 0;
      virtual Status freeSnapIterator(
         ILocalKeyValueStorageReadOnlyIterator* iter) const = 0;

      virtual void monitor() const = 0;
   };

   /*
    * TODO: Iterator cleanup. The general interface should expose only
    * getCurrent() and isEnd(), and there should be two interfaces inheriting
    * from it:
    * 1. One which is "clean", without awareness to blocks
    * 2. Second which is aware of blocks and versions.
    */
   class ILocalKeyValueStorageReadOnlyIterator
   {
   public:
      virtual KeyValuePair first(BlockId readVersion,
                                 BlockId& actualVersion,
                                 bool& isEnd) = 0;
      virtual KeyValuePair first() = 0;

      // Assumes lexicographical ordering of the keys, seek the first element
      // k >= key
      virtual KeyValuePair seekAtLeast(BlockId readVersion,
                                       Key key,
                                       BlockId& actualVersion,
                                       bool& isEnd) = 0;
      virtual KeyValuePair seekAtLeast(Key key) = 0;

      // Proceed to next element and return it
      virtual KeyValuePair next(BlockId readVersion,
                                Key key,
                                BlockId&
                                actualVersion,
                                bool& isEnd) = 0;
      virtual KeyValuePair next() = 0;

      // Return current element without moving
      virtual KeyValuePair getCurrent() = 0;

      // Check if iterator is in the end.
      virtual bool isEnd() = 0;
   };

   // Blocks appender
   class IBlocksAppender
   {
   public:
      virtual Status addBlock(const SetOfKeyValuePairs& updates,
                              BlockId& outBlockId) = 0;
   };

   // init/free environment
   void initEnv();
   void freeEnv();

   // TBDs9gg0:
   // (1) Allow direct reading from the replicas
   // (2) State transfer in the key-val blockchain (and not in the consensus
   //     module)

}

#endif
