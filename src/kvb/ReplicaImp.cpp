// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain replica implementation.

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#ifndef _WIN32
#include <sys/param.h>
#include <unistd.h>
#endif

#include <log4cplus/loggingmacros.h>

#include "Comparators.h"

#include "libbyz.h"
#include "Threading.h"
#include "ThreadLocalStorage.h"

#include "ReplicaImp.h"
#include "HashDefs.h"
#include <inttypes.h>
#include <cstdlib>
#include "HexTools.h"
#include <chrono>

using log4cplus::Logger;

namespace Blockchain {

TlsIndex ReplicaImp::m_sThreadLocalDataIdx = 0;

struct blockEntry
{
   uint32_t keyOffset;
   uint32_t keySize;
   uint32_t valOffset;
   uint32_t valSize;
};
struct blockHeader
{
   uint32_t numberOfElements;
   blockEntry entries[1]; // n>0 entries
};


/**
 * Opens the database and creates the replica thread. Replica state moves to
 * Starting.
 */
Status ReplicaImp::start()
{
   if (m_currentRepStatus != RepStatus::Idle) {
      return Status::IllegalOperation("todo");
   }

   m_currentRepStatus = RepStatus::Starting;

   bool res = createThread(&m_thread, replicaInternalThread, this);

   if (!res) {
      // TODO(GG)
      return Status::GeneralError("Failed to create replica internal thread");
   }

   return Status::OK();
}

/**
 * Closes the database. Call `wait()` after this to wait for thread to stop.
 */
Status ReplicaImp::stop()
{
   m_bcDbAdapter->getDb()->close();

   return Status::OK();
}

/**
 * Wait for replica thread to stop.
 */
Status ReplicaImp::wait()
{
   if (m_currentRepStatus != RepStatus::Starting &&
       m_currentRepStatus != RepStatus::Running) {
      return Status::IllegalOperation("todo");
   }

   threadJoin(m_thread);

   return Status::OK();
}

ReplicaImp::RepStatus ReplicaImp::getReplicaStatus() const
{
   return m_currentRepStatus;
}



const ILocalKeyValueStorageReadOnly &ReplicaImp::getReadOnlyStorage()
{
   return m_InternalStorageWrapperForIdleMode;
}

Status ReplicaImp::addBlockToIdleReplica(const SetOfKeyValuePairs& updates)
{
   if (getReplicaStatus() != IReplica::RepStatus::Idle) {
      return Status::IllegalOperation("");
   }

   BlockId d;
   return addBlockInternal(updates, d);
}

Status ReplicaImp::get(Slice key, Slice& outValue) const
{
   // TODO(GG): check legality of operation (the method should be invocked from
   // the replica's internal thread)

   BlockId dummy;
   return getInternal(lastBlock, key, outValue, dummy);
}

Status ReplicaImp::get(BlockId readVersion,
                       Slice key,
                       Slice &outValue,
                       BlockId &outBlock) const
{
   // TODO(GG): check legality of operation (the method should be invocked from
   // the replica's internal thread)

   return getInternal(readVersion, key, outValue, outBlock);
}

BlockId ReplicaImp::getLastBlock() const
{
   return lastBlock;
}

Status ReplicaImp::getBlockData(BlockId blockId,
                                SetOfKeyValuePairs &outBlockData) const
{
   // TODO(GG): check legality of operation (the method should be invocked from
   // the replica's internal thread)

   Slice block = getBlockInternal(blockId);

   if (block.size() == 0) {
      return Status::NotFound("todo");
   }

   outBlockData = fetchBlockData(block);

   return Status::OK();
}

Status ReplicaImp::mayHaveConflictBetween(Slice key,
                                          BlockId fromBlock,
                                          BlockId toBlock,
                                          bool &outRes) const
{
   // TODO(GG): add assert or print warning if fromBlock==0 (all keys have a
   // conflict in block 0)

   // we conservatively assume that we have a conflict
   outRes = true;

   Slice dummy;
   BlockId block = 0;
   Status s = getInternal(toBlock, key, dummy, block);
   if (s.ok() && block < fromBlock) {
      outRes = false;
   }

   return s;
}

ILocalKeyValueStorageReadOnlyIterator* ReplicaImp::getSnapIterator() const
{
   return m_InternalStorageWrapperForIdleMode.getSnapIterator();
}

Status ReplicaImp::freeSnapIterator(
   ILocalKeyValueStorageReadOnlyIterator *iter) const
{
   return m_InternalStorageWrapperForIdleMode.freeSnapIterator(iter);
}

void ReplicaImp::monitor() const
{
   m_InternalStorageWrapperForIdleMode.monitor();
}

Status ReplicaImp::addBlock(const SetOfKeyValuePairs &updates,
                            BlockId &outBlockId)
{
   // TODO(GG): check legality of operation (the method should be invocked from
   // the replica's internal thread)

   // TODO(GG): what do we want to do with several identical keys in the same
   // block?

   return addBlockInternal(updates, outBlockId);
}


ReplicaImp::ReplicaImp( string byzConfig,
                        string byzPrivateConfig,
                        ICommandsHandler *cmdHandler,
                        BlockchainDBAdapter *dbAdapter,
                        UPDATE_CONNECTIVITY_FN fPeerConnectivityCallback) :
   m_byzConfig(byzConfig),
   m_byzPrivateConfig(byzPrivateConfig),
   m_cmdHandler(cmdHandler),
   logger(log4cplus::Logger::getInstance("com.vmware.athena.kvb")),
   m_running(false),
   m_InternalStorageWrapperForIdleMode(this),
   m_bcDbAdapter(dbAdapter),
   m_fPeerConnectivityCallback(fPeerConnectivityCallback)
{
   // TODO(GG): add synchronization (to handle concurrent executions)
   if (m_sThreadLocalDataIdx == 0) {
      int res = Utils::allocTlsIndex(&m_sThreadLocalDataIdx);
      // TODO(GG): add error handling
      assert(res == 0);
   }

   m_currentRepStatus = RepStatus::Idle;
   lastBlock = 0;
}

ReplicaImp::~ReplicaImp()
{
}

Status ReplicaImp::addBlockInternal(const SetOfKeyValuePairs& updates,
                                    BlockId& outBlockId)
{
   lastBlock++;

   BlockId block = lastBlock;
   SetOfKeyValuePairs updatesInNewBlock;

   if (getReplicaStatus() == RepStatus::Running) {
      // TODO(GG): sizeof(int) is not enough - byz engine should support "BlockId"
      int page = block;
      Byz_modify(1, &page);
   }

   LOG4CPLUS_DEBUG(logger,
                   "addBlockInternal: Got " << updates.size() << " updates");

   Slice blockRaw = createBlockFromUpdates(updates, updatesInNewBlock);
   Status s = m_bcDbAdapter->addBlock(block, blockRaw);
   if (!s.ok())
   {
      LOG4CPLUS_ERROR(logger, "Failed to add block");
      return s;
   }

   for (SetOfKeyValuePairs::iterator it = updatesInNewBlock.begin();
        it != updatesInNewBlock.end();
        ++it) {
      const KeyValuePair& kvPair = *it;

      LOG4CPLUS_DEBUG(logger, "Adding for " <<
                      sliceToString((Slice&)kvPair.first) << " the value " <<
                      sliceToString((Slice&)kvPair.second));

      Status s = m_bcDbAdapter->updateKey(kvPair.first, block, kvPair.second);
      if (!s.ok()) {
         LOG4CPLUS_ERROR(logger, "Failed to update key " <<
                         sliceToString((Slice&)kvPair.first) <<
                         ", block " << block);
         return s;
      }
   }

   outBlockId = block;

   return Status::OK();
}


Status ReplicaImp::getInternal(BlockId readVersion,
                               Slice key,
                               Slice &outValue,
                               BlockId &outBlock) const
{
   Status s = m_bcDbAdapter->getKeyByReadVersion(
      readVersion, key, outValue, outBlock);
   if (!s.ok()) {
      LOG4CPLUS_ERROR(logger, "Failed to get key " << sliceToString(key) <<
                      " by read version " << readVersion);
      return s;
   }

   return Status::OK();
}


void ReplicaImp::revertBlock(BlockId blockId)
{
   Slice blockRaw;
   bool found;
   Status s = m_bcDbAdapter->getBlockById(blockId, blockRaw, found);
   if (!s.ok()) {
      // the replica is corrupted!
      // TODO(GG): what do we want to do now?
      LOG4CPLUS_FATAL(logger, "replica may be corrupted");
      // TODO(GG): how do we want to handle this - restart replica?
      exit(1);
   }

   if (found && blockRaw.size() > 0) {
      char* b = new char[blockRaw.size()];
      memcpy(b, blockRaw.data(), blockRaw.size());


      const char* begin = b;
      blockHeader* header = (blockHeader*)begin;

      for (size_t i = 0; i < header->numberOfElements; i++) {
         const char* key = begin + header->entries[i].keyOffset;
         const int16_t keyLen = header->entries[i].keySize;

         const Slice keySlice(key, keyLen);

         Status s = m_bcDbAdapter->delKey(keySlice, blockId);
         if (!s.ok()) {
            // TODO(SG): What to do?
            LOG4CPLUS_FATAL(logger, "Failed to delete key");
            exit(1);
         }
      }
   }

   if (found) {
      m_bcDbAdapter->delBlock(blockId);
   }

}

void ReplicaImp::insertBlockInternal(BlockId blockId, Slice block)
{
   if (blockId > lastBlock) {
      lastBlock = blockId;
   }

   bool found = false;
   Slice blockRaw;
   Status s = 	m_bcDbAdapter->getBlockById(blockId, blockRaw, found);
   if (!s.ok()) {
      // the replica is corrupted!
      // TODO(GG): what do we want to do now?
      LOG4CPLUS_FATAL(logger, "replica may be corrupted\n\n");
      // TODO(GG): how do we want to handle this - restart replica?
      exit(1);
   }

   // if we already have a block with the same ID
   if (found && blockRaw.size() > 0) {
      if (blockRaw.size() != block.size() ||
          memcmp(blockRaw.data(), block.data(), block.size())) {
         // the replica is corrupted !
         // TODO(GG): what do we want to do now ?
         LOG4CPLUS_ERROR(logger, "found block " << blockId <<
                         ", size in db is " << blockRaw.size() <<
                         ", inserted is " << block.size() <<
                         ", data in db " << sliceToString(blockRaw).c_str() <<
                         ", data inserted " << sliceToString(block).c_str());
         LOG4CPLUS_ERROR(logger, "Block size test " <<
                         (blockRaw.size()!=block.size()) <<
                         ", block data test " <<
                         (memcmp(blockRaw.data(), block.data(), block.size())));

         // TODO(GG): If new block is empty, just revert block
         revertBlock(blockId);

         // TODO(GG): how do we want to handle this - restart replica?
         //exit(1);
         return;
      }
   }
   else {
      if (block.size() > 0) {
         char* b = new char[block.size()];
         memcpy(b, block.data(), block.size());

         const char* begin = b;
         blockHeader* header = (blockHeader*)begin;

         for (size_t i = 0; i < header->numberOfElements; i++)
         {
            const char* key = begin + header->entries[i].keyOffset;
            const uint32_t keyLen = header->entries[i].keySize;
            const char* val = begin + header->entries[i].valOffset;
            const uint32_t valLen = header->entries[i].valSize;

            const Slice keySlice(key, keyLen);
            const Slice valSlice(val, valLen);

            const KeyIDPair pk(keySlice, blockId);

            Status s = m_bcDbAdapter->updateKey(pk.key, pk.blockId, valSlice);
            if (!s.ok()) {
               // TODO(SG): What to do?
               LOG4CPLUS_FATAL(logger, "Failed to update key");
               exit(1);
            }
         }

         Slice newBlock(b, block.size());
         Status s = m_bcDbAdapter->addBlock(blockId, newBlock);
         if (!s.ok()) {
            // TODO(SG): What to do?
            printf("Failed to add block");
            exit(1);
         }
      }
      else {
         Status s = m_bcDbAdapter->addBlock(blockId, block);
         if (!s.ok()) {
            // TODO(SG): What to do?
            printf("Failed to add block");
            exit(1);
         }
      }
   }
}


Slice ReplicaImp::getBlockInternal(BlockId blockId) const
{
   assert(blockId <= lastBlock);
   Slice retVal;

   bool found;
   Status s = m_bcDbAdapter->getBlockById(blockId, retVal, found);
   if (!s.ok()) {
      // TODO(SG): To do something smarter
      LOG4CPLUS_ERROR(logger, "An error occurred in get block");
      return Slice();
   }

   if (!found) {
      return Slice();
   } else {
      return retVal;
   }
}

ReplicaImp::StorageWrapperForIdleMode::StorageWrapperForIdleMode(
   const ReplicaImp *r) : rep(r) {}

Status ReplicaImp::StorageWrapperForIdleMode::get(Slice key,
                                                  Slice &outValue) const
{
   if (rep->getReplicaStatus() != IReplica::RepStatus::Idle) {
      return Status::IllegalOperation("");
   }

   return rep->get(key, outValue);
}

Status ReplicaImp::StorageWrapperForIdleMode::get(BlockId readVersion,
                                                  Slice key,
                                                  Slice &outValue,
                                                  BlockId &outBlock) const
{
   if (rep->getReplicaStatus() != IReplica::RepStatus::Idle) {
      return Status::IllegalOperation("");
   }

   return rep->get(readVersion, key, outValue, outBlock);
}

BlockId ReplicaImp::StorageWrapperForIdleMode::getLastBlock() const
{
   return rep->getLastBlock();
}

Status ReplicaImp::StorageWrapperForIdleMode::getBlockData(
   BlockId blockId, SetOfKeyValuePairs &outBlockData) const
{
   if (rep->getReplicaStatus() != IReplica::RepStatus::Idle) {
      return Status::IllegalOperation("");
   }

   Slice block = rep->getBlockInternal(blockId);

   if (block.size() == 0) {
      return Status::NotFound("todo");
   }

   outBlockData = ReplicaImp::fetchBlockData(block);

   return Status::OK();
}

Status ReplicaImp::StorageWrapperForIdleMode::mayHaveConflictBetween(
   Slice key, BlockId fromBlock, BlockId toBlock, bool &outRes) const
{
   outRes = true;

   Slice dummy;
   BlockId block = 0;
   Status s = rep->getInternal(toBlock, key, dummy, block);

   if (s.ok() && block < fromBlock) {
      outRes = false;
   }

   return s;
}

ILocalKeyValueStorageReadOnlyIterator*
ReplicaImp::StorageWrapperForIdleMode::getSnapIterator() const
{
   return new StorageIterator(this->rep);
}

Status ReplicaImp::StorageWrapperForIdleMode::freeSnapIterator(
   ILocalKeyValueStorageReadOnlyIterator *iter) const
{
   if (iter == NULL) {
      return Status::InvalidArgument("Invalid iterator");
   }

   StorageIterator *storageIter = (StorageIterator*)iter;
   Status s = storageIter->freeInternalIterator();
   delete storageIter;
   return s;
}

void ReplicaImp::StorageWrapperForIdleMode::monitor() const
{
   this->rep->m_bcDbAdapter->monitor();
}

Slice ReplicaImp::createBlockFromUpdates(
   const SetOfKeyValuePairs &updates, SetOfKeyValuePairs &outUpdatesInNewBlock)
{
   // TODO(GG): overflow handling ....
   // TODO(SG): How? Right now - will put empty block instead

   assert(outUpdatesInNewBlock.size() == 0);

   uint32_t blockBodySize = 0;
   uint16_t numOfElemens = 0;
   for (auto it = updates.begin(); it != updates.end(); ++it) {
      const KeyValuePair &kvPair = KeyValuePair(it->first, it->second);
      numOfElemens++;
      blockBodySize += (kvPair.first.size() + kvPair.second.size());
   }

   const uint32_t headerSize =
      sizeof(blockHeader::numberOfElements) + sizeof(blockEntry)*(numOfElemens);
   const uint32_t blockSize = headerSize + blockBodySize;

   try {
      char *blockBuffer = new char[blockSize];
      memset(blockBuffer, 0, blockSize);

      blockHeader *header = (blockHeader*)blockBuffer;

      int16_t idx = 0;
      header->numberOfElements = numOfElemens;
      int32_t currentOffset = headerSize;
      for (auto it = updates.begin(); it != updates.end(); ++it) {
         const KeyValuePair &kvPair = *it;

         // key
         header->entries[idx].keyOffset = currentOffset;
         header->entries[idx].keySize = kvPair.first.size();
         memcpy(blockBuffer + currentOffset,
                kvPair.first.data(),
                kvPair.first.size());
         Slice newKey(blockBuffer + currentOffset, kvPair.first.size());

         currentOffset += kvPair.first.size();

         // value
         header->entries[idx].valOffset = currentOffset;
         header->entries[idx].valSize = kvPair.second.size();
         memcpy(blockBuffer + currentOffset,
                kvPair.second.data(),
                kvPair.second.size());
         Slice newVal(blockBuffer + currentOffset, kvPair.second.size());

         currentOffset += kvPair.second.size();

         // add to outUpdatesInNewBlock
         KeyValuePair newKVPair(newKey, newVal);
         outUpdatesInNewBlock.insert(newKVPair);

         idx++;
      }
      assert(idx == numOfElemens);
      assert((uint32_t) currentOffset == blockSize);

      return Slice(blockBuffer, blockSize);
   } catch (std::bad_alloc& ba) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "Failed to alloc size " << blockSize <<
                      ", error: " << ba.what());
      char *emptyBlockBuffer = new char[1];
      memset(emptyBlockBuffer, 0, 1);
      return Slice(emptyBlockBuffer, 1);
   }
}

SetOfKeyValuePairs ReplicaImp::fetchBlockData(Slice block)
{
   SetOfKeyValuePairs retVal;

   if (block.size() > 0) {
      const char *begin = block.data();
      blockHeader *header = (blockHeader*)begin;

      for (size_t i = 0; i < header->numberOfElements; i++) {
         const char *key = begin + header->entries[i].keyOffset;
         const uint32_t keyLen = header->entries[i].keySize;
         const char *val = begin + header->entries[i].valOffset;
         const uint32_t valLen = header->entries[i].valSize;

         Slice keySlice(key, keyLen);
         Slice valSlice(val, valLen);

         KeyValuePair kv(keySlice, valSlice);

         retVal.insert(kv);
      }
   }

   return retVal;
}


int ReplicaImp::get_block(int n, char **page)
{
   void* t = NULL;
   getTlsVal(m_sThreadLocalDataIdx, &t);
   ReplicaImp* r = (ReplicaImp*)t;

   BlockId bId = n;
   size_t size;

   bool found = false;
   Slice blockRaw;
   Status s = r->getBcDbAdapter()->getBlockById(bId, blockRaw, found);
   if (!s.ok()) {
      LOG4CPLUS_ERROR(Logger::getInstance("com.vmware.athena.kvb"),
                      "error getting block by id " << bId);
      return -1;
   }

   if (found) {
      size = blockRaw.size();
      *page = new char[size];
      memcpy(*page, blockRaw.data(), size);
   } else {
      *page = nullptr;
      size = 0;
   }

   // Free blockRaw
   if (blockRaw.size() > 0) {
      // Will free underlying data only if it is fetched from RocksDB database
      r->getBcDbAdapter()->freeFetchedBlock(blockRaw);
      blockRaw.clear();
   }

   LOG4CPLUS_DEBUG(Logger::getInstance("com.vmware.athena.kvb"),
                   "n " << n << " size " << size << " page " <<
                   sliceToString(Slice(*page, size)));
   return size;
}

void ReplicaImp::put_blocks(int count, int *sizes, int *indices, char **pages)
{
   void *t = NULL;
   getTlsVal(m_sThreadLocalDataIdx, &t);
   ReplicaImp *r = (ReplicaImp*)t;

   for (int i = 0; i < count; i++) {
      BlockId blockId = indices[i];
      size_t blockSize = sizes[i];
      char *blockPtr = pages[i];

      Slice b(blockPtr, blockSize);

      r->insertBlockInternal(blockId, b);
   }
}

bool ReplicaImp::check_nond(Byz_buffer *b)
{
   return true;
}

int ReplicaImp::exec_command(Byz_req *inb,
                             Byz_rep *outb,
                             Byz_buffer *non_det,
                             int client,
                             bool ro)
{
   void *t = NULL;
   getTlsVal(m_sThreadLocalDataIdx, &t);
   ReplicaImp *r = (ReplicaImp*)t;

   // TODO(GG): verify command .....

   Slice cmdContent(inb->contents, inb->size);

   if (ro) {
      size_t replySize = 0;
      // TODO(GG): ret vals
      r->m_cmdHandler->executeReadOnlyCommand(
         cmdContent, *r, outb->size, outb->contents, replySize);
      outb->size = replySize;
   } else {
      size_t replySize = 0;
      // TODO(GG): ret vals
      r->m_cmdHandler->executeCommand(
         cmdContent, *r, *r, outb->size, outb->contents, replySize);
      outb->size = replySize;
   }

   return 0;
}


#if defined(_WIN32)
DWORD WINAPI ReplicaImp::replicaInternalThread(LPVOID param)
#else
   void* ReplicaImp::replicaInternalThread(void *param)
#endif
{
   ReplicaImp *r = (ReplicaImp*)param;

#if defined(_WIN32)
   // Suspecting this assert is not correct in linux, and key may be 0
   assert(m_sThreadLocalDataIdx != 0);
#endif
   setTlsVal(m_sThreadLocalDataIdx, r);

   // TODO(GG): Explain. In the future, we will probably need to map several
   // blocks to the same object/page
   assert(10 * 1e6 < INT_MAX);

   Logger logger(Logger::getInstance("com.vmware.athena.kvb"));
   LOG4CPLUS_DEBUG(logger, "initializing byz");
   // TODO(GG): clean & understand ....
   int used_mem = Byz_init_replica(r->m_byzConfig.c_str(),
                                   r->m_byzPrivateConfig.c_str(),
                                   10 * 1e6,
                                   exec_command,
                                   0,
                                   0,
                                   check_nond,
                                   get_block,
                                   put_blocks,
                                   0,
                                   0,
                                   0,
                                   r->m_fPeerConnectivityCallback);

   if (used_mem < 0) {
      LOG4CPLUS_ERROR(logger, "Byz_init_replica failed");
      return 0;
   }

   r->m_currentRepStatus = RepStatus::Running;

   // TODO(GG): add support for "stop" in the BFT engine
   Byz_replica_run();

   return 0;
}

IReplica* createReplica(const ReplicaConsensusConfig& consensusConfig,
                        ICommandsHandler* cmdHandler,
                        IDBClient* db,
                        UPDATE_CONNECTIVITY_FN fPeerConnectivityCallback)
{
   LOG4CPLUS_DEBUG(Logger::getInstance("com.vmware.athena.kvb"),
                   "Creating replica");
   BlockchainDBAdapter *dbAdapter = new BlockchainDBAdapter(db);

   ReplicaImp *r = new ReplicaImp(consensusConfig.byzConfig,
                                  consensusConfig.byzPrivateConfig,
                                  cmdHandler,
                                  dbAdapter,
                                  fPeerConnectivityCallback);

   //Initialization of the database object is done here so that we can
   //read the latest block number and take a decision regarding
   //genesis block creation.
   Status s = db->init();

   if (!s.ok()) {
      LOG4CPLUS_FATAL(Logger::getInstance("com.vmware.athena.kvb"),
                      "Failure in Database Initialization");
      throw ReplicaInitException("Failure in Database Initialization");
   }

   //Get the latest block count from persistence.
   //Will always be 0 for either InMemory mode or for persistence mode
   //when no database files exist.
   r->lastBlock = dbAdapter->getLatestBlock();
   return r;
}

void release(IReplica *r)
{
   ReplicaImp *rep = (ReplicaImp*)r;
   delete rep;
}


ReplicaImp::StorageIterator::StorageIterator(const ReplicaImp *r) :
   logger(log4cplus::Logger::getInstance("com.vmware.athena.kvb")),
   rep(r)
{
   m_iter = r->getBcDbAdapter()->getIterator();
   m_currentBlock = r->getLastBlock();
}

KeyValuePair ReplicaImp::StorageIterator::first(BlockId readVersion,
                                                BlockId &actualVersion,
                                                bool &isEnd)
{
   Key key;
   Value value;
   Status s = rep->getBcDbAdapter()->first(
      m_iter, readVersion, actualVersion, isEnd, key, value);

   if (s.IsNotFound()) {
      isEnd = true;
      m_current = KeyValuePair();
      return m_current;
   }

   if (!s.ok()) {
      LOG4CPLUS_ERROR(logger, "Failed to get first");
      exit(1);
   }

   m_isEnd = isEnd;
   m_current = KeyValuePair(key, value);

   return m_current;
}

KeyValuePair ReplicaImp::StorageIterator::seekAtLeast(BlockId readVersion,
                                                      Key key,
                                                      BlockId &actualVersion,
                                                      bool &isEnd)
{
   Key actualKey;
   Value value;
   Status s = rep->getBcDbAdapter()->seekAtLeast(
      m_iter, key, readVersion, actualVersion, actualKey, value, isEnd);

   if (s.IsNotFound()) {
      isEnd = true;
      m_current = KeyValuePair();
      return m_current;
   }

   if (!s.ok()) {
      LOG4CPLUS_FATAL(logger, "Failed to seek at least");
      exit(1);
   }

   m_isEnd = isEnd;
   m_current = KeyValuePair(actualKey, value);
   return m_current;
}

/**
 * TODO(SG): There is a question mark regarding on these APIs. Suppose I have
 * (k0,2), (k1,7), (k2,4) and I request next(k0,5). Do we return end() (because
 * k1 cannot be returned), or do we return k2?  I implemented the second choice,
 * as it makes better sense. The world in Block 5 did not include k1, that's
 * perfectly OK.
 */
// Note: key,readVersion must exist in map already
KeyValuePair ReplicaImp::StorageIterator::next(BlockId readVersion,
                                               Key key,
                                               BlockId &actualVersion,
                                               bool &isEnd)
{
   Key nextKey;
   Value nextValue;
   Status s = rep->getBcDbAdapter()->next(
      m_iter, readVersion, nextKey, nextValue, actualVersion, isEnd);

   if (s.IsNotFound()) {
      isEnd = true;
      m_current = KeyValuePair();
      return m_current;
   }

   if (!s.ok()) {
      LOG4CPLUS_FATAL(logger, "Failed to get next");
      exit(1);
   }

   m_isEnd = isEnd;
   m_current = KeyValuePair(nextKey, nextValue);
   return m_current;
}

KeyValuePair ReplicaImp::StorageIterator::getCurrent()
{
   Key key;
   Value value;
   Status s = rep->getBcDbAdapter()->getCurrent(m_iter, key, value);

   if (!s.ok()) {
      LOG4CPLUS_FATAL(logger, "Failed to get current");
      exit(1);
   }

   m_current = KeyValuePair(key, value);
   return m_current;
}

bool ReplicaImp::StorageIterator::isEnd()
{
   bool isEnd;
   Status s = rep->getBcDbAdapter()->isEnd(m_iter, isEnd);

   if (!s.ok()) {
      LOG4CPLUS_FATAL(logger, "Failed to get current");
      exit(1);
   }

   return isEnd;

}

Status ReplicaImp::StorageIterator::freeInternalIterator()
{
   return rep->getBcDbAdapter()->freeIterator(m_iter);
}

}
