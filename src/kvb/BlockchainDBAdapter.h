// Copyright 2018 VMware, all rights reserved
//
// Translation between BlockAppender/ILocalkeyValueStorage* to the underlying
// database.

#ifndef BLOCKCHAINDBADAPTER_H
#define BLOCKCHAINDBADAPTER_H

#include "Database/DatabaseInterface.h"
#include "BlockchainInterfaces.h"
#include "slice.h"

namespace Blockchain {

   // TODO(BWF): are there more types coming here?
   enum class _EDBKeyType
   {
      E_DB_KEY_TYPE_FIRST = 1,
      E_DB_KEY_TYPE_BLOCK = E_DB_KEY_TYPE_FIRST,
      E_DB_KEY_TYPE_KEY,
      E_DB_KEY_TYPE_LAST
   };

   // TODO(BWF): What's the point of this redefinition?
   typedef _EDBKeyType EDBKeyType;

   class BlockchainDBAdapter
   {
   public:
      BlockchainDBAdapter(IDBClient* _db) :
         logger(Logger::getInstance("com.vmware.athena.kvb")),
         m_db(_db) {}

      IDBClient* getDb() { return m_db; }

      Status addBlock(BlockId _blockId, Slice _blockRaw);
      Status updateKey(Key _key, BlockId _block, Value _value);
      Status getKeyByReadVersion(BlockId readVersion,
                                 Slice key,
                                 Slice &outValue,
                                 BlockId &outBlock) const;
      Status getBlockById(BlockId _blockId,
                          Slice &_blockRaw,
                          bool &_found) const;
      Status freeFetchedBlock(Slice &_block) const;

      IDBClient::IDBClientIterator* getIterator()
      {
         return m_db->getIterator();
      }

      Status freeIterator(IDBClient::IDBClientIterator* _iter) {
         return m_db->freeIterator(_iter);
      }

      Status first(IDBClient::IDBClientIterator* iter,
                   BlockId readVersion,
                   OUT BlockId &actualVersion,
                   OUT bool &isEnd,
                   OUT Slice &_key,
                   OUT Slice &_value);
      Status seekAtLeast(IDBClient::IDBClientIterator* iter,
                         Slice _searchKey,
                         BlockId _readVersion,
                         OUT BlockId &_actualVersion,
                         OUT Slice &_key,
                         OUT Slice &_value,
                         OUT bool &_isEnd);
      Status next(IDBClient::IDBClientIterator* iter,
                  BlockId _readVersion,
                  OUT Slice &_key,
                  OUT Slice &_value,
                  OUT BlockId &_actualVersion,
                  OUT bool &_isEnd);

      Status getCurrent(IDBClient::IDBClientIterator* iter,
                        OUT Slice &_key,
                        OUT Slice &_value);
      Status isEnd(IDBClient::IDBClientIterator* iter, OUT bool &_isEnd);

      Status delKey(Slice _key, BlockId _blockID);
      Status delBlock(BlockId _blockId);

      void monitor() const;

   private:
      log4cplus::Logger logger;
      IDBClient* m_db;
      KeyValuePair m_current;
      bool m_isEnd;
   };

   // TODO(BWF): Why not define a key class?
   Slice genDbKey(EDBKeyType _type, Slice _key, BlockId _blockId);
   Slice genBlockDbKey(BlockId _blockId);
   Slice genDataDbKey(Slice _key, BlockId _blockId);
   char extractTypeFromKey(Slice _key);
   BlockId extractBlockIdFromKey(Slice _key);
   Slice extractKeyFromKeyComposedWithBlockId(Slice _composedKey);
}

#endif
