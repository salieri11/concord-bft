// Copyright 2018 VMware, all rights reserved
//
// Translation between BlockAppender/ILocalkeyValueStorage* to the underlying
// database.

#ifndef BLOCKCHAINDBADAPTER_H
#define BLOCKCHAINDBADAPTER_H

#include <log4cplus/loggingmacros.h>
#include <cstdint>

#include "BlockchainInterfaces.h"
#include "DatabaseInterface.h"
#include "sliver.hpp"

namespace Blockchain {

// TODO(BWF): are there more types coming here?
enum class EDBKeyType : std::uint8_t {
  E_DB_KEY_TYPE_FIRST = 1,
  E_DB_KEY_TYPE_BLOCK = E_DB_KEY_TYPE_FIRST,
  E_DB_KEY_TYPE_KEY,
  E_DB_KEY_TYPE_LAST
};

class BlockchainDBAdapter {
 public:
  BlockchainDBAdapter(IDBClient *_db)
      : logger(log4cplus::Logger::getInstance("com.vmware.concord.kvb")),
        m_db(_db) {}

  IDBClient *getDb() { return m_db; }

  Status addBlock(BlockId _blockId, Sliver _blockRaw);
  Status updateKey(Key _key, BlockId _block, Value _value);
  Status updateMultiKey(const SetOfKeyValuePairs &_kvMap, BlockId _block);
  Status getKeyByReadVersion(BlockId readVersion, Sliver key, Sliver &outValue,
                             BlockId &outBlock) const;
  Status getBlockById(BlockId _blockId, Sliver &_blockRaw, bool &_found) const;

  IDBClient::IDBClientIterator *getIterator() { return m_db->getIterator(); }

  Status freeIterator(IDBClient::IDBClientIterator *_iter) {
    return m_db->freeIterator(_iter);
  }

  Status first(IDBClient::IDBClientIterator *iter, BlockId readVersion,
               OUT BlockId &actualVersion, OUT bool &isEnd, OUT Sliver &_key,
               OUT Sliver &_value);
  Status seekAtLeast(IDBClient::IDBClientIterator *iter, Sliver _searchKey,
                     BlockId _readVersion, OUT BlockId &_actualVersion,
                     OUT Sliver &_key, OUT Sliver &_value, OUT bool &_isEnd);
  Status next(IDBClient::IDBClientIterator *iter, BlockId _readVersion,
              OUT Sliver &_key, OUT Sliver &_value, OUT BlockId &_actualVersion,
              OUT bool &_isEnd);

  Status getCurrent(IDBClient::IDBClientIterator *iter, OUT Sliver &_key,
                    OUT Sliver &_value);
  Status isEnd(IDBClient::IDBClientIterator *iter, OUT bool &_isEnd);

  Status delKey(Sliver _key, BlockId _blockID);
  Status delBlock(BlockId _blockId);

  void monitor() const;

  BlockId getLatestBlock();
  BlockId getLastReachableBlock();

 private:
  log4cplus::Logger logger;
  IDBClient *m_db;
  KeyValuePair m_current;
  bool m_isEnd;
};

// TODO(BWF): Why not define a key class?
Sliver genDbKey(EDBKeyType _type, Sliver _key, BlockId _blockId);
Sliver genBlockDbKey(BlockId _blockId);
Sliver genDataDbKey(Sliver _key, BlockId _blockId);
char extractTypeFromKey(Sliver _key);
BlockId extractBlockIdFromKey(Sliver _key);
Sliver extractKeyFromKeyComposedWithBlockId(Sliver _composedKey);
}  // namespace Blockchain

#endif
