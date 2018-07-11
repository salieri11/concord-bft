// Copyright 2018 VMware, all rights reserved

/**
 * @file RocksDBClient.h
 *
 *  @brief Header file containing the RocksDBClientIterator and RocksDBClient
 *  class definitions.
 *
 *  Objects of RocksDBClientIterator contain an iterator for database along with
 *  a pointer to the client object.
 *
 *  Objects of RocksDBClient signify connections with RocksDB database. They
 *  contain variables for storing the database directory path, connection object
 *  and comparator.
 *
 */

#ifndef ROCKSDBCLIENT_H
#define ROCKSDBCLIENT_H

#ifdef USE_ROCKSDB

#include <log4cplus/loggingmacros.h>

#include "rocksdb/db.h"
#include "rocksdb/comparator.h"
#include "DatabaseInterface.h"

namespace Blockchain
{
   // Measurements

   class RocksDBClient;

   class RocksDBClientIterator : public IDBClient::IDBClientIterator
   {
      friend class RocksDBClient;

   public:
      RocksDBClientIterator(const RocksDBClient *_parentClient);
      virtual ~RocksDBClientIterator() {
	 delete m_iter;
      }

      // Inherited via IDBClientIterator
      virtual KeyValuePair first() override;
      virtual KeyValuePair seekAtLeast(Slice _searchKey) override;
      virtual KeyValuePair previous() override;
      virtual KeyValuePair next() override;
      virtual KeyValuePair getCurrent() override;
      virtual bool isEnd() override;
      virtual Status getStatus() override;

   private:
      log4cplus::Logger logger;

      rocksdb::Iterator* m_iter;

      // Reference to the RocksDBClient
      const RocksDBClient* m_parentClient;

      // Status tracker
      Status m_status;
   };

   class RocksDBClient : public IDBClient
   {
   public:
   RocksDBClient(std::string _dbPath, rocksdb::Comparator *_comparator)
      : logger(log4cplus::Logger::getInstance("com.athena.vmware.kvb")),
         m_dbPath(_dbPath), m_comparator(_comparator) {}

      virtual Status init() override;
      virtual Status get(Slice _key, OUT Slice & _outValue) const override;
      virtual IDBClientIterator* getIterator() const override;
      virtual Status freeIterator(IDBClientIterator *_iter) const override;
      virtual Status put(Slice _key, Slice _value) override;
      virtual Status close() override;
      virtual Status del(Slice _key) override;
      virtual Status freeValue(Slice &_value) override;
      virtual rocksdb::Iterator* getNewRocksDbIterator() const;
      virtual void monitor() const override;

   private:
      log4cplus::Logger logger;

      // Database path on directory (used for connection).
      std::string m_dbPath;

      // Database object (created on connection).
      rocksdb::DB *m_dbInstance;

      // Comparator object.
      rocksdb::Comparator *m_comparator;
   };

   rocksdb::Slice toRocksdbSlice(Slice _s);
   Slice fromRocksdbSlice(rocksdb::Slice _s);
}
#endif
#endif
