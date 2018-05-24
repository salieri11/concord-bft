// Copyright 2018 VMware, all rights reserved

/**
 * @file InMemoryDBClient.h
 *
 * @brief Header file containing the InMemoryDBClient and
 * InMemoryDBClientIterator class definitions.
 *
 * Objects of InMemoryDBClientIterator contain an iterator for the in memory
 * object store (implemented as a map) along with a pointer to the map.
 *
 * Objects of InMemoryDBClient are implementations of an in memory database
 * (implemented as a map).
 *
 * The map contains key value pairs of the type KeyValuePair. Keys and values
 * are of type Slice.
 *
 */

#ifndef INMEMBORYDBCLIENT_H
#define INMEMBORYDBCLIENT_H

#include <log4cplus/loggingmacros.h>

#include "DatabaseInterface.h"
#include <map>

namespace Blockchain
{
   class InMemoryDBClient;

   typedef std::map<Slice, Slice, IDBClient::KeyComparator> TKVStore;

   class InMemoryDBClientIterator: public IDBClient::IDBClientIterator
   {
      friend class InMemoryDBClient;

   public:
   InMemoryDBClientIterator(InMemoryDBClient *_parentClient) :
      logger(log4cplus::Logger::getInstance("com.vmware.athena.kvb")),
      m_parentClient(_parentClient) {}

      // Inherited via IDBClientIterator
      virtual KeyValuePair first() override;
      virtual KeyValuePair seekAtLeast(Slice _searchKey) override;
      virtual KeyValuePair next() override;
      virtual KeyValuePair getCurrent() override;
      virtual bool isEnd() override;
      virtual Status getStatus() override;

   private:
      log4cplus::Logger logger;

      // Pointer to the InMemoryDBClient.
      InMemoryDBClient *m_parentClient;

      // Current iterator inside the map.
      TKVStore::const_iterator m_current;
   };

   class InMemoryDBClient: public IDBClient
   {
   public:
      InMemoryDBClient(KeyComparator comp)
      {
         setComparator(comp);
      }

      virtual Status init() override;
      virtual Status get(Slice _key, OUT Slice &_outValue) const override;
      virtual IDBClientIterator *getIterator() const override;
      virtual Status freeIterator(IDBClientIterator *_iter) const override;
      virtual Status put(Slice _key, Slice _value) override;
      virtual Status close() override
      {
         return Status::OK();
      };
      virtual Status del(Slice _key) override;
      virtual Status freeValue(Slice &_value) override;
      virtual void monitor() const override {};

      TKVStore& getMap()
      {
         return map;
      }
      void setComparator(KeyComparator comp)
      {
         map = TKVStore(comp);
      }

   private:
      // map that stores the in memory database.
      TKVStore map;
   };
}

#endif
