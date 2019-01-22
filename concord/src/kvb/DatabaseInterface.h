// Copyright 2018 VMware, all rights reserved

/**
 * @file DatabaseInterface.h
 *
 * @brief Header file containing the IDBClient and IDBClientIterator class
 * definitions.
 *
 */

#ifndef DATABASEINTERFACE_H
#define DATABASEINTERFACE_H

#include "status.hpp"
#include "sliver.hpp"

#define OUT

using std::pair;

namespace Blockchain
{
   typedef pair<Sliver, Sliver> KeyValuePair;

   class IDBClient
   {
   public:
      typedef bool (*KeyComparator)(const Sliver&, const Sliver&);

      virtual Status init(bool readOnly=false) = 0;
      virtual Status close() = 0;
      virtual Status get(Sliver _key, OUT Sliver &_outValue) const = 0;
      virtual Status put(Sliver _key, Sliver _value) = 0;
      virtual Status del(Sliver _key) = 0;
      virtual void monitor() const = 0;

      // TODO(GG): add multi-get , multi-put, and multi-del

      class IDBClientIterator
      {
      public:
         virtual KeyValuePair first() = 0;

         // Returns next keys if not found for this key
         virtual KeyValuePair seekAtLeast(Sliver _searchKey) = 0;
         virtual KeyValuePair previous() = 0;
         virtual KeyValuePair next() = 0;
         virtual KeyValuePair getCurrent() = 0;
         virtual bool isEnd() = 0;

         // Status of last operation
         virtual Status getStatus() = 0;

	 virtual ~IDBClientIterator() { }
      };

      virtual IDBClientIterator* getIterator() const = 0;
      virtual Status freeIterator(IDBClientIterator *_iter) const = 0;
   };
}

#endif
