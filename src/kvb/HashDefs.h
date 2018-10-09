// Copyright 2018 VMware, all rights reserved
//
// Hash functions for our Slice and KeyValuePair types.

#ifndef HASHDEFS_H
#define HASHDEFS_H

#include <stdlib.h>
#include "slice.h"
#include "BlockchainInterfaces.h"

// TODO(GG): do we want this hash function ? See also
// http://www.cse.yorku.ca/~oz/hash.html
inline size_t simpleHash(const char *data, const size_t len)
{
   size_t hash = 5381;
   size_t t;

   for (size_t i = 0; i < len; i++)
   {
      t = data[i];
      hash = ((hash << 5) + hash) + t;
   }

   return hash;
}

namespace std {
   template <> struct hash<Blockchain::Slice>
   {
      typedef Blockchain::Slice argument_type;
      typedef std::size_t result_type;

      result_type operator()(const Blockchain::Slice & t) const
      {
         return simpleHash(t.data(), t.size());
      }
   };

   template <> struct hash<Blockchain::KeyValuePair>
   {
      typedef Blockchain::KeyValuePair argument_type;
      typedef std::size_t result_type;

      result_type operator()(const Blockchain::KeyValuePair & t) const
      {
         size_t keyHash = simpleHash(t.first.data(), t.first.size());
         return keyHash;
      }
   };
}

#endif
