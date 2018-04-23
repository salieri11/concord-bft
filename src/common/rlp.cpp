// Copyright 2018 VMware, all rights reserved
//
// RLP Encoding, as defined: https://github.com/ethereum/wiki/wiki/RLP
//
// RLPBuilder is a utility for creating RLP encodings. To use it, create a
// builder, then add your elements to it in reverse order, and finally call
// "build()". Adding in reverse order allows the implementation to be simple,
// while preserving some amount of efficiency (we only have to append to a
// vector, instead of possibly adding at both the start and the end).
//
// For example, to create an RLP encoding of [nonce, from_address, to_address],
// write:
//
//   RLPBuilder rlpb;
//   rlpb.start_list();
//   rlpb.add(to_address);
//   rlpb.add(from_address);
//   rlpb.add(nonce);
//   std::vector<uint8_t> rlp = rlpb.build();
//
// The bytes of the encoding with be std::move'd out of build(), so you should
// ensure that the lifetime of the builder is at least as long as the lifetime
// of the encoded vector.

#include <algorithm>
#include <assert.h>
#include <memory>
#include <vector>
#include "rlp.hpp"

void com::vmware::athena::RLPBuilder::add_size(size_t size,
                                               uint8_t type_byte_short,
                                               uint8_t type_byte_long)
{
   if (size < 56) {
      buffer.push_back(type_byte_short + size);
   } else {
      // Long lists require a byte indicating the length of the length
      uint8_t field_length_count = 0;
      do {
         ++field_length_count;
         buffer.push_back(size & 0xff);
         size >>= 8;
      } while (size > 0);
      buffer.push_back(type_byte_long + field_length_count);
   }
}

void com::vmware::athena::RLPBuilder::add_string_size(size_t size) {
   add_size(size, 0x80, 0xb7);
}

void com::vmware::athena::RLPBuilder::add_list_size(size_t size) {
   add_size(size, 0xc0, 0xf7);
}

void com::vmware::athena::RLPBuilder::add(const std::vector<uint8_t> &vec)
{
   assert(!finished);
   std::reverse_copy(vec.begin(), vec.end(), std::back_inserter(buffer));
   add_string_size(vec.size());
}

void com::vmware::athena::RLPBuilder::add(const uint8_t *data, size_t size)
{
   assert(!finished);
   std::reverse_copy(data, data+size, std::back_inserter(buffer));
   add_string_size(size);
}

void com::vmware::athena::RLPBuilder::add(const evm_address &address)
{
   assert(!finished);
   add(address.bytes, sizeof(evm_address));
}

void com::vmware::athena::RLPBuilder::add(const evm_uint256be &uibe)
{
   assert(!finished);
   add(uibe.bytes, sizeof(evm_uint256be));
}

void com::vmware::athena::RLPBuilder::add(uint64_t number)
{
   assert(!finished);
   if (number <= 0x7f) {
      buffer.push_back((uint8_t)number);
   } else {
      uint8_t length = 0;
      do {
         ++length;
         buffer.push_back(number & 0xff);
         number >>= 8;
      } while (number > 0);
      add_string_size(length);
   }
}

void com::vmware::athena::RLPBuilder::start_list() {
   assert(!finished);
   assert(list_depth < MAX_LIST_DEPTH-1);
   list_start[++list_depth] = buffer.size();
}

void com::vmware::athena::RLPBuilder::end_list() {
   assert(!finished);
   assert(list_depth >= 0);
   add_list_size(buffer.size()-list_start[list_depth]);
   --list_depth;
}

// Closes any open lists, then reverses and returns the buffer.
std::vector<uint8_t>&& com::vmware::athena::RLPBuilder::build() {
   assert(!finished);
   while (list_depth >= 0) {
      end_list();
   }
   std::reverse(buffer.begin(), buffer.end());
   finished = true;
   return std::move(buffer);
}
