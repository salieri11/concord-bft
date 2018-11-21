// Copyright 2018 VMware, all rights reserved
//
// RLP Encoding.
#ifndef RLP_HPP
#define RLP_HPP

#include "evm.h"
#include <memory>
#include <vector>

namespace com {
namespace vmware {
namespace concord {

class RLPBuilder {
public:
   void add(const std::vector<uint8_t> &vec);
   void add(const uint8_t *data, size_t size);
   void add(const std::string &str);
   void add(const evm_address &address);
   void add(const evm_uint256be &uibe);
   void add(uint64_t number);
   void start_list();
   void end_list();
   std::vector<uint8_t>&& build();

private:
   static const int MAX_LIST_DEPTH = 4;

   std::vector<uint8_t> buffer;
   size_t list_start[MAX_LIST_DEPTH];
   int list_depth = -1;
   bool finished = false;

   void add_size(size_t size, uint8_t type_byte_short, uint8_t type_byte_long);
   void add_string_size(size_t size);
   void add_list_size(size_t size);
};

class RLPParser {
public:
   RLPParser(std::vector<uint8_t> &rlp) :
      rlp_(rlp), offset(0) { };

   std::vector<uint8_t> next();
   bool at_end();

private:
   std::vector<uint8_t> &rlp_;
   size_t offset;

   std::vector<uint8_t> short_run(size_t length);
   std::vector<uint8_t> long_run(size_t length_length);
};

}
}
}

#endif
