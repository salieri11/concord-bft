// Concord
//
// Copyright (c) 2018 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the
// "License").  You may not use this product except in compliance with the
// Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.

#include "BlockchainDBTypes.hpp"

#include <cstring>

namespace concord {
namespace consensus {

bool copyToAndAdvance(uint8_t *_buf, size_t *_offset, size_t _maxOffset,
                      uint8_t *_src, size_t _srcSize) {
  if (!_buf) {
    return false;
  }

  if (!_offset) {
    return false;
  }

  if (!_src) {
    return false;
  }

  if (*_offset >= _maxOffset && _srcSize > 0) {
    return false;
  }

  memcpy(_buf + *_offset, _src, _srcSize);
  *_offset += _srcSize;

  return true;
}

}  // namespace consensus
}  // namespace concord
