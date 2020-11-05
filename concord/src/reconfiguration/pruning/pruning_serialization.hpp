// Copyright 2019 VMware, all rights reserved
//
// Contains simple serialization routines used when signing and verifying
// pruning messages. Fields of messages are serialized in the order they appear
// in the message. Integer fields are serialized in network (big endian) byte
// order.

#ifndef CONCORD_PRUNING_PRUNING_SERIALIZATION_HPP
#define CONCORD_PRUNING_PRUNING_SERIALIZATION_HPP

#include <boost/endian/buffers.hpp>

#include <string>
#include <type_traits>
#include "concord.cmf.hpp"

namespace concord {
namespace reconfiguration {
namespace pruning {
namespace detail {

template <typename T>
std::string &operator<<(std::string &buf, T val) {
  static_assert(std::is_integral_v<T>);

  using buf_t = boost::endian::big_uint64_buf_at;
  const auto big_endian_val = buf_t{val};

  buf.append(big_endian_val.data(), sizeof(buf_t::value_type));
  return buf;
}

std::string &operator<<(std::string &,
                        const concord::messages::LatestPrunableBlock &);

}  // namespace detail
}  // namespace pruning
}  // namespace reconfiguration
}  // namespace concord

#endif  //  CONCORD_PRUNING_PRUNING_SERIALIZATION_HPP
