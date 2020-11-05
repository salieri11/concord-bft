// Copyright 2019 VMware, all rights reserved

#include "pruning_serialization.hpp"

namespace concord {
namespace reconfiguration {
namespace pruning {
namespace detail {

std::string &operator<<(std::string &buf,
                        const concord::messages::LatestPrunableBlock &block) {
  buf << block.replica << block.block_id;
  buf += std::string(block.signature.begin(), block.signature.end());

  return buf;
}

}  // namespace detail
}  // namespace pruning
}  // namespace reconfiguration
}  // namespace concord
