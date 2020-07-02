// Copyright 2019 VMware, all rights reserved
//
// Wrapper used by concord::storage::ConcordCommandsHandler to store BFT
// metadata (sequence number).

#ifndef CONCORD_STORAGE_CONCORD_METADATA_STORAGE_HPP_
#define CONCORD_STORAGE_CONCORD_METADATA_STORAGE_HPP_

#include <exception>
#include <string>

#include "block_metadata.hpp"
#include "concord_storage.pb.h"
#include "db_interfaces.h"
#include "sliver.hpp"

namespace concord {
namespace storage {
using concord::kvbc::ILocalKeyValueStorageReadOnly;

class ConcordStorageException : public std::exception {
 public:
  explicit ConcordStorageException(const std::string &what) : msg_(what) {}

  const char *what() const noexcept override { return msg_.c_str(); }

 private:
  std::string msg_;
};

class ConcordBlockMetadata : public concord::kvbc::IBlockMetadata {
 public:
  ConcordBlockMetadata(const ILocalKeyValueStorageReadOnly &storage)
      : IBlockMetadata(storage) {
    logger_ = logging::getLogger("concord.storage.ConcordMetadataStorage");
  }
  uint64_t getLastBlockSequenceNum(
      const concordUtils::Sliver &key) const override;
  concordUtils::Sliver serialize(uint64_t sequence_num) const override;

 protected:
  const int64_t kBlockMetadataVersion = 1;
};

}  // namespace storage
}  // namespace concord

#endif  // CONCORD_STORAGE_CONCORD_METADATA_STORAGE_HPP_
