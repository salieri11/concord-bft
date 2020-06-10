// Copyright 2018 VMware, all rights reserved

#ifndef CONCORD_HLF_KVB_STORAGE_H_
#define CONCORD_HLF_KVB_STORAGE_H_

#include "Logger.hpp"
#include "common/concord_types.hpp"
#include "concord.pb.h"
#include "concord_storage.pb.h"
#include "db_interfaces.h"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"
#include "kv_types.hpp"
#include "sliver.hpp"
#include "status.hpp"

#include <string>
#include <vector>

namespace concord {
namespace hlf {

class HlfKvbStorage {
 private:
  const concord::kvbc::ILocalKeyValueStorageReadOnly& ro_storage_;
  concord::kvbc::IBlocksAppender* ptr_block_appender_;
  static concord::kvbc::SetOfKeyValuePairs updates_;
  std::vector<com::vmware::concord::hlf::storage::HlfTransaction>
      pending_hlf_transactions_;
  logging::Logger logger_;

 public:
  // This is used for adding block id to the
  // result of GetHlfState
  const std::string kStateSeparator = "%%";

  // read-only mode
  HlfKvbStorage(const concord::kvbc::ILocalKeyValueStorageReadOnly& ro_storage);

  // read-write mode
  HlfKvbStorage(const concord::kvbc::ILocalKeyValueStorageReadOnly& ro_storage,
                concord::kvbc::IBlocksAppender* block_appender);

  ~HlfKvbStorage();

  bool is_read_only();
  uint64_t next_block_number();
  const concord::kvbc::ILocalKeyValueStorageReadOnly& getReadOnlyStorage();

  uint64_t current_block_number();

  void reset();

  concordUtils::Status get(const concordUtils::Sliver& key,
                           concordUtils::Sliver& out);

  concordUtils::Status get(const kvbc::BlockId read_version,
                           const concordUtils::Sliver& key,
                           concordUtils::Sliver& value,
                           kvbc::BlockId& out_block);

  concordUtils::Sliver KvbKey(uint8_t type, const std::string& key) const;

  concordUtils::Sliver KvbKey(uint8_t type, const uint8_t* bytes,
                              size_t length) const;

  concordUtils::Sliver HlfStateKey(const std::string& key) const;

  concordUtils::Sliver HlfTransactionKey(const evm_uint256be& hash) const;

  concordUtils::Sliver HlfBlockKey(const evm_uint256be& hash) const;

  void put(const concordUtils::Sliver& key, const concordUtils::Sliver& value);

  std::string GetHlfState(const std::string& key);

  std::string GetHlfState(const std::string& key, uint64_t& block_number);

  concordUtils::Status SetHlfState(const std::string key, std::string value);

  concordUtils::Status AddHlfTransaction(
      const com::vmware::concord::HlfRequest& hlf_request);

  concordUtils::Status WriteHlfBlock();

  com::vmware::concord::hlf::storage::HlfBlock GetHlfBlock(uint64_t);
};

}  // namespace hlf
}  // namespace concord

#endif  // BLOCKCHAIN_HLF_KVB_STORAGE_H_
