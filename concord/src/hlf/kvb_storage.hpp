// Copyright 2018 VMware, all rights reserved

#ifndef CONCORD_HLF_KVB_STORAGE_H_
#define CONCORD_HLF_KVB_STORAGE_H_

#include <log4cplus/loggingmacros.h>
#include <vector>
#include "blockchain/db_interfaces.h"
#include "common/concord_types.hpp"
#include "concord.pb.h"
#include "concord_storage.pb.h"
#include "hash_defs.h"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"
#include "sliver.hpp"
#include "status.hpp"

namespace concord {
namespace hlf {

class HlfKvbStorage {
 private:
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly&
      ro_storage_;
  concord::storage::blockchain::IBlocksAppender* ptr_block_appender_;
  static concord::storage::SetOfKeyValuePairs updates_;
  std::vector<com::vmware::concord::hlf::storage::HlfTransaction>
      pending_hlf_transactions_;
  log4cplus::Logger logger_;

 public:
  // This is used for adding block id to the
  // result of GetHlfState
  const std::string kStateSeparator = "%%";

  // read-only mode
  HlfKvbStorage(
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly&
          ro_storage);

  // read-write mode
  HlfKvbStorage(
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly&
          ro_storage,
      concord::storage::blockchain::IBlocksAppender* block_appender);

  ~HlfKvbStorage();

  bool is_read_only();
  uint64_t next_block_number();
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly&
  getReadOnlyStorage();

  uint64_t current_block_number();

  void reset();

  concordUtils::Status get(const concordUtils::Sliver& key,
                           concordUtils::Sliver& out);

  concordUtils::Status get(const concordUtils::BlockId read_version,
                           const concordUtils::Sliver& key,
                           concordUtils::Sliver& value,
                           concordUtils::BlockId& out_block);

  concordUtils::Sliver KvbKey(uint8_t type, const std::string& key) const;

  concordUtils::Sliver KvbKey(uint8_t type, const uint8_t* bytes,
                              size_t length) const;

  concordUtils::Sliver HlfStateKey(const std::string& key) const;

  concordUtils::Sliver HlfTransactionKey(const evm_uint256be& hash) const;

  concordUtils::Sliver HlfBlockKey(const evm_uint256be& hash) const;

  void put(const concordUtils::Sliver& key, const concordUtils::Sliver& value);

  string GetHlfState(const std::string& key);

  string GetHlfState(const std::string& key, uint64_t& block_number);

  concordUtils::Status SetHlfState(const string key, string value);

  concordUtils::Status AddHlfTransaction(
      const com::vmware::concord::HlfRequest& hlf_request);

  concordUtils::Status WriteHlfBlock();

  com::vmware::concord::hlf::storage::HlfBlock GetHlfBlock(uint64_t);
};

}  // namespace hlf
}  // namespace concord

#endif  // BLOCKCHAIN_HLF_KVB_STORAGE_H_
