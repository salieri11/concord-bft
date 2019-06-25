// Copyright 2018 VMware, all rights reserved

#ifndef CONCORD_HLF_KVB_STORAGE_H_
#define CONCORD_HLF_KVB_STORAGE_H_

#include <log4cplus/loggingmacros.h>
#include <vector>
#include "common/concord_types.hpp"
#include "concord.pb.h"
#include "concord_storage.pb.h"
#include "consensus/hash_defs.h"
#include "consensus/sliver.hpp"
#include "consensus/status.hpp"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"
#include "storage/blockchain_interfaces.h"

namespace concord {
namespace hlf {

class HlfKvbStorage {
 private:
  const concord::storage::ILocalKeyValueStorageReadOnly& ro_storage_;
  concord::storage::IBlocksAppender* ptr_block_appender_;
  concord::storage::SetOfKeyValuePairs updates_;
  std::vector<com::vmware::concord::hlf::storage::HlfTransaction>
      pending_hlf_transactions_;
  log4cplus::Logger logger_;

 public:
  // 0x10 - 0x1F reserved for HLF
  const uint8_t kTypeHlfBlock = 0x10;
  const uint8_t kTypeHlfTransaction = 0x11;
  const uint8_t kTypeHlfState = 0x12;

  // read-only mode
  HlfKvbStorage(
      const concord::storage::ILocalKeyValueStorageReadOnly& ro_storage);

  // read-write mode
  HlfKvbStorage(
      const concord::storage::ILocalKeyValueStorageReadOnly& ro_storage,
      concord::storage::IBlocksAppender* block_appender, uint64_t sequence_num);

  ~HlfKvbStorage();

  bool is_read_only();
  uint64_t next_block_number();
  const concord::storage::ILocalKeyValueStorageReadOnly& getReadOnlyStorage();

  uint64_t current_block_number();

  void reset();

  concord::consensus::Status get(const concord::consensus::Sliver& key,
                                 concord::consensus::Sliver& out);

  concord::consensus::Status get(const concord::storage::BlockId read_version,
                                 const concord::consensus::Sliver& key,
                                 concord::consensus::Sliver& value,
                                 concord::storage::BlockId& out_block);

  concord::consensus::Sliver KvbKey(uint8_t type, const std::string& key) const;

  concord::consensus::Sliver KvbKey(uint8_t type, const uint8_t* bytes,
                                    size_t length) const;

  concord::consensus::Sliver HlfStateKey(const std::string& key) const;

  concord::consensus::Sliver HlfTransactionKey(const evm_uint256be& hash) const;

  concord::consensus::Sliver HlfBlockKey(const evm_uint256be& hash) const;

  void put(const concord::consensus::Sliver& key,
           const concord::consensus::Sliver& value);

  string GetHlfState(const std::string& key);

  string GetHlfState(const std::string& key, uint64_t& block_number);

  concord::consensus::Status SetHlfState(const string key, string value);

  concord::consensus::Status AddHlfTransaction(
      const com::vmware::concord::HlfRequest& hlf_request);

  concord::consensus::Status WriteHlfBlock();

  com::vmware::concord::hlf::storage::HlfBlock GetHlfBlock(uint64_t);
};

}  // namespace hlf
}  // namespace concord

#endif  // BLOCKCHAIN_HLF_KVB_STORAGE_H_
