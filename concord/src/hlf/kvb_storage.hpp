// Copyright 2018 VMware, all rights reserved

#ifndef CONCORD_HLF_KVB_STORAGE_H_
#define CONCORD_HLF_KVB_STORAGE_H_

#include <log4cplus/loggingmacros.h>
#include <vector>
#include "common/concord_types.hpp"
#include "concord.pb.h"
#include "concord_storage.pb.h"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "consensus/kvb/HashDefs.h"
#include "consensus/kvb/sliver.hpp"
#include "hlf_services.pb.h"
#include "hlf_storage.pb.h"

namespace concord {
namespace hlf {

class HlfKvbStorage {
 private:
  const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage_;
  Blockchain::IBlocksAppender* ptr_block_appender_;
  Blockchain::SetOfKeyValuePairs updates_;
  std::vector<com::vmware::concord::hlf::storage::HlfTransaction>
      pending_hlf_transactions_;
  log4cplus::Logger logger_;

  // BFT sequence number associated with HLF chaincode execution.
  uint64_t bft_sequence_num_ = 0;

 public:
  // 0x10 - 0x1F reserved for HLF
  const uint8_t kTypeHlfBlock = 0x10;
  const uint8_t kTypeHlfTransaction = 0x11;
  const uint8_t kTypeHlfState = 0x12;

  // read-only mode
  HlfKvbStorage(const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage);

  // read-write mode
  HlfKvbStorage(const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage,
                Blockchain::IBlocksAppender* block_appender,
                uint64_t sequence_num);

  ~HlfKvbStorage();

  bool is_read_only();
  uint64_t next_block_number();
  const Blockchain::ILocalKeyValueStorageReadOnly& getReadOnlyStorage();

  uint64_t current_block_number();

  void reset();

  Blockchain::Status get(const Blockchain::Sliver& key,
                         Blockchain::Sliver& out);

  Blockchain::Status get(const Blockchain::BlockId read_version,
                         const Blockchain::Sliver& key,
                         Blockchain::Sliver& value,
                         Blockchain::BlockId& out_block);

  Blockchain::Sliver KvbKey(uint8_t type, const std::string& key) const;

  Blockchain::Sliver KvbKey(uint8_t type, const uint8_t* bytes,
                            size_t length) const;

  Blockchain::Sliver HlfStateKey(const std::string& key) const;

  Blockchain::Sliver HlfTransactionKey(const evm_uint256be& hash) const;

  Blockchain::Sliver HlfBlockKey(const evm_uint256be& hash) const;

  void put(const Blockchain::Sliver& key, const Blockchain::Sliver& value);

  string GetHlfState(const std::string& key);

  string GetHlfState(const std::string& key, uint64_t& block_number);

  void SetHlfState(const string key, string value);

  Blockchain::Status AddHlfTransaction(
      const com::vmware::concord::HlfRequest& hlf_request);

  Blockchain::Status WriteHlfBlock();

  com::vmware::concord::hlf::storage::HlfBlock GetHlfBlock(uint64_t);
};

}  // namespace hlf
}  // namespace concord

#endif  // BLOCKCHAIN_HLF_KVB_STORAGE_H_
