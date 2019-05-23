// Copyright 2018 VMware, all rights reserved

#ifndef BLOCKCHAIN_HLF_KVB_STORAGE_H_
#define BLOCKCHAIN_HLF_KVB_STORAGE_H_

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
namespace blockchain {
namespace hlf {

class KvbStorageForHlf {
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
  KvbStorageForHlf(const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage);

  // read-write mode
  KvbStorageForHlf(const Blockchain::ILocalKeyValueStorageReadOnly& ro_storage,
                   Blockchain::IBlocksAppender* block_appender,
                   uint64_t sequence_num);

  ~KvbStorageForHlf();

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

  Blockchain::Sliver KvbKey(uint8_t type, const uint8_t* bytes,
                            size_t length) const;

  Blockchain::Sliver HlfStateKey(const uint8_t* key, size_t length) const;
  Blockchain::Sliver HlfTransactionKey(const uint8_t* key, size_t length) const;
  Blockchain::Sliver HlfBlockKey(const uint8_t* key, size_t length) const;

  void put(const Blockchain::Sliver& key, const Blockchain::Sliver& value);

  string GetHlfState(const uint8_t* key, size_t length);
  string GetHlfState(const uint8_t* key, size_t length, uint64_t& block_number);
  Blockchain::Status AddHlfTransaction(
      const com::vmware::concord::HlfRequest& hlf_request);

  Blockchain::Status WriteHlfBlock();

  com::vmware::concord::hlf::storage::HlfBlock GetHlfBlock(uint64_t);
  void SetHlfState(const uint8_t* key, size_t length, string value);
};

}  // namespace hlf
}  // namespace blockchain
}  // namespace concord

#endif  // BLOCKCHAIN_HLF_KVB_STORAGE_H_
