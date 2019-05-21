// Copyright 2018 VMware, all rights reserved

#ifndef BLOCKCHAIN_HLF_KVB_STORAGE_HPP
#define BLOCKCHAIN_HLF_KVB_STORAGE_HPP

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

class KVBHlfStorage {
 private:
  const Blockchain::ILocalKeyValueStorageReadOnly &roStorage_;
  Blockchain::IBlocksAppender *blockAppender_;
  Blockchain::SetOfKeyValuePairs updates;
  std::vector<com::vmware::concord::hlf::storage::HlfTransaction>
      pending_hlf_transactions;
  log4cplus::Logger logger;

  // BFT sequence number associated with HLF chaincode execution.
  uint64_t bftSequenceNum_ = 0;

  // 0x10 - 0x1F reserved for HLF
  const uint8_t TYPE_HLF_BLOCK = 0x10;
  const uint8_t TYPE_HLF_TRANSACTION = 0x11;
  const uint8_t TYPE_HLF_STATE = 0x12;

 public:
  // read-only mode
  KVBHlfStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage);

  // read-write mode
  KVBHlfStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
                Blockchain::IBlocksAppender *blockAppender,
                uint64_t sequenceNum);

  ~KVBHlfStorage();

  bool is_read_only();
  uint64_t next_block_number();
  const Blockchain::ILocalKeyValueStorageReadOnly &getReadOnlyStorage();

  uint64_t current_block_number();

  void reset();

  Blockchain::Status get(const Blockchain::Sliver &key,
                         Blockchain::Sliver &out);

  Blockchain::Status get(const Blockchain::BlockId readVersion,
                         const Blockchain::Sliver &key,
                         Blockchain::Sliver &value,
                         Blockchain::BlockId &outBlock);

  Blockchain::Sliver kvb_key(uint8_t type, const uint8_t *bytes,
                             size_t length) const;

  Blockchain::Sliver hlf_state_key(const uint8_t *key, size_t length) const;
  Blockchain::Sliver hlf_transaction_key(const uint8_t *key,
                                         size_t length) const;
  Blockchain::Sliver hlf_block_key(const uint8_t *key, size_t length) const;

  void put(const Blockchain::Sliver &key, const Blockchain::Sliver &value);

  string get_hlf_state(const uint8_t *key, size_t length);
  string get_hlf_state(const uint8_t *key, size_t length,
                       uint64_t &block_number);
  Blockchain::Status add_hlf_transaction(
      const com::vmware::concord::HlfRequest &hlfRequest);
  Blockchain::Status write_hlf_block();
  com::vmware::concord::hlf::storage::HlfBlock get_hlf_block(uint64_t);
  void set_hlf_state(const uint8_t *key, size_t length, string value);
};

}  // namespace hlf
}  // namespace blockchain
}  // namespace concord

#endif  // BLOCKCHAIN_HLF_KVB_STORAGE_HPP
