// Copyright 2018 VMware, all rights reserved
//
// Wrapper around KVB to provide EVM execution storage context. This class
// defines the mapping of EVM object to KVB address. It also records updates to
// be used in minting a block when a transaction finishes.

#ifndef BLOCKCHAIN_KVB_STORAGE_HPP
#define BLOCKCHAIN_KVB_STORAGE_HPP

#include <log4cplus/loggingmacros.h>
#include <vector>

#include "common/concord_types.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "consensus/kvb/HashDefs.h"
#include "consensus/kvb/sliver.hpp"
#include "evm.h"

namespace concord {
namespace blockchain {

class KVBStorage {
 private:
  const Blockchain::ILocalKeyValueStorageReadOnly &roStorage_;
  Blockchain::IBlocksAppender *blockAppender_;
  Blockchain::SetOfKeyValuePairs updates;
  std::vector<concord::common::EthTransaction> pending_transactions;
  log4cplus::Logger logger;
  // BFT sequence number associated with EVM contract execution.
  uint64_t bftSequenceNum_ = 0;

  /* Value of "type" byte, at the start of each key. */
  const uint8_t TYPE_BLOCK = 0x01;
  const uint8_t TYPE_TRANSACTION = 0x02;
  const uint8_t TYPE_BALANCE = 0x03;
  const uint8_t TYPE_CODE = 0x04;
  const uint8_t TYPE_STORAGE = 0x05;
  const uint8_t TYPE_NONCE = 0x06;
  const uint8_t TYPE_BLOCK_METADATA = 0x07;

  Blockchain::Sliver kvb_key(uint8_t type, const uint8_t *bytes,
                             size_t length) const;

  Blockchain::Sliver block_key(const concord::common::EthBlock &blk) const;
  Blockchain::Sliver block_key(const evm_uint256be &hash) const;
  Blockchain::Sliver transaction_key(
      const concord::common::EthTransaction &tx) const;
  Blockchain::Sliver transaction_key(const evm_uint256be &hash) const;
  Blockchain::Sliver balance_key(const evm_address &addr) const;
  Blockchain::Sliver nonce_key(const evm_address &addr) const;
  Blockchain::Sliver code_key(const evm_address &addr) const;
  Blockchain::Sliver storage_key(const evm_address &addr,
                                 const evm_uint256be &location) const;
  Blockchain::Status get(const Blockchain::Sliver &key,
                         Blockchain::Sliver &out);
  Blockchain::Status get(const Blockchain::BlockId readVersion,
                         const Blockchain::Sliver &key,
                         Blockchain::Sliver &value,
                         Blockchain::BlockId &outBlock);
  void put(const Blockchain::Sliver &key, const Blockchain::Sliver &value);

  uint64_t next_block_number();
  void add_block(concord::common::EthBlock &blk);

 public:
  // read-only mode
  KVBStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage);

  // read-write mode
  KVBStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
             Blockchain::IBlocksAppender *blockAppender, uint64_t sequenceNum);

  ~KVBStorage();

  bool is_read_only();
  const Blockchain::ILocalKeyValueStorageReadOnly &getReadOnlyStorage();

  uint64_t current_block_number();
  concord::common::EthBlock get_block(const evm_uint256be &hash);
  concord::common::EthBlock get_block(uint64_t number);
  concord::common::EthTransaction get_transaction(const evm_uint256be &hash);
  evm_uint256be get_balance(const evm_address &addr);
  evm_uint256be get_balance(const evm_address &addr, uint64_t &block_number);
  uint64_t get_nonce(const evm_address &addr);
  uint64_t get_nonce(const evm_address &addr, uint64_t &block_number);
  bool account_exists(const evm_address &addr);
  bool get_code(const evm_address &addr, std::vector<uint8_t> &out,
                evm_uint256be &hash);
  bool get_code(const evm_address &addr, std::vector<uint8_t> &out,
                evm_uint256be &hash, uint64_t &block_number);
  evm_uint256be get_storage(const evm_address &addr,
                            const evm_uint256be &location);
  evm_uint256be get_storage(const evm_address &addr,
                            const evm_uint256be &location,
                            uint64_t &block_number);
  Blockchain::Sliver build_block_metadata_key() const;
  uint64_t get_block_metadata(Blockchain::Sliver key);

  Blockchain::Status write_block(uint64_t timestamp, uint64_t gas_limit);
  void reset();
  void add_transaction(concord::common::EthTransaction &tx);
  void set_balance(const evm_address &addr, evm_uint256be balance);
  void set_nonce(const evm_address &addr, uint64_t nonce);
  void set_code(const evm_address &addr, const uint8_t *code, size_t code_size);
  void set_storage(const evm_address &addr, const evm_uint256be &location,
                   const evm_uint256be &data);
  Blockchain::Sliver set_block_metadata_value(uint64_t bftSequenceNum) const;
  void set_block_metadata();
};

}  // namespace blockchain
}  // namespace concord

#endif  // BLOCKCHAIN_KVB_STORAGE_HPP
