// Copyright 2018 VMware, all rights reserved
//
// Wrapper around KVB to provide EVM execution storage context. This class
// defines the mapping of EVM object to KVB address. It also records updates to
// be used in minting a block when a transaction finishes.

#ifndef CONCORD_KVB_STORAGE_HPP
#define CONCORD_KVB_STORAGE_HPP

#include <log4cplus/loggingmacros.h>
#include <vector>

#include "concord_types.hpp"
#include "evm.h"
#include "kvb/BlockchainInterfaces.h"
#include "kvb/HashDefs.h"
#include "kvb/sliver.hpp"

namespace com {
namespace vmware {
namespace concord {

class KVBStorage {
 private:
  const Blockchain::ILocalKeyValueStorageReadOnly &roStorage_;
  Blockchain::IBlocksAppender *blockAppender_;
  Blockchain::SetOfKeyValuePairs updates;
  std::vector<EthTransaction> pending_transactions;
  log4cplus::Logger logger;

  /* Value of "type" byte, at the start of each key. */
  const uint8_t TYPE_BLOCK = 0x01;
  const uint8_t TYPE_TRANSACTION = 0x02;
  const uint8_t TYPE_BALANCE = 0x03;
  const uint8_t TYPE_CODE = 0x04;
  const uint8_t TYPE_STORAGE = 0x05;
  const uint8_t TYPE_NONCE = 0x06;

  Blockchain::Sliver kvb_key(uint8_t type, const uint8_t *bytes,
                             size_t length) const;

  Blockchain::Sliver block_key(const EthBlock &blk) const;
  Blockchain::Sliver block_key(const evm_uint256be &hash) const;
  Blockchain::Sliver transaction_key(const EthTransaction &tx) const;
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
  void add_block(EthBlock &blk);

 public:
  // read-only mode
  KVBStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage);

  // read-write mode
  KVBStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
             Blockchain::IBlocksAppender *blockAppender);

  ~KVBStorage();

  bool is_read_only();
  const Blockchain::ILocalKeyValueStorageReadOnly &getReadOnlyStorage();

  uint64_t current_block_number();
  EthBlock get_block(const evm_uint256be &hash);
  EthBlock get_block(uint64_t number);
  EthTransaction get_transaction(const evm_uint256be &hash);
  uint64_t get_balance(const evm_address &addr);
  uint64_t get_balance(const evm_address &addr, uint64_t &block_number);
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

  Blockchain::Status write_block(uint64_t timestamp, uint64_t gas_limit);
  void reset();
  void add_transaction(EthTransaction &tx);
  void set_balance(const evm_address &addr, uint64_t balance);
  void set_nonce(const evm_address &addr, uint64_t nonce);
  void set_code(const evm_address &addr, const uint8_t *code, size_t code_size);
  void set_storage(const evm_address &addr, const evm_uint256be &location,
                   const evm_uint256be &data);
};

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif
