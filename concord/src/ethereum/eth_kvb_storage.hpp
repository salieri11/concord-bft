// Copyright 2018 VMware, all rights reserved
//
// Wrapper around KVB to provide EVM execution storage context. This class
// defines the mapping of EVM object to KVB address. It also records updates to
// be used in minting a block when a transaction finishes.

#ifndef ETHEREUM_ETH_KVB_STORAGE_HPP
#define ETHEREUM_ETH_KVB_STORAGE_HPP

#include <vector>
#include "Logger.hpp"

#include "common/concord_types.hpp"
#include "db_interfaces.h"
#include "evm.h"
#include "sliver.hpp"
#include "status.hpp"

namespace concord {
namespace ethereum {

class EthKvbStorage {
 private:
  const concord::kvbc::ILocalKeyValueStorageReadOnly &roStorage_;
  concord::kvbc::IBlocksAppender *blockAppender_;
  concord::kvbc::SetOfKeyValuePairs updates;
  std::vector<concord::common::EthTransaction> pending_transactions;
  logging::Logger logger;

  concordUtils::Sliver kvb_key(uint8_t type, const uint8_t *bytes,
                               size_t length) const;

  concordUtils::Sliver block_key(const concord::common::EthBlock &blk) const;
  concordUtils::Sliver block_key(const evm_uint256be &hash) const;
  concordUtils::Sliver transaction_key(
      const concord::common::EthTransaction &tx) const;
  concordUtils::Sliver transaction_key(const evm_uint256be &hash) const;
  concordUtils::Sliver balance_key(const evm_address &addr) const;
  concordUtils::Sliver nonce_key(const evm_address &addr) const;
  concordUtils::Sliver code_key(const evm_address &addr) const;
  concordUtils::Sliver storage_key(const evm_address &addr,
                                   const evm_uint256be &location) const;
  concordUtils::Status get(const concordUtils::Sliver &key,
                           concordUtils::Sliver &out);
  concordUtils::Status get(const kvbc::BlockId readVersion,
                           const concordUtils::Sliver &key,
                           concordUtils::Sliver &value,
                           kvbc::BlockId &outBlock);
  void put(const concordUtils::Sliver &key, const concordUtils::Sliver &value);

  uint64_t next_block_number();
  void add_block(concord::common::EthBlock &blk);

 public:
  // read-only mode
  EthKvbStorage(const concord::kvbc::ILocalKeyValueStorageReadOnly &roStorage);

  // read-write mode
  EthKvbStorage(const concord::kvbc::ILocalKeyValueStorageReadOnly &roStorage,
                concord::kvbc::IBlocksAppender *blockAppender);

  ~EthKvbStorage();

  bool is_read_only();
  const concord::kvbc::ILocalKeyValueStorageReadOnly &getReadOnlyStorage();

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

  concordUtils::Status write_block(uint64_t timestamp, uint64_t gas_limit);
  void reset();
  void add_transaction(concord::common::EthTransaction &tx);
  void set_balance(const evm_address &addr, evm_uint256be balance);
  void set_nonce(const evm_address &addr, uint64_t nonce);
  void set_code(const evm_address &addr, const uint8_t *code, size_t code_size);
  void set_storage(const evm_address &addr, const evm_uint256be &location,
                   const evm_uint256be &data);
};

}  // namespace ethereum
}  // namespace concord

#endif  // ETHEREUM_ETH_KVB_STORAGE_HPP
