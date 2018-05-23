// Copyright 2018 VMware, all rights reserved
//
// Wrapper around KVB to provide EVM execution storage context. This class
// defines the mapping of EVM object to KVB address. It also records updates to
// be used in minting a block when a transaction finishes.

#ifndef ATHENA_KVB_STORAGE_HPP
#define ATHENA_KVB_STORAGE_HPP

#include <vector>
#include <log4cplus/loggingmacros.h>

#include "athena_types.hpp"
#include "kvb/slice.h"
#include "kvb/BlockchainInterfaces.h"
#include "kvb/HashDefs.h"
#include "evm.h"

namespace com {
namespace vmware {
namespace athena {

class KVBStorage {
private:
   const Blockchain::ILocalKeyValueStorageReadOnly &roStorage_;
   Blockchain::IBlocksAppender *blockAppender_;
   Blockchain::SetOfKeyValuePairs updates;
   log4cplus::Logger logger;

   const char TYPE_BLOCK       = 0x01;
   const char TYPE_TRANSACTION = 0x02;
   const char TYPE_BALANCE     = 0x03;
   const char TYPE_CODE        = 0x04;
   const char TYPE_STORAGE     = 0x05;
   const char TYPE_NONCE       = 0x06;

   Blockchain::Slice kvb_key(
      char type, const uint8_t *bytes, size_t length) const;

   Blockchain::Slice block_key(const EthBlock &blk) const;
   Blockchain::Slice block_key(const evm_uint256be &hash) const;
   Blockchain::Slice transaction_key(const EthTransaction &tx) const;
   Blockchain::Slice transaction_key(const evm_uint256be &hash) const;
   Blockchain::Slice balance_key(const evm_address &addr) const;
   Blockchain::Slice nonce_key(const evm_address &addr) const;
   Blockchain::Slice code_key(const evm_address &addr) const;
   Blockchain::Slice storage_key(const evm_address &addr,
                                 const evm_uint256be &location) const;

   Blockchain::Status get(const Blockchain::Slice &key,
                          Blockchain::Slice &out);
   void put(const Blockchain::Slice &key, const Blockchain::Slice &value);

   uint64_t next_block_number();
   void add_block(EthBlock &blk);

public:
   // read-only mode
   KVBStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage);

   // read-write mode
   KVBStorage(const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
              Blockchain::IBlocksAppender *blockAppender);

   bool is_read_only();

   uint64_t current_block_number();
   EthBlock get_block(const evm_uint256be &hash);
   EthBlock get_block(uint64_t number);
   EthTransaction get_transaction(const evm_uint256be &hash);
   uint64_t get_balance(const evm_address &addr);
   uint64_t get_nonce(const evm_address &addr);
   bool account_exists(const evm_address &addr);
   bool get_code(const evm_address &addr,
                 std::vector<uint8_t> &out,
                 evm_uint256be &hash);
   evm_uint256be get_storage(const evm_address &addr,
                             const evm_uint256be &location);

   void write_block();
   void add_transaction(EthTransaction &tx);
   void set_balance(const evm_address &addr, uint64_t balance);
   void set_nonce(const evm_address &addr, uint64_t nonce);
   void set_code(const evm_address &addr,
                 const uint8_t *code,
                 size_t code_size);
   void set_storage(const evm_address &addr,
                    const evm_uint256be &location,
                    const evm_uint256be &data);
};

}
}
}

#endif
