// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#ifndef ATHENA_EVM_HPP
#define ATHENA_EVM_HPP

#include <map>
#include <vector>
#include <memory>
#include <log4cplus/loggingmacros.h>
#include "common/utils.hpp"
#include "evm.h"
#include "athena_types.hpp"
#include "athena_kvb_storage.hpp"
#include "evm_init_params.hpp"
#include "kvb/BlockchainInterfaces.h"

namespace com {
namespace vmware {
namespace athena {

/**
 * Our wrapper around EVM's wrapper, where we can add pointers to the modules
 * we're using to keep state.
 */
typedef struct athena_context {
   /** evmctx must be first, so we can cast to our wrapper */
   struct evm_context evmctx;
   class EVM *ath_object;
} athena_context;

extern "C" {
/**
 * This extern block of ath_* functions are callbacks that the EVM uses to interact
 * with our state-keeping layer.
 */

   EVM* ath_object(const struct evm_context* evmctx);
   int ath_account_exists(struct evm_context* evmctx,
                          const struct evm_address* address);
   void ath_get_storage(struct evm_uint256be* result,
                        struct evm_context* evmctx,
                        const struct evm_address* address,
                        const struct evm_uint256be* key);
   void ath_set_storage(struct evm_context* evmctx,
                        const struct evm_address* address,
                        const struct evm_uint256be* key,
                        const struct evm_uint256be* value);
   void ath_get_balance(struct evm_uint256be* result,
                        struct evm_context* evmctx,
                        const struct evm_address* address);
   size_t ath_get_code_size(struct evm_context* evmctx,
                            const struct evm_address* address);
   size_t ath_get_code(const uint8_t** result_code,
                       struct evm_context* evmctx,
                       const struct evm_address* address);
   void ath_selfdestruct(struct evm_context* evmctx,
                         const struct evm_address* address,
                         const struct evm_address* beneficiary);
   void ath_emit_log(struct evm_context* evmctx,
                     const struct evm_address* address,
                     const uint8_t* data,
                     size_t data_size,
                     const struct evm_uint256be topics[],
                     size_t topics_count);
   void ath_call(struct evm_result* result,
                 struct evm_context* evmctx,
                 const struct evm_message* msg);
   void ath_get_block_hash(struct evm_uint256be* result,
                           struct evm_context* evmctx,
                           int64_t number);
   void ath_get_tx_context(struct evm_tx_context* result,
                           struct evm_context* evmctx);
}

/**
 * Function dispatch table for EVM. Specified by EEI.
 */
const static struct evm_context_fn_table athena_fn_table = {
      ath_account_exists,
      ath_get_storage,
      ath_set_storage,
      ath_get_balance,
      ath_get_code_size,
      ath_get_code,
      ath_selfdestruct,
      ath_call,
      ath_get_tx_context,
      ath_get_block_hash,
      ath_emit_log
};

class EVM {
public:
   explicit EVM(EVMInitParams params);
   ~EVM();

   /* Athena API */
   void run(evm_message &message,
            KVBStorage &kvbStorage,
            evm_result &result, /* out */
            evm_uint256be &txhash /* out */);
   void create(evm_message &message,
               KVBStorage &kvbStorage,
               evm_result &result, /* out */
               evm_uint256be &txhash /* out */);
   EthTransaction get_transaction(const evm_uint256be &txhash,
                                  KVBStorage &kvbStorage) const;
   evm_uint256be get_storage_at(const evm_address &account,
                                const evm_uint256be &key) const;
   bool get_code(const evm_address &address,
                 std::vector<uint8_t> &result_code,
                 evm_uint256be &result_hash) const;
   std::vector<EthBlock> get_block_list(uint64_t latest,
                                        uint64_t count,
                                        KVBStorage &kvbStorage) const;
   EthBlock get_block_for_number(uint64_t number,
                                 KVBStorage &kvbStorage) const;
   EthBlock get_block_for_hash(evm_uint256be hash,
                               KVBStorage &kvbStorage) const;
   bool new_account(const std::string &passphrase,
                    KVBStorage &kvbStorage,
                    evm_address &address /* OUT */);

   /* EVM callbacks */
   int account_exists(const struct evm_address* address);
   void get_storage(struct evm_uint256be* result,
                    const struct evm_address* address,
                    const struct evm_uint256be* key) const;
   void set_storage(const struct evm_address* address,
                    const struct evm_uint256be* key,
                    const struct evm_uint256be* value);
   void get_balance(struct evm_uint256be* result,
                    const struct evm_address* address);
   bool get_code(const struct evm_address* address,
                 std::vector<uint8_t> &result_code,
                 evm_uint256be &result_hash);
   void selfdestruct(const struct evm_address* address,
                     const struct evm_address* beneficiary);
   void emit_log(const struct evm_address* address,
                 const uint8_t* data,
                 size_t data_size,
                 const struct evm_uint256be topics[],
                 size_t topics_count);
   void call(struct evm_result* result,
             const struct evm_message* msg);
   void get_block_hash(struct evm_uint256be* result,
                       int64_t number);
   void get_tx_context(struct evm_tx_context* result);

private:
   athena_context athctx;
   evm_instance *evminst;
   log4cplus::Logger logger;

   // chain to which we are connected
   uint64_t chainId;

   // only available during `run` and `create` calls; will be in read-only mode
   // during `run` calls that are read-only (non-transaction)
   KVBStorage *txctx_kvbStorage;

   // map from account address to latest nonce
   std::map<evm_address, uint64_t> nonces;

   // map from [(contract address)+(storage location)] to data at that location
   std::map<std::vector<uint8_t>, evm_uint256be> storage_map;

   void create_genesis_block(EVMInitParams params);
   evm_address contract_destination(const evm_message &message);
   void execute(evm_message &message,
                const std::vector<uint8_t> &code,
                evm_result &result /* out */);
   uint64_t get_nonce(const evm_address &address);

   evm_uint256be record_transaction(const evm_message &message,
                                    const evm_result &result,
                                    const evm_address &to_override,
                                    const evm_address &contract_address,
                                    KVBStorage &kvbStorage);
   void record_block(EthTransaction &tx, KVBStorage &kvbStorage);
   std::vector<uint8_t> storage_key(const struct evm_address* address,
                                    const struct evm_uint256be* key) const;

public:
   // TODO(BWF): move to common?
   static evm_uint256be keccak_hash(const std::vector<uint8_t> &data);
   static evm_uint256be keccak_hash(const uint8_t *data, size_t size);

};

}
}
}

#endif //ATHENA_EVM_HPP
