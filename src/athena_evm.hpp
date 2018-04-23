// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#ifndef ATHENA_EVM_HPP
#define ATHENA_EVM_HPP

#include <map>
#include <vector>
#include <unordered_map>
#include <memory>
#include <log4cplus/loggingmacros.h>
#include "common/utils.hpp"
#include "evm.h"
#include "filter_manager.hpp"
#include "athena_types.hpp"
#include "evm_init_params.hpp"


namespace com {
namespace vmware {
namespace athena {

class EVMException: public std::exception {
public:
   explicit EVMException(const std::string &what): msg(what) {};

   virtual const char* what() const noexcept override
   {
      return msg.c_str();
   }

private:
   std::string msg;
};

class TransactionNotFoundException: public EVMException {
public:
   TransactionNotFoundException() :
      EVMException("Transaction not found") { }
};

class BlockNotFoundException: public EVMException {
public:
   BlockNotFoundException() :
      EVMException("Block not found") { }
};

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


// forward declaration to break circular references between
// athena_evm.hpp and filter_manager.hpp
class FilterManager;


class EVM {
public:
   explicit EVM(EVMInitParams params);
   ~EVM();

   /* Athena API */
   void run(evm_message &message,
                bool isTransaction,
                evm_result &result,
                evm_uint256be &txhash /* out */);
   void create(evm_message &message, evm_result &result,
               evm_uint256be &txhash /* out */);
   EthTransaction get_transaction(const evm_uint256be &txhash) const;
   evm_uint256be get_storage_at(const evm_address &account,
                                const evm_uint256be &key) const;
   FilterManager* get_filter_manager();
   std::vector<std::shared_ptr<EthBlock>> get_block_list(uint64_t latest,
                                                         uint64_t count) const;
   std::shared_ptr<EthBlock> get_block_for_number(uint64_t number) const;
   std::shared_ptr<EthBlock> get_block_for_hash(evm_uint256be hash) const;
   bool new_account(const std::string& passphrase, evm_address& address);

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

   uint64_t current_block_number() const;

private:
   athena_context athctx;
   evm_instance *evminst;
   log4cplus::Logger logger;

   // chain to which we are connected
   uint64_t chainId;

   // map from account address to account balance
   std::map<evm_address, uint64_t> balances;

   // map from contract address to a pair of (contract code, code hash)
   std::map<evm_address, std::pair<std::vector<uint8_t>, evm_uint256be>>
      contract_code;

   // map from account address to latest nonce
   std::map<evm_address, uint64_t> nonces;

   // the transactions we have processed; map is hash -> tx
   std::map<evm_uint256be, EthTransaction> transactions;

   // transactions in flight for the current block
   std::vector<EthTransaction> pending;

   // the blocks we have created, in two mappings: by hash and by number.
   // using shared pointers inside the maps, so that the memory will be cleaned
   // up later.
   //TODO: unoordered map
   std::map<evm_uint256be, std::shared_ptr<EthBlock>> blocks_by_hash;
   std::map<uint64_t, std::shared_ptr<EthBlock>> blocks_by_number;

   // the latest block id we have used
   // TODO: make this atomic
   uint64_t latestBlock = 0;

   // map from [(contract address)+(storage location)] to data at that location
   std::map<std::vector<uint8_t>, evm_uint256be> storage_map;

   // Instace of filter manager
   FilterManager *filterManager;

   void create_genesis_block(EVMInitParams params);
   evm_address contract_destination(const evm_message &message);
   evm_uint256be keccak_hash(const std::vector<uint8_t> &data) const;
   void execute(evm_message &message,
                const std::vector<uint8_t> &code,
                evm_result &result /* out */);
   bool get_code(const evm_address &address,
                 std::vector<uint8_t> &result_code,
                 evm_uint256be &result_hash);
   uint64_t get_nonce(const evm_address &address);
   uint64_t next_block_number();

   evm_uint256be hash_for_transaction(const EthTransaction &tx) const;
   evm_uint256be hash_for_block(const std::shared_ptr<EthBlock> tx) const;
   evm_uint256be record_transaction(const size_t pending_index,
                                    const evm_message &message,
                                    const evm_result &result,
                                    const evm_address &to_override,
                                    const evm_address &contract_address);
   void record_block();
   std::vector<uint8_t> storage_key(const struct evm_address* address,
                                    const struct evm_uint256be* key) const;
};

}
}
}

#endif //ATHENA_EVM_HPP
