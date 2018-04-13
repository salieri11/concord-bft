// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#ifndef ATHENA_EVM_HPP
#define ATHENA_EVM_HPP

#include <map>
#include <vector>
#include <log4cplus/loggingmacros.h>
#include "common/utils.hpp"
#include "evm.h"
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
   void call(evm_message &message, evm_result &result,
             evm_uint256be &txhash /* out */);
   void create(evm_message &message, evm_result &result,
               evm_uint256be &txhash /* out */);
   EthTransaction get_transaction(evm_uint256be &txhash);
   std::vector<uint8_t> get_storage_at(std::vector<uint8_t> &account,
                                       std::vector<uint8_t> &key);

   /* EVM callbacks */
   int account_exists(const struct evm_address* address);
   void get_storage(struct evm_uint256be* result,
                    const struct evm_address* address,
                    const struct evm_uint256be* key);
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

   // map from account address to account balance
   std::map<std::vector<uint8_t>, uint64_t> balances;

   // map from contract address to a pair of (contract code, code hash)
   std::map<std::vector<uint8_t>,
            std::pair<std::vector<uint8_t>, evm_uint256be>>
       contract_code;

   // map from account address to latest nonce
   std::map<std::vector<uint8_t>, uint64_t> nonces;

   // the transactions we have processed; map is hash -> tx
   std::map<evm_uint256be, EthTransaction> transactions;

   // map from [(contract address)+(storage location)] to data at that location
   std::map<std::vector<uint8_t>, std::vector<uint8_t>> storage_map;

   void contract_destination(evm_message &message,
                             std::vector<uint8_t> &address);
   void keccak_hash(std::vector<uint8_t> &data,
                    evm_uint256be &hash);
   void execute(evm_message &message,
                const std::vector<uint8_t> &code,
                evm_result &result /* out */);
   bool get_code(const std::vector<uint8_t> &address,
                 std::vector<uint8_t> &result_code,
                 evm_uint256be &result_hash);
   uint64_t get_nonce(std::vector<uint8_t> &address);
   void hash_for_transaction(EthTransaction &tx,
                             evm_uint256be &hash /* out */);
   void record_transaction(evm_message &message,
                           evm_result &result,
                           std::vector<uint8_t> &to_override,
                           std::vector<uint8_t> &contract_address,
                           evm_uint256be &txhash /* out */);
   std::vector<uint8_t> storage_key(const struct evm_address* address,
                                    const struct evm_uint256be* key);
};

}
}
}

#endif //ATHENA_EVM_HPP
