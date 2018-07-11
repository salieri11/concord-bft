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

// forward declaration for callbacks.
class EVM;

/**
 * This extern block of ath_* functions are callbacks that the EVM uses to interact
 * with our state-keeping layer.
 */
extern "C" {
   /**
    * Our wrapper around EVM's wrapper, where we can add pointers to the modules
    * we're using to keep state.
    */
   typedef struct athena_context {
      /** evmctx must be first, so we can cast to our wrapper */
      struct evm_context evmctx;
      class EVM *ath_object;
      class KVBStorage *kvbStorage;
      log4cplus::Logger *logger;
   } athena_context;

   EVM* ath_object(const struct evm_context* evmctx);
   const athena_context* ath_context(const struct evm_context* evmctx);

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

  /*
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
}

class EVM {
public:
   explicit EVM(EVMInitParams params);
   ~EVM();

   /* Athena API */
   void run(evm_message &message,
            KVBStorage &kvbStorage,
            evm_result &result /* out */);
   void create(evm_message &message,
               KVBStorage &kvbStorage,
               evm_result &result /* out */);
   bool new_account(const std::string &passphrase,
                    KVBStorage &kvbStorage,
                    evm_address &address /* OUT */);

private:
   evm_instance *evminst;
   log4cplus::Logger logger;

   // chain to which we are connected
   uint64_t chainId;

   evm_address contract_destination(const evm_message &message,
                                    KVBStorage &kvbStorage);
   void execute(evm_message &message,
                KVBStorage &kvbStorage,
                const std::vector<uint8_t> &code,
                evm_result &result /* out */);
};

}
}
}

#endif //ATHENA_EVM_HPP
