// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#ifndef ATHENA_EVM_HPP
#define ATHENA_EVM_HPP

#include <map>
#include <vector>
#include <log4cplus/loggingmacros.h>
#include "evm.h"

// TODO: We may want "release" versions of these macros that print fewer bytes.

/** Hex-format the bytes of an evm_address for Log4Cplus. */
#define LOG4CPLUS_HEXADDR(addr) "0x" \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[0] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[1] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[2] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[3] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[4] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[5] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[6] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[7] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[8] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[9] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[10] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[11] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[12] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[13] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[14] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[15] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[16] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[17] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[18] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)addr->bytes[19] \
   << std::dec

/** Hex-format the bytes of an evm_uint256be for Log4Cplus. */
#define LOG4CPLUS_HEXVAL(val) "0x" \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[0] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[1] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[2] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[3] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[4] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[5] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[6] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[7] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[8] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[9] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[10] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[11] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[12] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[13] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[14] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[15] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[16] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[17] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[18] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[19] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[20] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[21] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[22] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[23] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[24] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[25] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[26] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[27] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[28] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[29] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[30] \
   << std::hex << std::setw(2) << std::setfill('0') << (uint)val->bytes[31] \
   << std::dec

namespace com {
namespace vmware {
namespace athena {
namespace evm {

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
 * The next several ath_* functions are callbacks that the EVM uses to interact
 * with our state-keeping layer.
 */

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

/**
 * Function dispatch table for EVM. Specified by EEI.
 */
const static struct evm_context_fn_table athena_fn_table = {
    ath_account_exists,
    ath_get_storage,
    ath_set_storage,
    ath_get_balance,
    ath_get_code,
    ath_selfdestruct,
    ath_call,
    ath_get_tx_context,
    ath_get_block_hash,
    ath_emit_log
};

/**
 * EVM context is currently just a wrapper around the function table. Are
 * they going to add more to it?
 */
static struct evm_context athena_evm_context = { &athena_fn_table };

/**
 * Our wrapper around EVM's wrapper, where we can add pointers to the modules
 * we're using to keep state.
 */
typedef struct athena_context {
   struct evm_context evmctx;

   // map address to the code stored there
   std::map<std::vector<uint8_t>, std::vector<uint8_t>> *codemap;
} athena_context;

/**
 * Our actual state. This is a singleton for now, as we're only running one
 * chain, so the EVM will apply one op at a time.
 */
static athena_context *athctx;
static evm_instance *evminst;

/**
 * Initialize the context and evm instance. TODO: Add parameters for data
 * directory, genesis block definition, etc.
 */
void init_evm();

/**
 * Shutdown and dealloc the evm instance and its contexts. Safe to call multiple
 * times, including if init_evm was not called or returns false.
 */
void stop_evm();

/**
 * Send a message to EVM for evaluation.
 */
void execute(
   evm_message *message,
   const uint8_t *code,
   size_t code_size,
   evm_result *result);

}}}}

#endif //ATHENA_EVM_HPP
