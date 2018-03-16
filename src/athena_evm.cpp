// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#include <iostream>
#include <iomanip>
#include <cstring>
#include <stdexcept>
#include <log4cplus/loggingmacros.h>

#include "athena_evm.hpp"
#include "hera.h"

using namespace com::vmware::athena::evm;
using log4cplus::Logger;

// TODO: We may want "release" versions of these macros that print fewer bytes.

/** Hex-format the bytes of an evm_address for Log4Cplus. */
#define LOG4CPLUS_HEXADDR(addr) "0x" << std::hex << std::setw(2) <<            \
   addr->bytes[0] << addr->bytes[1] << addr->bytes[2] << addr->bytes[3] <<     \
   addr->bytes[4] << addr->bytes[5] << addr->bytes[6] << addr->bytes[7] <<     \
   addr->bytes[8] << addr->bytes[9] << addr->bytes[10] << addr->bytes[11] <<   \
   addr->bytes[12] << addr->bytes[13] << addr->bytes[14] << addr->bytes[15] << \
   addr->bytes[16] << addr->bytes[17] << addr->bytes[18] << addr->bytes[19] << \
   std::dec

/** Hex-format the bytes of an evm_uint256be for Log4Cplus. */
#define LOG4CPLUS_HEXVAL(val) "0x" << std::hex << std::setw(2) <<          \
   val->bytes[0] << val->bytes[1] << val->bytes[2] << val->bytes[3] <<     \
   val->bytes[4] << val->bytes[5] << val->bytes[6] << val->bytes[7] <<     \
   val->bytes[8] << val->bytes[9] << val->bytes[10] << val->bytes[11] <<   \
   val->bytes[12] << val->bytes[13] << val->bytes[14] << val->bytes[15] << \
   val->bytes[16] << val->bytes[17] << val->bytes[18] << val->bytes[19] << \
   val->bytes[24] << val->bytes[25] << val->bytes[26] << val->bytes[27] << \
   val->bytes[28] << val->bytes[29] << val->bytes[30] << val->bytes[31] << \
   std::dec

/**
 * Initialize the athena/hera context and start the hera instance.
 *
 * TODO: Make this thread-safe. For now, call this once in main only.
 */
void com::vmware::athena::evm::init_evm()
{
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   // No, these are not good enough. Just getting some sanity set up for first
   // attempts.
   assert(hera == NULL);
   assert(athctx == NULL);

   hera = hera_create();
   if (hera == NULL) {
      LOG4CPLUS_FATAL(logger, "Could not create Hera instance");
      throw EVMException("Could not create Hera instance");
   }

   athctx = (athena_context*)malloc(sizeof(athena_context));
   if (athctx == NULL) {
      LOG4CPLUS_FATAL(logger, "Could not allocate Athena Hera context");
      hera->destroy(hera);
      throw EVMException("Could not allocate Athena Hera context");
   }

   athctx->evmctx = athena_evm_context;
   //TODO: there will be other things to init here (logger, storage, etc.)

   LOG4CPLUS_INFO(logger, "Hera VM started");
}

/**
 * Shutdown the hera instance and destroy the athena context.
 *
 * TODO: Make this thread-safe. For now, call this from one thread only.
 */
void com::vmware::athena::evm::stop_evm() {
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   if (hera != NULL) {
      hera->destroy(hera);
   }

   // TODO: there will be other things to shutdown here (storage, etc.)
   if (athctx != NULL) {
      free(athctx);
   }

   LOG4CPLUS_INFO(logger, "Hera VM stopped");
}

void com::vmware::athena::evm::execute(
   evm_message *message,
   const uint8_t *code,
   size_t code_size,
   evm_result *result) {
   *result = hera->execute(hera, &athctx->evmctx, EVM_BYZANTIUM,
                           message, code, code_size);
}

/**
 * Does the account at the address exists?
 *
 * TODO: is this called for both accounts and contracts?
 *
 * Returns 1 if the account exists, 0 otherwise.
 */
int com::vmware::athena::evm::ath_account_exists(
   struct evm_context* evmctx,
   const struct evm_address* address)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_account_exists called, address: " <<
                  LOG4CPLUS_HEXADDR(address));

   return 1; // all accounts exist for now
};

/**
 * Get the value stored at the given key
 *
 * TODO: what does "not found" look like?
 */
void com::vmware::athena::evm::ath_get_storage(
   struct evm_uint256be* result,
   struct evm_context* evmctx,
   const struct evm_address* address,
   const struct evm_uint256be* key)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_get_storage called, address: " <<
                  LOG4CPLUS_HEXADDR(address) << " key: " <<
                  LOG4CPLUS_HEXVAL(address));

   // TODO: actually look up value, for now just fill with zero
   memset(result, 0, sizeof(*result));
}

/**
 * Set the value stored at the given key
 */
void com::vmware::athena::evm::ath_set_storage(
   struct evm_context* evmctx,
   const struct evm_address* address,
   const struct evm_uint256be* key,
   const struct evm_uint256be* value)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_set_storage called, address: " <<
                  LOG4CPLUS_HEXADDR(address) << " key: " <<
                  LOG4CPLUS_HEXVAL(key));

   // TODO: actually set value
}

/**
 * Get the gas balance for a given account.
 */
void com::vmware::athena::evm::ath_get_balance(
   struct evm_uint256be* result,
   struct evm_context* evmctx,
   const struct evm_address* address)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_get_balance called, address: " <<
                  LOG4CPLUS_HEXADDR(address));

   // TODO: actually look up value, for now just fill with one (to give accounts
   // plenty of gas to maneuver).
   memset(result, 1, sizeof(*result));
}

/**
 * Get the code for the contract at the given address.
 *
 * Returns the size in bytes of the code.
 */
size_t com::vmware::athena::evm::ath_get_code(
   const uint8_t** result_code,
   struct evm_context* evmctx,
   const struct evm_address* address)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_get_code called, address: " <<
                  LOG4CPLUS_HEXADDR(address));

   // TODO: Actually lookup code. For now, say "there is no code" (length = 0).
   return 0;
}

/**
 * Cause a contract to self-destruct.
 */
void com::vmware::athena::evm::ath_selfdestruct(
   struct evm_context* evmctx,
   const struct evm_address* address,
   const struct evm_address* beneficiary)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_selfdestruct called, address: " <<
                  LOG4CPLUS_HEXADDR(address) << " beneficiary: " <<
                  LOG4CPLUS_HEXADDR(beneficiary));

   // TODO: Actually self-destruct contract.
}

/**
 * Log a message.
 */
void com::vmware::athena::evm::ath_emit_log(
   struct evm_context* evmctx,
   const struct evm_address* address,
   const uint8_t* data,
   size_t data_size,
   const struct evm_uint256be topics[],
   size_t topics_count)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_emit_log called, address: " <<
                  LOG4CPLUS_HEXADDR(address));

   // TODO: Actually log the message.
}

/**
 * Handle a call inside a contract.
 */
void com::vmware::athena::evm::ath_call(
   struct evm_result* result,
   struct evm_context* evmctx,
   const struct evm_message* msg)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_call called");

   // TODO: Actually handle the call.
}

/**
 * Get the hash for the block at the given index.
 */
void com::vmware::athena::evm::ath_get_block_hash(
   struct evm_uint256be* result,
   struct evm_context* evmctx,
   int64_t number)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_get_block_hash called, block: " << number);

   // TODO: Actually look up the hash.
}

/**
 * Get the transaction context.
 */
void com::vmware::athena::evm::ath_get_tx_context(
   struct evm_tx_context* result,
   struct evm_context* evmctx)
{
   athena_context *athctx = reinterpret_cast<athena_context*>(evmctx);
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   LOG4CPLUS_INFO(logger, "ath_get_tx_context called");

   // TODO: Actually get the transaction context. For now, set to known value.
   memset(result, 0, sizeof(*result));
}
