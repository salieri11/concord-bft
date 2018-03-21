// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#include <iostream>
#include <iomanip>
#include <cstring>
#include <stdexcept>
#include <log4cplus/loggingmacros.h>

#include "athena_evm.hpp"

#ifdef USE_HERA
#include "hera.h"
#else
#include "evmjit.h"
#endif

using namespace com::vmware::athena::evm;
using log4cplus::Logger;

/**
 * Initialize the athena/evm context and start the evm instance.
 *
 * TODO: Make this thread-safe. For now, call this once in main only.
 */
void com::vmware::athena::evm::init_evm()
{
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   // No, these are not good enough. Just getting some sanity set up for first
   // attempts.
   assert(evminst == NULL);
   assert(athctx == NULL);

#ifdef USE_HERA
   evminst = hera_create();
#else
   evminst = evmjit_create();
#endif

   if (evminst == NULL) {
      LOG4CPLUS_FATAL(logger, "Could not create EVM instance");
      throw EVMException("Could not create EVM instance");
   }

   athctx = (athena_context*)malloc(sizeof(athena_context));
   if (athctx == NULL) {
      LOG4CPLUS_FATAL(logger, "Could not allocate Athena EVM context");
      evminst->destroy(evminst);
      throw EVMException("Could not allocate Athena EVM context");
   }

   athctx->evmctx = athena_evm_context;

   athctx->codemap = new std::map<std::vector<uint8_t>, std::vector<uint8_t>>;
   assert(athctx->codemap != NULL);

   LOG4CPLUS_INFO(logger, "EVM started");
}

/**
 * Shutdown the EVM instance and destroy the athena context.
 *
 * TODO: Make this thread-safe. For now, call this from one thread only.
 */
void com::vmware::athena::evm::stop_evm()
{
   Logger logger = Logger::getInstance("com.vmware.athena.evm");

   if (evminst != NULL) {
      evminst->destroy(evminst);
   }

   if (athctx != NULL) {
      if (athctx->codemap != NULL) {
         delete athctx->codemap;
      }
      free(athctx);
   }

   LOG4CPLUS_INFO(logger, "EVM stopped");
}

void com::vmware::athena::evm::execute(
   evm_message *message,
   const uint8_t *code,
   size_t code_size,
   evm_result *result)
{
   Logger logger = Logger::getInstance("com.vmware.athena.evm");
   uint8_t *contract_code = NULL;

   // A call with input data but no code expects to find a contract.
   if (message->kind == EVM_CALL && code == NULL && message->input_data != NULL) {
      LOG4CPLUS_DEBUG(logger, "Loading code from " <<
                      LOG4CPLUS_HEXADDR((&message->destination)));
      std::vector<uint8_t> to_addr(message->destination.bytes,
                                   message->destination.bytes+20);
      std::vector<uint8_t> code_v = (*athctx->codemap)[to_addr];


      contract_code = new uint8_t[code_v.size()];
      memcpy(contract_code, &code_v[0], code_v.size());
      code_size = code_v.size();
   }

   LOG4CPLUS_DEBUG(logger, "Executing evm " <<
                   (message->kind == EVM_CREATE ? "create" : "call") <<
                   " with " << code_size << " bytes of code; " <<
                   message->input_size << " bytes of input; gas = " <<
                   message->gas);

   *result = evminst->execute(evminst, &athctx->evmctx, EVM_BYZANTIUM,
                              message,
                              contract_code == NULL ? code : contract_code,
                              code_size);

   if (contract_code != NULL) {
      delete contract_code;
   }

   if (message->kind == EVM_CREATE && result->status_code == EVM_SUCCESS) {
      // result->create_address is not set by the execution above (because it
      // does not include a CREATE opcode.

      //TODO: calculate address: sha3(rlp.encode(sender, nonce))[12:]
      // for now, just use the static address 0x030000..000000 to make testing easy
      std::vector<uint8_t> store_addr_v;
      store_addr_v.resize(20);
      store_addr_v[0] = 0x03;

      std::vector<uint8_t> outcode(result->output_data,
                                   result->output_data + result->output_size);

      (*athctx->codemap)[store_addr_v] = outcode;
   }
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
                  LOG4CPLUS_HEXVAL(key));

   // TODO: actually look up value, for now just fill with zero
   memset(result, 0, 32);
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
                  LOG4CPLUS_HEXVAL(key) << " value: " <<
                  LOG4CPLUS_HEXVAL(value));

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
