// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#include <iostream>
#include <cstring>
#include <stdexcept>
#include <log4cplus/loggingmacros.h>
#include <keccak.h>


#include "athena_evm.hpp"
#include "athena_log.hpp"
#include "athena_types.hpp"

#ifdef USE_HERA
#include "hera.h"
#else
#include "evmjit.h"
#endif

using namespace com::vmware::athena;
using log4cplus::Logger;

/**
 * Initialize the athena/evm context and start the evm instance.
 */
com::vmware::athena::EVM::EVM(EVMInitParams params)
   : logger(Logger::getInstance("com.vmware.athena.evm")),
     balances(params.get_initial_accounts()),
     chainId(params.get_chainID()) {
   // wrap an evm context in an athena context
   athctx = {{&athena_fn_table}, this};

#ifdef USE_HERA
   evminst = hera_create();
#else
   evminst = evmjit_create();
#endif

   if (!evminst) {
      LOG4CPLUS_FATAL(logger, "Could not create EVM instance");
      throw EVMException("Could not create EVM instance");
   }
   LOG4CPLUS_INFO(logger, "EVM started");
}

/**
 * Shutdown the EVM instance and destroy the athena context.
 */
com::vmware::athena::EVM::~EVM() {
   evminst->destroy(evminst);
   LOG4CPLUS_INFO(logger, "EVM stopped");
}

/**
 * Call a contract, or just transfer value if the destination is not a
 * contract.
 */
void com::vmware::athena::EVM::call(evm_message &message,
                                    evm_result &result /* out */,
                                    std::vector<uint8_t> &txhash /* out */)
{
   assert(message.kind != EVM_CREATE);

   std::vector<uint8_t> code;
   std::vector<uint8_t> hash;
   std::vector<uint8_t> to(message.destination.bytes,
                           message.destination.bytes+sizeof(evm_address));
   std::vector<uint8_t> from(message.sender.bytes,
                             message.sender.bytes+sizeof(evm_address));
   // use empty vector when no contract is created
   std::vector<uint8_t> created_contract_address;
   if (get_code(&message.destination, code, hash)) {
      LOG4CPLUS_DEBUG(logger, "Loaded code from " <<
                      HexPrintAddress{&message.destination});
      memcpy(&message.code_hash.bytes, &hash[0], sizeof(evm_uint256be));

      execute(message, code, result);

      record_transaction(message, result, to, created_contract_address, txhash);
   } else if (message.input_size == 0) {
      LOG4CPLUS_DEBUG(logger, "No code found at " <<
                      HexPrintAddress{&message.destination});

      uint64_t transfer_val = from_evm_uint256be(&message.value);
      // All addresses exist by default. They are considered as accounts with
      // 0 balances. Hence, we never throw an accont not found error. Instead
      // we will simply say that account does not have sufficient balance.
      if (balances.count(from) == 0 || balances[from] < transfer_val) {
         result.status_code = EVM_FAILURE;
         LOG4CPLUS_INFO(logger, "Account with address " <<
                         HexPrintAddress{&message.sender} <<
                        ", does not have sufficient funds (" << balances[from] << ").");
      } else {
         balances[to] += transfer_val;
         balances[from] -= transfer_val;
         result.status_code = EVM_SUCCESS;
         record_transaction(message, result, to, created_contract_address, txhash);
         LOG4CPLUS_DEBUG(logger, "Transferred  " << transfer_val <<
                         " units to: " << HexPrintAddress{&message.destination} <<
                         " from: " << HexPrintAddress{&message.sender});
      }
   } else {
      LOG4CPLUS_DEBUG(logger, "Input data, but no code at " <<
                      HexPrintAddress{&message.destination} <<
                      ", returning error code.");
      // attempted to call a contract that doesn't exist
      result.status_code = EVM_FAILURE;
   }
}

/**
 * Create a contract.
 */
void com::vmware::athena::EVM::create(evm_message &message,
                                      evm_result &result /* out */,
                                      std::vector<uint8_t> &txhash /* out */)
{
   assert(message.kind == EVM_CREATE);
   assert(message.input_size > 0);

   std::vector<uint8_t> contract_address;
   contract_destination(message, contract_address);

   std::vector<uint8_t> code;
   std::vector<uint8_t> hash;
   if (!get_code(contract_address, code, hash)) {
      LOG4CPLUS_DEBUG(logger, "Creating contract at " <<
                      HexPrintVector{contract_address});

      std::vector<uint8_t> create_code =
         std::vector<uint8_t>(message.input_data,
                              message.input_data+message.input_size);
      memcpy(message.destination.bytes, &contract_address[0],
             sizeof(evm_address));

      keccak_hash(create_code, hash);
      memcpy(&message.code_hash.bytes, &hash[0], sizeof(evm_uint256be));

      execute(message, create_code, result);

      // creates are not addressed
      std::vector<uint8_t> to;
      // don't expose the address if it wasn't used
      std::vector<uint8_t> recorded_contract_address =
         result.status_code == EVM_SUCCESS ?
         contract_address : std::vector<uint8_t>();
      record_transaction(message, result, to, recorded_contract_address,
                         txhash);

      if (result.status_code == EVM_SUCCESS) {
         LOG4CPLUS_DEBUG(logger, "Contract created at " <<
                         HexPrintVector{contract_address} <<
                         " with " << result.output_size << "bytes of code.");

         code = std::vector<uint8_t>(result.output_data,
                                     result.output_data+result.output_size);
         keccak_hash(code, hash);

         contract_code[contract_address] =
            std::pair<std::vector<uint8_t>, std::vector<uint8_t>>(code, hash);
         memcpy(result.create_address.bytes, &contract_address[0],
                sizeof(evm_address));
      }
   } else {
      LOG4CPLUS_DEBUG(logger, "Existing code found at " <<
                      HexPrintAddress{&message.destination} <<
                      ", returning error code.");
      // attempted to call a contract that doesn't exist
      result.status_code = EVM_FAILURE;
   }
}

/**
 * Compute the hash for the transaction, and put the record in storage.
 */
void com::vmware::athena::EVM::record_transaction(
   evm_message &message,
   evm_result &result,
   std::vector<uint8_t> &to_override,
   std::vector<uint8_t> &contract_address,
   std::vector<uint8_t> &txhash /* out */)
{
   std::vector<uint8_t> from(message.sender.bytes,
                             message.sender.bytes+sizeof(evm_address));
   uint64_t nonce = get_nonce(from);
   EthTransaction tx{
      nonce, from, to_override, contract_address,
         std::vector<uint8_t>(message.input_data,
                              message.input_data+message.input_size),
         result.status_code
         };

   hash_for_transaction(tx, txhash);
   LOG4CPLUS_DEBUG(logger, "Recording transaction " <<
                   HexPrintVector{txhash});

   transactions[txhash] = tx;
}

/**
 * Get a transaction given its hash.
 */
EthTransaction com::vmware::athena::EVM::get_transaction(
   std::vector<uint8_t> txhash)
{
   return transactions[txhash];
}

/**
 * Get the value written at the given key in contract storage.
 */
std::vector<uint8_t> com::vmware::athena::EVM::get_storage_at(
   std::vector<uint8_t> &account, std::vector<uint8_t> &key)
{
   if (account.size() == sizeof(evm_address) &&
       key.size() == sizeof(evm_uint256be)) {
      evm_uint256be result;
      evm_uint256be location;
      evm_address address;

      memcpy(&address, &account[0], sizeof(evm_address));
      memcpy(&location, &key[0], sizeof(evm_uint256be));

      get_storage(&result, &address, &location);
      return std::vector<uint8_t>(result.bytes,
                                  result.bytes+sizeof(evm_uint256be));
   } else {
      LOG4CPLUS_WARN(logger, "Invalid account (" << account.size() <<
                     " bytes) or key (" << key.size() << " bytes)");
      return std::vector<uint8_t>(sizeof(evm_uint256be), 0);
   }
}

/**
 * Contract destination is the low 20 bytes of the SHA3 hash of the RLP encoding
 * of [sender_address, sender_nonce].
 */
void com::vmware::athena::EVM::contract_destination(
   evm_message &message, std::vector<uint8_t> &address /* out */)
{
   //TODO: write RLP encoding function
   // https://github.com/ethereum/wiki/wiki/RLP

   // allocate min space needed:
   //    1 bytes of nonce (no prefix needed if small enough)
   //  +20 bytes of address
   //  + 1 byte of address prefix
   //  + 1 byte of list prefix
   //  = address length + 3
   std::vector<uint8_t> rlp;
//      std::vector<uint8_t>(sizeof(evm_address)+3);

   // Build RLP encoding backward, then reverse before returning.
   // backward = nonce first, low bits first
   size_t nonce_bytes = 0;
   std::vector<uint8_t>addr_v = std::vector<uint8_t>(
      message.sender.bytes, message.sender.bytes+sizeof(evm_address));
   uint64_t nonce = get_nonce(addr_v);
   if (nonce < 0x80) {
      ++nonce_bytes;
      // very small numbers are represented by themselves
      rlp.push_back(nonce);
   } else {
      do {
         ++nonce_bytes;
         rlp.push_back(nonce & 0xff);
         nonce >>= 8;
      } while(nonce > 0);
      // prefix (0x80 = short string)
      rlp.push_back(0x80 + nonce_bytes);
      // for reference at list prefix
      ++nonce_bytes;
   }

   // now the address
   for (int i = sizeof(evm_address)-1; i >= 0; i--) {
      rlp.push_back(message.sender.bytes[i]);
   }
   // prefix (max 55 bytes for this particular encoding)
   assert(sizeof(evm_address) < 56);
   rlp.push_back(0x80 + sizeof(evm_address));

   // now the list prefix (0xc0 = short list)
   assert(sizeof(evm_address)+1+nonce_bytes < 56);
   rlp.push_back(0xc0 + sizeof(evm_address)+1+nonce_bytes);

   // get the RLP in the correct order
   std::reverse(rlp.begin(), rlp.end());

   // hash it
   std::vector<uint8_t> hash;
   keccak_hash(rlp, hash);

   // the lower 20 bytes are the address
   address.resize(sizeof(evm_address));
   address.assign(hash.begin()+(hash.size()-sizeof(evm_address)), hash.end());
}

/**
 * Compute the hash which will be used to reference the transaction.
 */
void com::vmware::athena::EVM::hash_for_transaction(
   EthTransaction &tx, std::vector<uint8_t> &txhash /* out */)
{
   //TODO: write RLP encoding function
   // https://github.com/ethereum/wiki/wiki/RLP

   /*
    * WARNING: This is not the same as Ethereum's transaction hash right now,
    * but is instead an approximation, in order to provide something to fill API
    * holes. For now, the plan is:
    *
    * RLP([nonce, from, to/contract_address, input])
    */

   std::vector<uint8_t> rlp;
   size_t field_length_count;
   size_t field_length;

   // Build RLP encoding backward, then reverse before returning.
   // backward = input first
   std::reverse_copy(tx.input.begin(), tx.input.end(), std::back_inserter(rlp));
   if (tx.input.size() < 56) {
      rlp.push_back(0x80 + tx.input.size());
   } else {
      field_length = tx.input.size();
      field_length_count = 0;
      do {
         ++field_length_count;
         rlp.push_back(field_length & 0xff);
         field_length >>= 8;
      } while (field_length > 0);
      rlp.push_back(0xb7 + field_length);
   }

   // now the to/contract address
   std::vector<uint8_t> &to = tx.contract_address.size() > 0
      ? tx.contract_address : tx.to;
   std::reverse_copy(to.begin(), to.end(), std::back_inserter(rlp));
   // prefix (max 55 bytes for this particular encoding)
   assert(to.size() < 56);
   rlp.push_back(0x80 + to.size());

   // now the from address
   std::reverse_copy(tx.from.begin(), tx.from.end(), std::back_inserter(rlp));
   // prefix (max 55 bytes for this particular encoding)
   assert(tx.from.size() < 56);
   rlp.push_back(0x80 + tx.from.size());

   // and finally the nonce
   uint64_t nonce = tx.nonce;
   if (nonce < 0x80) {
      // very small numbers are represented by themselves
      rlp.push_back(nonce);
   } else {
      field_length_count = 0;
      do {
         ++field_length_count;
         rlp.push_back(nonce & 0xff);
         nonce >>= 8;
      } while(nonce > 0);
      // prefix (0x80 = short string)
      rlp.push_back(0x80 + field_length_count);
   }

   field_length = rlp.size();
   if (field_length < 56) {
      // 0xc0 = short list
      rlp.push_back(0xc0 + field_length);
   } else {
      field_length_count = 0;
      do {
         ++field_length_count;
         rlp.push_back(field_length & 0xff);
         field_length >>= 8;
      } while(field_length > 0);
      // 0xf7 = long list
      rlp.push_back(0xf7 + field_length_count);
   }

   // get the RLP in the correct order
   std::reverse(rlp.begin(), rlp.end());

   // hash it
   keccak_hash(rlp, txhash);

   assert(txhash.size() == 32);
}

void com::vmware::athena::EVM::keccak_hash(
   std::vector<uint8_t> &data, std::vector<uint8_t> &hash /* out */)
{
   CryptoPP::Keccak_256 keccak;
   int digestSize = keccak.DigestSize();
   hash.resize(digestSize);
   keccak.CalculateDigest(&hash[0], &data[0], data.size());
}

uint64_t com::vmware::athena::EVM::get_nonce(std::vector<uint8_t> &address) {
   uint64_t nonce = 1;
   if (nonces.count(address) > 0) {
      nonce = nonces[address];
   }
   nonces[address] = nonce+1;
   return nonce;
}

void com::vmware::athena::EVM::execute(evm_message &message,
                                            const std::vector<uint8_t> &code,
                                            evm_result &result)
{
   result = evminst->execute(evminst, &athctx.evmctx, EVM_BYZANTIUM,
                             &message, &code[0], code.size());
}


/**
 * Does the account at the address exists?
 *
 * TODO: is this called for both accounts and contracts?
 *
 * Returns 1 if the account exists, 0 otherwise.
 */
int com::vmware::athena::EVM::account_exists(
   const struct evm_address* address)
{
   LOG4CPLUS_INFO(logger, "EVM::account_exists called, address: " <<
                  HexPrintAddress{address});

   return 1; // all accounts exist for now
};

/**
 * Construct a key into storage_map, based on the contract address and storage
 * location.
 */
std::vector<uint8_t> com::vmware::athena::EVM::storage_key(
   const struct evm_address* address,
   const struct evm_uint256be* key)
{
   // we're just using the key appended to the address as the key into our
   // internal storage for now
   std::vector<uint8_t> storagekey(address->bytes,
                                   address->bytes+sizeof(evm_address));
   storagekey.insert(storagekey.end(),
                     key->bytes,
                     key->bytes+sizeof(evm_uint256be));
   return storagekey;
}

/**
 * Get the value stored at the given key. If the key is not found, the value is
 * zeroed out.
 */
void com::vmware::athena::EVM::get_storage(
   struct evm_uint256be* result,
   const struct evm_address* address,
   const struct evm_uint256be* key)
{
   LOG4CPLUS_DEBUG(logger, "EVM::get_storage called, address: " <<
                   HexPrintAddress{address} << " key: " <<
                   HexPrintUint256Be{key});

   std::vector<uint8_t> storagekey = storage_key(address, key);
   if (storage_map.count(storagekey)) {
      std::vector<uint8_t> value = storage_map[storagekey];
      assert(value.size() == sizeof(evm_uint256be));
      memcpy(result, &value[0], sizeof(evm_uint256be));
   } else {
      memset(result, 0, 32);
   }
}

/**
 * Set the value stored at the given key.
 */
void com::vmware::athena::EVM::set_storage(
   const struct evm_address* address,
   const struct evm_uint256be* key,
   const struct evm_uint256be* value)
{
   LOG4CPLUS_DEBUG(logger, "EVM::set_storage called, address: " <<
                   HexPrintAddress{address} << " key: " <<
                   HexPrintUint256Be{key} << " value: " <<
                   HexPrintUint256Be{value});

   std::vector<uint8_t> storagekey = storage_key(address, key);
   std::vector<uint8_t> storagevalue(value->bytes,
                                     value->bytes+sizeof(evm_uint256be));

   storage_map[storagekey] = storagevalue;
}

/**
 * Get the gas balance for a given account.
 */
void com::vmware::athena::EVM::get_balance(
   struct evm_uint256be* result,
   const struct evm_address* address)
{
   LOG4CPLUS_INFO(logger, "EVM::get_balance called, address: " <<
                  HexPrintAddress{address});

   // TODO: actually look up value, for now just fill with one (to give accounts
   // plenty of gas to maneuver).
   std::vector<uint8_t> account(address->bytes, address->bytes+sizeof(evm_address));
   if (balances.count(account))
      to_evm_uint256be(balances[account], result);
   else {
      to_evm_uint256be(0, result);
   }
}

/**
 * Get the code for the contract at the given address.
 *
 * Returns the size in bytes of the code.
 */
bool com::vmware::athena::EVM::get_code(
   const struct evm_address *address,
   std::vector<uint8_t> &code /* out */,
   std::vector<uint8_t> &hash /* out */)
{
   LOG4CPLUS_INFO(logger, "ath_get_code called, address: " <<
                  HexPrintAddress{address});

   std::vector<uint8_t> addr_v =
      std::vector<uint8_t>(address->bytes, address->bytes+sizeof(evm_address));
   return get_code(addr_v, code, hash);
}

bool com::vmware::athena::EVM::get_code(
   const std::vector<uint8_t> &address,
   std::vector<uint8_t> &code /* out */,
   std::vector<uint8_t> &hash /* out */)
{
   // no log here, because this is the internal version
   auto iter = contract_code.find(address);
   if (iter != contract_code.end()) {
      // iter->second == map value
      code = iter->second.first;
      hash = iter->second.second;
      return true;
   }

   return false;
}

/**
 * Cause a contract to self-destruct.
 */
void com::vmware::athena::EVM::selfdestruct(
   const struct evm_address* address,
   const struct evm_address* beneficiary)
{
   LOG4CPLUS_INFO(logger, "ath_selfdestruct called, address: " <<
                  HexPrintAddress{address} << " beneficiary: " <<
                  HexPrintAddress{beneficiary});

   // TODO: Actually self-destruct contract.
}

/**
 * Log a message.
 */
void com::vmware::athena::EVM::emit_log(
   const struct evm_address* address,
   const uint8_t* data,
   size_t data_size,
   const struct evm_uint256be topics[],
   size_t topics_count)
{
   LOG4CPLUS_INFO(logger, "EVM::emit_log called, address: " <<
                  HexPrintAddress{address});

   // TODO: Actually log the message.
}

/**
 * Handle a call inside a contract.
 */
void com::vmware::athena::EVM::call(
   struct evm_result* result,
   const struct evm_message* msg)
{
   LOG4CPLUS_INFO(logger, "EVM::call called");

   // TODO: Actually handle the call.
}

/**
 * Get the hash for the block at the given index.
 */
void com::vmware::athena::EVM::get_block_hash(
   struct evm_uint256be* result,
   int64_t number)
{
   LOG4CPLUS_INFO(logger, "EVM::get_block_hash called, block: " << number);

   // TODO: Actually look up the hash.
}

/**
 * Get the transaction context.
 */
void com::vmware::athena::EVM::get_tx_context(
   struct evm_tx_context* result)
{
   LOG4CPLUS_INFO(logger, "EVM::get_tx_context called");

   // TODO: Actually get the transaction context. For now, set to known value.
   memset(result, 0, sizeof(*result));
}

extern "C" {
/**
 * The next several ath_* functions are callbacks that the EVM uses to interact
 * with our state-keeping layer.
 */

   EVM* ath_object(const struct evm_context* evmctx) {
      return reinterpret_cast<const athena_context*>(evmctx)->ath_object;
   }

   int ath_account_exists(struct evm_context* evmctx,
                          const struct evm_address* address) {
      return ath_object(evmctx)->account_exists(address);
   }
   void ath_get_storage(struct evm_uint256be* result,
                        struct evm_context* evmctx,
                        const struct evm_address* address,
                        const struct evm_uint256be* key) {
      ath_object(evmctx)->get_storage(result, address, key);
   }
   void ath_set_storage(struct evm_context* evmctx,
                        const struct evm_address* address,
                        const struct evm_uint256be* key,
                        const struct evm_uint256be* value) {
      ath_object(evmctx)->set_storage(address, key, value);
   }
   void ath_get_balance(struct evm_uint256be* result,
                        struct evm_context* evmctx,
                        const struct evm_address* address) {
      ath_object(evmctx)->get_balance(result, address);
   }
   size_t ath_get_code_size(struct evm_context* evmctx,
                            const struct evm_address* address) {
      return ath_get_code(nullptr, evmctx, address);
   }
   size_t ath_get_code(const uint8_t** result_code,
                       struct evm_context* evmctx,
                       const struct evm_address* address) {
      std::vector<uint8_t> stored_code;
      std::vector<uint8_t> hash;
      if (ath_object(evmctx)->get_code(address, stored_code, hash)) {
         if (result_code) {
            *result_code = (uint8_t*)malloc(stored_code.size());
            if (*result_code) {
            memcpy(result_code, &stored_code[0], stored_code.size());
            }
         }
         return stored_code.size();
      }
      return 0;
   }
   void ath_selfdestruct(struct evm_context* evmctx,
                         const struct evm_address* address,
                         const struct evm_address* beneficiary) {
      ath_object(evmctx)->selfdestruct(address, beneficiary);
   }
   void ath_emit_log(struct evm_context* evmctx,
                     const struct evm_address* address,
                     const uint8_t* data,
                     size_t data_size,
                     const struct evm_uint256be topics[],
                     size_t topics_count) {
      ath_object(evmctx)->emit_log(address, data, data_size,
                                   topics, topics_count);
   }
   void ath_call(struct evm_result* result,
                 struct evm_context* evmctx,
                 const struct evm_message* msg) {
      ath_object(evmctx)->call(result, msg);
   }
   void ath_get_block_hash(struct evm_uint256be* result,
                           struct evm_context* evmctx,
                           int64_t number) {
      ath_object(evmctx)->get_block_hash(result, number);
   }
   void ath_get_tx_context(struct evm_tx_context* result,
                           struct evm_context* evmctx) {
      ath_object(evmctx)->get_tx_context(result);
   }
}
