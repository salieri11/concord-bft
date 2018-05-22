// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#include <iostream>
#include <cstring>
#include <stdexcept>
#include <memory>
#include <log4cplus/loggingmacros.h>
#include <keccak.h>

#include "athena_evm.hpp"
#include "athena_log.hpp"
#include "athena_types.hpp"
#include "common/rlp.hpp"
#include "kvb/BlockchainInterfaces.h"
#include "kvb/HashDefs.h"
#include "kvb/slice.h"
#include "kvb/HexTools.h"

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
     chainId(params.get_chainID())
{
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
com::vmware::athena::EVM::~EVM()
{
   evminst->destroy(evminst);
   LOG4CPLUS_INFO(logger, "EVM stopped");
}

/**
 * Run a contract, or just transfer value if the destination is not a
 * contract. Calling a contract can either be done with 'call' method or with
 * 'sendTransaction'. Generally pure methods (methods which don't change any
 * state) are called via 'call' method and all others are called via
 * 'sendTransaction' method. The 'sendTransaction' way requires that the
 * transaction is recorded. However for 'call' way there is no transaction to
 * record, it is a simple read storage operation.
 */
void com::vmware::athena::EVM::run(
   evm_message &message,
   const ILocalKeyValueStorageReadOnly &roStorage,
   IBlocksAppender *blockAppender,
   evm_result &result /* out */,
   evm_uint256be &txhash /* out */)
{
   assert(message.kind != EVM_CREATE);

   std::vector<uint8_t> code;
   evm_uint256be hash;
   if (get_code(&message.destination, code, hash)) {
      LOG4CPLUS_DEBUG(logger, "Loaded code from " << message.destination);
      message.code_hash = hash;

      txctx_roStorage = &roStorage;
      txctx_blockAppender = blockAppender;
      execute(message, code, result);
      txctx_roStorage = nullptr;
      txctx_blockAppender = nullptr;
   } else if (message.input_size == 0) {
      LOG4CPLUS_DEBUG(logger, "No code found at " << message.destination);

      uint64_t transfer_val = from_evm_uint256be(&message.value);

      // Don't allow if source account does not exist.
      if (account_exists(&message.sender) == 0) {
         result.status_code = EVM_FAILURE;
         LOG4CPLUS_INFO(logger, "Source account with address "
                        << message.sender << ", does not exist.");
      }

      // Don't allow if source account has insufficient balance.
      else if (balances[message.sender] < transfer_val) {
         result.status_code = EVM_FAILURE;
         LOG4CPLUS_INFO(logger, "Account with address " << message.sender <<
                        ", does not have sufficient funds (" <<
                        balances[message.sender] << ").");
      }

      // Don't allow if destination account does not exist.
      else if (account_exists(&message.destination) == 0) {
         result.status_code = EVM_FAILURE;
         LOG4CPLUS_INFO(logger, "Destination account with address "
                        << message.destination << " does not exist.");
      }
      else {
         balances[message.destination] += transfer_val;
         balances[message.sender] -= transfer_val;
         result.status_code = EVM_SUCCESS;
         LOG4CPLUS_DEBUG(logger, "Transferred  " << transfer_val <<
                         " units to: " << message.destination <<
                         " from: " << message.sender);
      }
   } else {
      LOG4CPLUS_DEBUG(logger, "Input data, but no code at " <<
                      message.destination << ", returning error code.");
      // attempted to run a contract that doesn't exist
      result.status_code = EVM_FAILURE;
   }

   // blockAppender will be null if this is a call instead of a transaction,
   // because calls are not allowed to modify state. Only one transaction will
   // be recorded per execution, so wait for stack to pop to depth zero.
   if (blockAppender && message.depth == 0) {
      txhash = record_transaction(message,
                                  result,
                                  message.destination,
                                  zero_address, /* no contract created */
                                  *blockAppender);
   }
}

/**
 * Create a contract.
 */
void com::vmware::athena::EVM::create(
   evm_message &message,
   const ILocalKeyValueStorageReadOnly &roStorage,
   IBlocksAppender &blockAppender,
   evm_result &result /* out */,
   evm_uint256be &txhash /* out */)
{
   assert(message.kind == EVM_CREATE);
   assert(message.input_size > 0);

   evm_address contract_address = contract_destination(message);

   std::vector<uint8_t> code;
   evm_uint256be hash;
   if (!get_code(contract_address, code, hash)) {
      LOG4CPLUS_DEBUG(logger, "Creating contract at " << contract_address);

      std::vector<uint8_t> create_code =
         std::vector<uint8_t>(message.input_data,
                              message.input_data+message.input_size);
      message.destination = contract_address;

      // we need a hash for this, or evmjit will cache its compilation under
      // something random
      message.code_hash = keccak_hash(create_code);

      txctx_roStorage = &roStorage;
      txctx_blockAppender = &blockAppender;
      execute(message, create_code, result);
      txctx_roStorage = nullptr;
      txctx_blockAppender = nullptr;

      // TODO: check if the new contract is zero bytes in length;
      //       return error, not success in that case
      if (result.status_code == EVM_SUCCESS) {
         LOG4CPLUS_DEBUG(logger, "Contract created at " << contract_address <<
                         " with " << result.output_size << "bytes of code.");

         // store the hash as well, so we don't have to recompute it for every
         // execution
         code = std::vector<uint8_t>(result.output_data,
                                     result.output_data+result.output_size);
         hash = keccak_hash(code);

         contract_code[contract_address] =
            std::pair<std::vector<uint8_t>, evm_uint256be>(code, hash);
         result.create_address = contract_address;
      }
   } else {
      LOG4CPLUS_DEBUG(logger, "Existing code found at " <<
                      message.destination << ", returning error code.");
      // attempted to call a contract that doesn't exist
      result.status_code = EVM_FAILURE;
   }

   // don't expose the address if it wasn't used
   evm_address recorded_contract_address =
      result.status_code == EVM_SUCCESS ? contract_address : zero_address;
   txhash = record_transaction(message, result,
                               zero_address, /* creates are not addressed */
                               recorded_contract_address,
                               blockAppender);
}

/**
 * Compute the hash for the transaction, and put the record in storage.
 */
evm_uint256be com::vmware::athena::EVM::record_transaction(
   const evm_message &message,
   const evm_result &result,
   const evm_address &to_override,
   const evm_address &contract_address,
   IBlocksAppender &blockAppender)
{
   uint64_t nonce = get_nonce(message.sender);
   uint64_t transfer_val = from_evm_uint256be(&message.value);
   EthTransaction tx = {
   nonce : nonce,
   block_hash : zero_hash, // set to zero for now
   // set to zero for now - will be set correctly when block is recorded
   block_number : 0,
   from : message.sender,
   to : to_override,
   contract_address : contract_address,
   input : std::vector<uint8_t>(message.input_data,
                                message.input_data+message.input_size),
   status : result.status_code,
   value : transfer_val
   };

   evm_uint256be txhash = tx.hash();
   LOG4CPLUS_DEBUG(logger, "Recording transaction " << txhash);

   assert(message.depth == 0);
   record_block(tx, blockAppender);

   return txhash;
}

void com::vmware::athena::EVM::record_block(EthTransaction &tx,
                                            IBlocksAppender &blockAppender)
{
   evm_uint256be th = tx.hash();

   // list of transaction hashes for the block - we're doing just one
   // transaciton per block
   std::vector<evm_uint256be> txlist;
   txlist.push_back(th);

   std::shared_ptr<EthBlock> blk = std::make_shared<EthBlock>();
   blk->number = next_block_number();

   if (blk->number-1 == 0) {
      // TODO(BWF): temporary during KVB transition
      blk->parent_hash = zero_hash;
   } else {
      blk->parent_hash = blocks_by_number[blk->number-1]->hash;
   }

   blk->transactions = txlist;
   blk->hash = hash_for_block(blk);

   LOG4CPLUS_DEBUG(logger, "Recording block " << blk->number <<
                   " hash " << blk->hash);
   blocks_by_hash[blk->hash] = blk;
   blocks_by_number[blk->number] = blk;

   tx.block_hash = blk->hash;
   tx.block_number = blk->number;

   char *txkey_arr = new char[sizeof(th)];
   std::copy(th.bytes, th.bytes+sizeof(th), txkey_arr);

   char *txser;
   size_t txser_length = tx.serialize(&txser);

   KeyValuePair kv(Blockchain::Slice(txkey_arr, sizeof(th)),
                   Blockchain::Slice(txser, txser_length));

   // TODO(BWF): `updates` will eventually become a member variable, and it will
   // accumulate all changes made by a transaction (like account value and
   // contract storage)
   SetOfKeyValuePairs updates;
   updates.insert(kv);

   BlockId outBlockId;
   blockAppender.addBlock(updates, outBlockId);
   LOG4CPLUS_INFO(logger, "Appended block number " << outBlockId);

   for (auto kvp: updates) {
      delete[] kvp.first.data();
      delete[] kvp.second.data();
   }
}

/**
 * Get a transaction given its hash.
 */
EthTransaction com::vmware::athena::EVM::get_transaction(
   const evm_uint256be &txhash,
   const ILocalKeyValueStorageReadOnly &roStorage) const
{
   Blockchain::Slice searchKey(reinterpret_cast<const char*>(txhash.bytes),
                               sizeof(txhash));
   Blockchain::BlockId actualVersion;
   Blockchain::Slice value;

   Status status = roStorage.get(searchKey, value);

   LOG4CPLUS_DEBUG(logger, "Getting transaction \'" << txhash << "\'" <<
                   "status: " << status.ToString() <<
                   " key: " << sliceToString(searchKey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      return EthTransaction::deserialize(value);
   }

   throw TransactionNotFoundException();
}

/**
 * Get the value written at the given key in contract storage.
 */
evm_uint256be com::vmware::athena::EVM::get_storage_at(
   const evm_address &account, const evm_uint256be &key) const
{
   evm_uint256be result;
   get_storage(&result, &account, &key);
   return result;
}

/**
 * Get the list of blocks, starting at latest, and going back count-1 steps in
 * the chain.
 */
std::vector<std::shared_ptr<EthBlock>> com::vmware::athena::EVM::get_block_list(
   uint64_t latest,
   uint64_t count,
   const ILocalKeyValueStorageReadOnly &roStorage) const
{
   if (latest > current_block_number()) {
      latest = current_block_number();
   }

   if (count > latest+1) {
      count = latest+1;
   }

   LOG4CPLUS_DEBUG(logger, "Getting block list from " << latest
                   << " to " << (latest-count));

   std::vector<std::shared_ptr<EthBlock>> result;
   for (int i = 0; i < count; i++) {
      if (latest-i == 0) {
         // the genesis block is in KVBlockchain
         SetOfKeyValuePairs outBlockData;
         // "1" == for some reason KVBlockchain starts at block 1 instead of 0
         Status status = roStorage.getBlockData(1, outBlockData);
         if (status.ok()) {
            std::shared_ptr<EthBlock> genesisBlock = std::make_shared<EthBlock>();
            genesisBlock->number = 0;
            genesisBlock->hash = zero_hash;
            genesisBlock->parent_hash = zero_hash;
            for (auto kvp: outBlockData) {
               evm_uint256be txhash;
               std::copy(kvp.first.data(),
                         kvp.first.data()+kvp.first.size(),
                         txhash.bytes);
               genesisBlock->transactions.push_back(txhash);
            }
            result.push_back(genesisBlock);
         }
      } else {
         // TODO(BWF): other blocks are not yet in KVBlockchain
         result.push_back(blocks_by_number.find(latest-i)->second);
      }
   }

   return result;
}

/**
 * Get block at given index.
 */
std::shared_ptr<EthBlock> com::vmware::athena::EVM::get_block_for_number(
   uint64_t number,
   const ILocalKeyValueStorageReadOnly &roStorage) const
{
   if (number == 0) {
      // genesis block is in KVBlockchain
      // the genesis block is in KVBlockchain
      SetOfKeyValuePairs outBlockData;
      // "1" == for some reason KVBlockchain starts at block 1 instead of 0
      Status status = roStorage.getBlockData(1, outBlockData);
      if (status.ok()) {
         std::shared_ptr<EthBlock> genesisBlock = std::make_shared<EthBlock>();
         genesisBlock->number = 0;
         genesisBlock->hash = zero_hash;
         genesisBlock->parent_hash = zero_hash;
         for (auto kvp: outBlockData) {
            evm_uint256be txhash;
            std::copy(kvp.first.data(),
                      kvp.first.data()+kvp.first.size(),
                      txhash.bytes);
            genesisBlock->transactions.push_back(txhash);
         }
         return genesisBlock;
      }
   } else {
      // TODO(BWF): other blocks are not yet in KVBlockchain
      auto iter = blocks_by_number.find(number);
      if (iter != blocks_by_number.end()) {
         return iter->second;
      }
   }

   throw BlockNotFoundException();
}

/**
 * Get block for given hash.
 */
std::shared_ptr<EthBlock> com::vmware::athena::EVM::get_block_for_hash(
   evm_uint256be hash,
   const ILocalKeyValueStorageReadOnly &roStorage) const
{
   if (hash == zero_hash) {
      //genesis block is in KVBlockchain
      SetOfKeyValuePairs outBlockData;
      // "1" == for some reason KVBlockchain starts at block 1 instead of 0
      Status status = roStorage.getBlockData(1, outBlockData);
      if (status.ok()) {
         std::shared_ptr<EthBlock> genesisBlock = std::make_shared<EthBlock>();
         genesisBlock->number = 0;
         genesisBlock->hash = zero_hash;
         genesisBlock->parent_hash = zero_hash;
         for (auto kvp: outBlockData) {
            evm_uint256be txhash;
            std::copy(kvp.first.data(),
                      kvp.first.data()+kvp.first.size(),
                      txhash.bytes);
            genesisBlock->transactions.push_back(txhash);
         }
         return genesisBlock;
      }
   } else {
      // TODO(BWF): other blocks are not yet in KVBlockchain
      auto iter = blocks_by_hash.find(hash);
      if (iter != blocks_by_hash.end()) {
         return iter->second;
      }
   }

   throw BlockNotFoundException();
}

/**
 * Contract destination is the low 20 bytes of the SHA3 hash of the RLP encoding
 * of [sender_address, sender_nonce].
 */
evm_address com::vmware::athena::EVM::contract_destination(
   const evm_message &message)
{
   RLPBuilder rlpb;
   rlpb.start_list();

   // RLP building is done in reverse order - build flips it for us
   rlpb.add(get_nonce(message.sender));
   rlpb.add(message.sender);
   std::vector<uint8_t> rlp = rlpb.build();

   // hash it
   evm_uint256be hash = keccak_hash(rlp);

   // the lower 20 bytes are the address
   evm_address address;
   std::copy(hash.bytes+(sizeof(evm_uint256be)-sizeof(evm_address)),
             hash.bytes+sizeof(evm_uint256be),
             address.bytes);
   return address;
}

/**
 * Compute the hash which will be used to reference the transaction.
 */
evm_uint256be com::vmware::athena::EVM::hash_for_block(
   const std::shared_ptr<EthBlock> blk) const
{
   /*
    * WARNING: This is not the same as Ethereum's block hash right now,
    * but is instead an approximation, in order to provide something to fill API
    * holes. For now, the plan is:
    *
    * RLP([number, parent_hash, [txhash1, txhash2, ...]])
    */

   RLPBuilder rlpb;
   rlpb.start_list();

   rlpb.start_list();
   for (auto txh = blk->transactions.rbegin();
        txh != blk->transactions.rend();
        ++txh) {
      rlpb.add(*txh);
   }
   rlpb.end_list();

   rlpb.add(blk->parent_hash);
   rlpb.add(blk->number);
   std::vector<uint8_t> rlp = rlpb.build();

   // hash it
   return keccak_hash(rlp);
}

/**
 * Creates a new user account with 0 balance.
 * Generates a Keccak256 hash of the passphrase provided by the
 * user and uses its last 20 bytes as the account address.
 */
bool com::vmware::athena::EVM::new_account(
   const std::string& passphrase, evm_address& address)
{
   std::vector<uint8_t> vec(passphrase.begin(), passphrase.end());
   evm_uint256be hash = keccak_hash(vec);

   std::copy(hash.bytes+(sizeof(evm_uint256be)-sizeof(evm_address)),
             hash.bytes+sizeof(evm_uint256be),address.bytes);

   if(EVM::account_exists(&address) == 1) {
      return false;
   } else {
      balances[address] = 0;
      return true;
   }
}

evm_uint256be com::vmware::athena::EVM::keccak_hash(
   const std::vector<uint8_t> &data)
{
   static_assert(sizeof(evm_uint256be) == CryptoPP::Keccak_256::DIGESTSIZE,
                 "hash is not the same size as uint256");

   CryptoPP::Keccak_256 keccak;
   evm_uint256be hash;
   keccak.CalculateDigest(hash.bytes, &data[0], data.size());
   return hash;
}

uint64_t com::vmware::athena::EVM::get_nonce(const evm_address &address)
{
   uint64_t nonce = 1;
   if (nonces.count(address) > 0) {
      nonce = nonces[address];
   }
   nonces[address] = nonce+1;
   return nonce;
}

uint64_t com::vmware::athena::EVM::next_block_number()
{
   return ++latestBlock;
}

uint64_t com::vmware::athena::EVM::current_block_number() const
{
   return latestBlock;
}

void com::vmware::athena::EVM::execute(evm_message &message,
                                       const std::vector<uint8_t> &code,
                                       evm_result &result)
{
   result = evminst->execute(evminst, &athctx.evmctx, EVM_BYZANTIUM,
                             &message, &code[0], code.size());
}


/**
 * Does the account at the address exist?
 *
 * TODO: is this called for both accounts and contracts?
 *
 * Returns 1 if the account exists, 0 otherwise.
 */
int com::vmware::athena::EVM::account_exists(
   const struct evm_address* address)
{
   LOG4CPLUS_INFO(logger, "EVM::account_exists called, address: " <<
                  *address);

   if (balances.count(*address) == 0)
      return 0;

   return 1;
};

/**
 * Construct a key into storage_map, based on the contract address and storage
 * location.
 */
std::vector<uint8_t> com::vmware::athena::EVM::storage_key(
   const struct evm_address* address,
   const struct evm_uint256be* key) const
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
   const struct evm_uint256be* key) const
{
   LOG4CPLUS_DEBUG(logger, "EVM::get_storage called, address: " <<
                   *address << " key: " << *key);

   std::vector<uint8_t> storagekey = storage_key(address, key);
   auto iter = storage_map.find(storagekey);
   if (iter != storage_map.end()) {
      *result = iter->second;
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
                   *address << " key: " << *key << " value: " << *value);

   std::vector<uint8_t> storagekey = storage_key(address, key);

   storage_map[storagekey] = *value;
}

/**
 * Get the gas balance for a given account.
 */
void com::vmware::athena::EVM::get_balance(
   struct evm_uint256be* result,
   const struct evm_address* address)
{
   LOG4CPLUS_INFO(logger, "EVM::get_balance called, address: " << *address);

   if (balances.count(*address))
      to_evm_uint256be(balances[*address], result);
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
   evm_uint256be &hash /* out */)
{
   LOG4CPLUS_INFO(logger, "ath_get_code called, address: " << *address);

   return get_code(*address, code, hash);
}

bool com::vmware::athena::EVM::get_code(
   const evm_address &address,
   std::vector<uint8_t> &code /* out */,
   evm_uint256be &hash /* out */) const
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
                  *address << " beneficiary: " << *beneficiary);

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
   LOG4CPLUS_INFO(logger, "EVM::emit_log called, address: " << *address);

   // TODO: Actually log the message.
}

/**
 * Handle a call inside a contract, EVM callback function
 */
void com::vmware::athena::EVM::call(
   struct evm_result* result,
   const struct evm_message* msg)
{
   LOG4CPLUS_DEBUG(logger, "EVM::call called; depth = " << msg->depth);
   assert(msg->depth > 0);
   evm_uint256be txhash;
   LOG4CPLUS_INFO(logger, msg);
   // create copy of message struct since
   // call function needs non-const message object
   evm_message call_msg = *msg;

   run(call_msg, *txctx_roStorage, txctx_blockAppender, *result, txhash);
}

/**
 * Get the hash for the block at the given index.
 */
void com::vmware::athena::EVM::get_block_hash(
   struct evm_uint256be* result,
   int64_t number)
{
   LOG4CPLUS_DEBUG(logger, "EVM::get_block_hash called, block: " << number);

   auto iter = blocks_by_number.find(number);
   if (iter != blocks_by_number.end()) {
      *result = iter->second->hash;
   } else {
      *result = zero_hash;
   }
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
      evm_uint256be hash;
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
