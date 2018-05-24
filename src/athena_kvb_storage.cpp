// Copyright 2018 VMware, all rights reserved
//
// Wrapper around KVB to provide EVM execution storage context. This class
// defines the mapping of EVM object to KVB address. It also records updates to
// be used in minting a block when a transaction finishes.
//
// Initializing a KVBStorage object without an IBlocksAppender causes it to
// operate in read-only mode. A ReadOnlyModeException will be thrown if any of
// the set/add/write functions are called on a KVBStorage object in read-only
// mode.
//
// To add a block, first call the set/add functions to prepare data for the
// block. When all data has been prepared, call `write_block`. A key-value pair
// with the block metadata is added for you. After calling `write_block`, the
// staging area is cleared, and more objects can be prepared for a new block, if
// desired.
//
// After calling set/add/write, a copy of the data has been made, which is
// managed by this object. The original value passed to the set/add/write
// function can be safely destroyed or modified.
//
// KVBlockchain writes a block as a set of key-value pairs. We use the first
// byte of a key to signify the type of the value (see the `TYPE_*` constants in
// `athena_kvb_storage.hpp`. Values are mostly Protocol Buffer encodings,
// defined in `athena_storage.proto`, with the exception being contract data
// (not code). All protobuf messages include a "version" field, so we can handle
// upgrades to storage at a later date.
//
// Storage layouts:
//
// * Block
//   - Key: TYPE_BLOCK+[block hash (32 bytes)]
//   - Value: com::vmware::athena::kvb::Block protobuf
//   - Notes: Do not confuse this with the KVB block. This is Ethereum-level
//            block information.
//
// * Transaction
//   - Key: TYPE_TRANSACTION+[transaction hash (32 bytes)]
//   - Value: com::vmware::athena::kvb::Transaction protobuf
//
// * Account or Contract Balance
//   - Key: TYPE_BALANCE+[account/contract address (20 bytes)]
//   - Value: com::vmware::athena::kvb::Balance protobuf
//   - Notes: Yes, it seems a little overkill to wrap a number in a protobuf
//            encoding, but this saves hassle with endian encoding.
//
// * Contract Code
//   - Key: TYPE_CODE+[contract address (20 bytes)]
//   - Value: com::vmware::athena::kvb::Code protobuf
//
// * Contract Data
//   - Key: TYPE_STORAGE+[contract address (20 bytes)]+[location (32 bytes)]
//   - Value: 32 bytes directly copied from an evm_uint256be
//   - Notes: aka "storage"
//
// * Account Nonce
//   - Key: TYPE_NONCE+[account address (20 bytes)]
//   - Value: com::vmware::athena::kvb::Nonce protobuf
//   - Notes: As with balance, using protobuf solves encoding issues.

#include <vector>
#include <cstring>

#include "athena_kvb_storage.hpp"
#include "athena_exception.hpp"
#include "athena_storage.pb.h"
#include "athena_evm.hpp"
#include "kvb/slice.h"
#include "kvb/BlockchainInterfaces.h"
#include "kvb/HashDefs.h"
#include "kvb/HexTools.h"
#include "evm.h"

using namespace Blockchain;
using namespace com::vmware::athena;

////////////////////////////////////////
// GENERAL

/* Current storage versions. */
const int64_t balance_storage_version = 1;
const int64_t nonce_storage_version = 1;
const int64_t code_storage_version = 1;

// read-only mode
com::vmware::athena::KVBStorage::KVBStorage(
   const Blockchain::ILocalKeyValueStorageReadOnly &roStorage)
   : roStorage_(roStorage),
     blockAppender_(nullptr),
     logger(log4cplus::Logger::getInstance("com.vmware.athena.kvb"))
{
}

// read-write mode
com::vmware::athena::KVBStorage::KVBStorage(
   const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
   Blockchain::IBlocksAppender *blockAppender)
   : roStorage_(roStorage),
     blockAppender_(blockAppender),
     logger(log4cplus::Logger::getInstance("com.vmware.athena.kvb"))
{
}

com::vmware::athena::KVBStorage::~KVBStorage()
{
   // Release the memory used for staging.
   for (auto kvp: updates) {
      delete[] kvp.first.data();
      delete[] kvp.second.data();
   }

   // We don't own the blockAppender we're pointing to, so leave it alone.
}

bool com::vmware::athena::KVBStorage::is_read_only()
{
   // if we don't have a blockAppender, we are read-only
   return !blockAppender_;
}

////////////////////////////////////////
// ADDRESSING

/**
 * Constructs a key: one byte of `type`, concatenated with `length` bytes of
 * `bytes`.
 */
Slice com::vmware::athena::KVBStorage::kvb_key(
   char type, const uint8_t *bytes, size_t length) const
{
   char *key = new char[1+length];
   key[0] = type;
   std::copy(bytes, bytes+length, key+1);
   return Slice(key, length+1);
}

/**
 * Convenience functions for constructing a key for each object type.
 */
Slice com::vmware::athena::KVBStorage::block_key(const EthBlock &blk) const
{
   return kvb_key(TYPE_BLOCK, blk.get_hash().bytes, sizeof(evm_uint256be));
}

Slice com::vmware::athena::KVBStorage::block_key(
   const evm_uint256be &hash) const
{
   return kvb_key(TYPE_BLOCK, hash.bytes, sizeof(hash));
}

Slice com::vmware::athena::KVBStorage::transaction_key(
   const EthTransaction &tx) const
{
   return kvb_key(TYPE_TRANSACTION, tx.hash().bytes, sizeof(evm_uint256be));
}

Slice com::vmware::athena::KVBStorage::transaction_key(
   const evm_uint256be &hash) const
{
   return kvb_key(TYPE_TRANSACTION, hash.bytes, sizeof(hash));
}

Slice com::vmware::athena::KVBStorage::balance_key(
   const evm_address &addr) const
{
   return kvb_key(TYPE_BALANCE, addr.bytes, sizeof(addr));
}

Slice com::vmware::athena::KVBStorage::nonce_key(
   const evm_address &addr) const
{
   return kvb_key(TYPE_NONCE, addr.bytes, sizeof(addr));
}

Slice com::vmware::athena::KVBStorage::code_key(const evm_address &addr) const
{
   return kvb_key(TYPE_CODE, addr.bytes, sizeof(addr));
}

Slice com::vmware::athena::KVBStorage::storage_key(
   const evm_address &addr, const evm_uint256be &location) const
{
   uint8_t combined[sizeof(addr)+sizeof(location)];
   std::copy(addr.bytes, addr.bytes+sizeof(addr), combined);
   std::copy(location.bytes,
             location.bytes+sizeof(location),
             combined+sizeof(addr));
   return kvb_key(TYPE_STORAGE, combined, sizeof(addr)+sizeof(location));
}

////////////////////////////////////////
// WRITING

/**
 * Add a key-value pair to be stored in the block. Throws ReadOnlyModeException
 * if this object is in read-only mode.
 */
void com::vmware::athena::KVBStorage::put(const Slice &key,
                                          const Slice &value)
{
   if (!blockAppender_) {
      throw ReadOnlyModeException();
   }

   KeyValuePair kvp(key, value);
   updates.insert(kvp);
}

/**
 * Add a block to the database, containing all of the key-value pairs that have
 * been prepared. A ReadOnlyModeException will be thrown if this object is in
 * read-only mode.
 */
void com::vmware::athena::KVBStorage::write_block() {
   if (!blockAppender_) {
      throw ReadOnlyModeException();
   }

   // Prepare the block metadata
   EthBlock blk;
   blk.number = next_block_number();

   if (blk.number == 0) {
      blk.parent_hash = zero_hash;
   } else {
      EthBlock parent = get_block(blk.number-1);
      blk.parent_hash = parent.hash;
   }

   for (auto u: updates) {
      if (u.first.data()[0] == TYPE_TRANSACTION) {
         evm_uint256be txhash;
         std::copy(u.first.data()+1,
                   u.first.data()+u.first.size(),
                   txhash.bytes);
         blk.transactions.push_back(txhash);
      }
   }
   blk.hash = blk.get_hash();

   // Add the block metadata to the key-value pair set
   add_block(blk);

   // Actually write the block
   BlockId outBlockId;
   blockAppender_->addBlock(updates, outBlockId);
   LOG4CPLUS_INFO(logger, "Appended block number " << outBlockId);

   // Release all the storage our staging was using
   for (auto kvp: updates) {
      delete[] kvp.first.data();
      delete[] kvp.second.data();
   }

   // Prepare to stage another block
   updates.clear();
}

/**
 * Preparation functions for each value type in a block. These creates
 * serialized versions of the objects and store them in a staging area.
 */
void com::vmware::athena::KVBStorage::add_block(EthBlock &blk)
{
   Slice blkaddr = block_key(blk);
   char *blkser;
   size_t blkser_length = blk.serialize(&blkser);

   put(blkaddr, Slice(blkser, blkser_length));
}

void com::vmware::athena::KVBStorage::add_transaction(EthTransaction &tx)
{
   Slice txaddr = transaction_key(tx);
   char *txser;
   size_t txser_length = tx.serialize(&txser);

   put(txaddr, Slice(txser, txser_length));
}

void com::vmware::athena::KVBStorage::set_balance(const evm_address &addr,
                                                  uint64_t balance)
{
   kvb::Balance proto;
   proto.set_version(balance_storage_version);
   proto.set_balance(balance);
   size_t sersize = proto.ByteSize();
   char *ser = new char[sersize];
   proto.SerializeToArray(ser, sersize);

   put(balance_key(addr), Slice(ser, sersize));
}

void com::vmware::athena::KVBStorage::set_nonce(const evm_address &addr,
                                                uint64_t nonce)
{
   kvb::Nonce proto;
   proto.set_version(nonce_storage_version);
   proto.set_nonce(nonce);
   size_t sersize = proto.ByteSize();
   char *ser = new char[sersize];
   proto.SerializeToArray(ser, sersize);

   put(nonce_key(addr), Slice(ser, sersize));
}

void com::vmware::athena::KVBStorage::set_code(const evm_address &addr,
                                               const uint8_t *code,
                                               size_t code_size)
{
   kvb::Code proto;
   proto.set_version(code_storage_version);
   proto.set_code(code, code_size);
   evm_uint256be hash = EVM::keccak_hash(code, code_size);
   proto.set_hash(hash.bytes, sizeof(hash));

   size_t sersize = proto.ByteSize();
   char *ser = new char[sersize];
   proto.SerializeToArray(ser, sersize);

   put(code_key(addr), Slice(ser, sersize));
}

void com::vmware::athena::KVBStorage::set_storage(
   const evm_address &addr,
   const evm_uint256be &location,
   const evm_uint256be &data)
{
   char *str = new char[sizeof(data)];
   std::copy(data.bytes, data.bytes+sizeof(data), str);
   put(storage_key(addr, location), Slice(str, sizeof(data)));
}

////////////////////////////////////////
// READING

/**
 * Get the number of the block that will be added when write_block is called.
 */
uint64_t com::vmware::athena::KVBStorage::next_block_number() {
   // Ethereum block number is 1+KVB block number. So, the most recent KVB block
   // number is actually the next Ethereum block number.
   return roStorage_.getLastBlock();
}

/**
 * Get the number of the most recent block that was added.
 */
uint64_t com::vmware::athena::KVBStorage::current_block_number() {
   // Ethereum block number is 1+KVB block number. So, the most recent Ethereum
   // block is one less than the most recent KVB block.
   return roStorage_.getLastBlock()-1;
}

/**
 * Get a value from storage. The staging area is searched first, so that it can
 * be used as a sort of current execution environment. If the key is not found
 * in the staging area, its value in the most recent block in which it was
 * written will be returned.
 */
Status com::vmware::athena::KVBStorage::get(const Slice &key, Slice &value)
{
   //TODO(BWF): this search will be very inefficient for a large set of changes
   for (auto u: updates) {
      if (u.first == key) {
         value = u.second;
         return Status::OK();
      }
   }

   return roStorage_.get(key, value);
}

/**
 * Fetch functions for each value type.
 */
EthBlock com::vmware::athena::KVBStorage::get_block(uint64_t number)
{
   SetOfKeyValuePairs outBlockData;

   // "1+" == KVBlockchain starts at block 1, but Ethereum starts at 0
   Status status = roStorage_.getBlockData(1+number, outBlockData);

   LOG4CPLUS_DEBUG(logger, "Getting block number " << number <<
                   " status: " << status.ToString() <<
                   " value.size: " << outBlockData.size());

   if (status.ok()) {
      for (auto kvp: outBlockData) {
         if (kvp.first.data()[0] == TYPE_BLOCK) {
            return EthBlock::deserialize(kvp.second);
         }
      }
   }
   throw BlockNotFoundException();
}

EthBlock com::vmware::athena::KVBStorage::get_block(const evm_uint256be &hash)
{
   Slice kvbkey = block_key(hash);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting block " << hash <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      // TODO: we may store less for block, by using this part to get the number,
      // then get_block(number) to rebuild the transaction list from KV pairs
      return EthBlock::deserialize(value);
   }

   throw BlockNotFoundException();
}

EthTransaction com::vmware::athena::KVBStorage::get_transaction(
   const evm_uint256be &hash)
{
   Slice kvbkey = transaction_key(hash);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting transaction " << hash <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      // TODO: lookup block hash and number as well
      return EthTransaction::deserialize(value);
   }

   throw TransactionNotFoundException();
}

uint64_t com::vmware::athena::KVBStorage::get_balance(const evm_address &addr)
{
   Slice kvbkey = balance_key(addr);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting balance " << addr <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      kvb::Balance balance;
      if (balance.ParseFromArray(value.data(), value.size())) {
         if (balance.version() == balance_storage_version) {
            return balance.balance();
         } else {
            throw EVMException("Unknown balance storage version");
         }
      }
   }

   // untouched accounts have a balance of 0
   return 0;
}

uint64_t com::vmware::athena::KVBStorage::get_nonce(const evm_address &addr)
{
   Slice kvbkey = nonce_key(addr);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting nonce " << addr <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      kvb::Nonce nonce;
      if (nonce.ParseFromArray(value.data(), value.size())) {
         if (nonce.version() == nonce_storage_version) {
            return nonce.nonce();
         } else {
            throw EVMException("Unknown nonce storage version");
         }
      }
   }

   // untouched accounts have a nonce of 0
   return 0;
}

bool com::vmware::athena::KVBStorage::account_exists(const evm_address &addr)
{
   Slice kvbkey = balance_key(addr);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting balance " << addr <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      // if there was a balance recorded, the account exists
      return true;
   }

   return false;
}

/**
 * Code and hash will be copied to `out`, if found, and `true` will be
 * returned. If no code is found, `false` is returned.
 */
bool com::vmware::athena::KVBStorage::get_code(const evm_address &addr,
                                               std::vector<uint8_t> &out,
                                               evm_uint256be &hash)
{
   Slice kvbkey = code_key(addr);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting code " << addr <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      kvb::Code code;
      if (code.ParseFromArray(value.data(), value.size())) {
         if (code.version() == code_storage_version) {
            std::copy(code.code().begin(),
                      code.code().end(),
                      std::back_inserter(out));
            std::copy(code.hash().begin(),
                      code.hash().end(),
                      hash.bytes);
            return true;
         } else {
            LOG4CPLUS_ERROR(logger, "Unknown code storage version" <<
                            code.version());
            throw EVMException("Unkown code storage version");
         }
      } else {
         LOG4CPLUS_ERROR(logger,
                         "Unable to decode storage for contract at " << addr);
         throw EVMException("Corrupt code storage");
      }
   }

   return false;
}

evm_uint256be com::vmware::athena::KVBStorage::get_storage(
   const evm_address &addr, const evm_uint256be &location)
{
   Slice kvbkey = storage_key(addr, location);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting storage " << addr <<
                   " at " << location <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   evm_uint256be out;
   if (status.ok() && value.size() > 0) {
      if (value.size() == sizeof(evm_uint256be)) {
         std::copy(value.data(), value.data()+value.size(), out.bytes);
      } else {
         LOG4CPLUS_ERROR(logger, "Contract " << addr <<
                         " storage " << location <<
                         " only had " << value.size() << " bytes.");
         throw EVMException("Corrupt contract storage");
      }
   } else {
      std::memset(out.bytes, 0, sizeof(out));
   }
   return out;
}
