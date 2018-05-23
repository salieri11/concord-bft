// Copyright 2018 VMware, all rights reserved
//
// Wrapper around KVB to provide EVM execution storage context. This class
// defines the mapping of EVM object to KVB address. It also records updates to
// be used in minting a block when a transaction finishes.

#include <vector>
#include <cstring>

#include "athena_kvb_storage.hpp"
#include "athena_exception.hpp"
#include "athena_storage.pb.h"
#include "kvb/slice.h"
#include "kvb/BlockchainInterfaces.h"
#include "kvb/HashDefs.h"
#include "kvb/HexTools.h"
#include "evm.h"

using namespace Blockchain;
using namespace com::vmware::athena;

////////////////////////////////////////
// GENERAL

const int64_t balance_storage_version = 1;

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

bool com::vmware::athena::KVBStorage::is_read_only()
{
   // if we don't have a blockAppender, we are read-only
   return !blockAppender_;
}

////////////////////////////////////////
// ADDRESSING

Slice com::vmware::athena::KVBStorage::kvb_key(
   char type, const uint8_t *bytes, size_t length) const
{
   char *key = new char[1+length];
   key[0] = type;
   std::copy(bytes, bytes+length, key+1);
   return Slice(key, length+1);
}

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

void com::vmware::athena::KVBStorage::put(const Slice &key,
                                          const Slice &value)
{
   if (!blockAppender_) {
      throw ReadOnlyModeException();
   }

   KeyValuePair kvp(key, value);
   updates.insert(kvp);
}

void com::vmware::athena::KVBStorage::write_block() {
   if (!blockAppender_) {
      throw ReadOnlyModeException();
   }

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
                   u.first.data()+sizeof(txhash),
                   txhash.bytes);
         blk.transactions.push_back(txhash);
      }
   }
   blk.hash = blk.get_hash();

   add_block(blk);

   BlockId outBlockId;
   blockAppender_->addBlock(updates, outBlockId);
   LOG4CPLUS_INFO(logger, "Appended block number " << outBlockId);

   for (auto kvp: updates) {
      delete[] kvp.first.data();
      delete[] kvp.second.data();
   }

   updates.clear();
}

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

void com::vmware::athena::KVBStorage::set_balance(evm_address &addr,
                                                  uint64_t balance)
{
   kvb::Balance proto;
   proto.set_version(balance_storage_version);
   proto.set_balance(balance);
   std::string str;
   proto.SerializeToString(&str);

   put(balance_key(addr), Slice(str));
}

void com::vmware::athena::KVBStorage::set_code(evm_address &addr,
                                               std::vector<uint8_t> &code)
{
   char *str = new char[code.size()];
   std::copy(code.begin(), code.end(), str);
   put(code_key(addr), Slice(str, code.size()));
}

void com::vmware::athena::KVBStorage::set_storage(evm_address &addr,
                                                  evm_uint256be &location,
                                                  evm_uint256be &data)
{
   char *str = new char[sizeof(data)];
   std::copy(data.bytes, data.bytes+sizeof(data), str);
   put(storage_key(addr, location), Slice(str, sizeof(data)));
}

////////////////////////////////////////
// READING

uint64_t com::vmware::athena::KVBStorage::next_block_number() {
   // Ethereum block number is 1+KVB block number. So, the most recent KVB block
   // number is actually the next Ethereum block number.
   return roStorage_.getLastBlock();
}

uint64_t com::vmware::athena::KVBStorage::current_block_number() {
   // Ethereum block number is 1+KVB block number. So, the most recent Ethereum
   // block is one less than the most recent KVB block.
   return roStorage_.getLastBlock()-1;
}

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

EthBlock com::vmware::athena::KVBStorage::get_block(uint64_t number)
{
   SetOfKeyValuePairs outBlockData;

   // "1" == KVBlockchain starts at block 1 instead of 0
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
      // TODO: we may store less for block; use this part to get the number,
      // then to get_block(number) to rebuild the transaction list from KV pairs
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

   throw AccountNotFoundException();
}

bool com::vmware::athena::KVBStorage::get_code(const evm_address &addr,
                                               std::vector<uint8_t> &out)
{
   Slice kvbkey = code_key(addr);
   Slice value;
   Status status = get(kvbkey, value);

   LOG4CPLUS_DEBUG(logger, "Getting code " << addr <<
                   " status: " << status.ToString() <<
                   " key: " << sliceToString(kvbkey) <<
                   " value.size: " << value.size());

   if (status.ok() && value.size() > 0) {
      std::copy(value.data(),
                value.data()+value.size(),
                std::back_inserter(out));
      return true;
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
      std::copy(value.data(), value.data()+value.size(), out.bytes);
   } else {
      std::memset(out.bytes, 0, sizeof(out));
   }
   return out;
}
