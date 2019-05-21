// Copyright 2018 VMware, all rights reserved

#include "concord_hlf_kvb_storage.hpp"

#include <cstring>
#include <vector>

#include "common/concord_exception.hpp"
#include "concord_storage.pb.h"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "consensus/kvb/HashDefs.h"
#include "consensus/kvb/HexTools.h"
#include "consensus/kvb/sliver.hpp"
#include "utils/concord_eth_hash.hpp"

using namespace Blockchain;
using Blockchain::Sliver;

using concord::common::BlockNotFoundException;
using concord::common::EVMException;
using concord::common::ReadOnlyModeException;

using concord::common::zero_hash;
using concord::common::operator<<;

namespace concord {
namespace blockchain {
namespace hlf {

////////////////////////////////////////
// General

/* Current storage versions. */
const int64_t hlf_state_storage_version = 1;

// read-only mode
KVBHlfStorage::KVBHlfStorage(
    const Blockchain::ILocalKeyValueStorageReadOnly &roStorage)
    : roStorage_(roStorage),
      blockAppender_(nullptr),
      logger(log4cplus::Logger::getInstance("com.vmware.concord.hlf.storage")) {
}

// read-write mode
KVBHlfStorage::KVBHlfStorage(
    const Blockchain::ILocalKeyValueStorageReadOnly &roStorage,
    Blockchain::IBlocksAppender *blockAppender, uint64_t sequenceNum)
    : roStorage_(roStorage),
      blockAppender_(blockAppender),
      logger(log4cplus::Logger::getInstance("com.vmware.concord.hlf.storage")),
      bftSequenceNum_(sequenceNum) {}

KVBHlfStorage::~KVBHlfStorage() {}

bool KVBHlfStorage::is_read_only() {
  // if we don't have a blockAppender, we are read-only
  auto res = blockAppender_ == nullptr;
  return res;
}

/**
 * Allow access to read-only storage object, to enabled downgrades to read-only
 * KVBHlfStorage when convenient.
 */
const Blockchain::ILocalKeyValueStorageReadOnly &
KVBHlfStorage::getReadOnlyStorage() {
  return roStorage_;
}

////////////////////////////////////////
// ADDRESSING

Sliver KVBHlfStorage::kvb_key(uint8_t type, const uint8_t *bytes,
                              size_t length) const {
  uint8_t *key = new uint8_t[1 + length];
  key[0] = type;
  if (length) {
    std::copy(bytes, bytes + length, key + 1);
  }
  return Sliver(key, length + 1);
}

Sliver KVBHlfStorage::hlf_state_key(const uint8_t *key, size_t length) const {
  return kvb_key(TYPE_HLF_STATE, key, length);
}

Sliver KVBHlfStorage::hlf_transaction_key(const uint8_t *key,
                                          size_t length) const {
  return kvb_key(TYPE_HLF_TRANSACTION, key, length);
}

Sliver KVBHlfStorage::hlf_block_key(const uint8_t *key, size_t length) const {
  return kvb_key(TYPE_HLF_BLOCK, key, length);
}

////////////////////////////////////////
// WRITING

/**
 * Add a key-value pair to be stored in the block. Throws ReadOnlyModeException
 * if this object is in read-only mode.
 */
void KVBHlfStorage::put(const Sliver &key, const Sliver &value) {
  if (!blockAppender_) {
    throw ReadOnlyModeException();
  }

  updates[key] = value;
}

/*
 * Add hlf transaction will append newly incoming
 * HLF transaction to queue for later handling.
 */
Status KVBHlfStorage::add_hlf_transaction(
    const com::vmware::concord::HlfRequest &hlfRequest) {
  com::vmware::concord::hlf::storage::HlfTransaction tx;

  tx.set_version(concord::common::tx_storage_version);
  tx.set_chain_id(hlfRequest.chain_id());
  tx.set_chaincode_id(hlfRequest.chaincode_name());
  tx.set_input(hlfRequest.input());
  if (hlfRequest.has_version()) {
    tx.set_chaincode_version(hlfRequest.version());
  } else {
    tx.set_chaincode_version("0");
  }
  pending_hlf_transactions.push_back(tx);

  return Status::OK();
}

com::vmware::concord::hlf::storage::HlfBlock KVBHlfStorage::get_hlf_block(
    uint64_t blockNumber) {
  SetOfKeyValuePairs outBlockData;

  Status status = roStorage_.getBlockData(blockNumber, outBlockData);

  LOG4CPLUS_DEBUG(logger, "Getting block number "
                              << blockNumber << " status: " << status
                              << " value.size: " << outBlockData.size());

  if (status.isOK()) {
    for (auto kvp : outBlockData) {
      if (kvp.first.data()[0] == TYPE_HLF_BLOCK) {
        com::vmware::concord::hlf::storage::HlfBlock hlfBlock;
        hlfBlock.ParseFromArray(kvp.second.data(), kvp.second.length());
        return hlfBlock;
      }
    }
  }
  throw BlockNotFoundException();
}

/*
 * Add block of hlf type to the database, this API should have more
 * functionalities in the future.
 */
Status KVBHlfStorage::write_hlf_block() {
  if (!blockAppender_) {
    throw ReadOnlyModeException();
  }

  com::vmware::concord::hlf::storage::HlfBlock block;
  // hlf block number start from 1
  block.set_number(next_block_number());
  block.set_version(hlf_state_storage_version);

  LOG4CPLUS_INFO(logger,
                 "Get Current Block number: " << current_block_number());
  // it means no block has been committed yet
  if (block.number() == 1) {
    block.set_parent_hash(zero_hash.bytes, sizeof(evm_uint256be));
  } else {
    com::vmware::concord::hlf::storage::HlfBlock parent =
        get_hlf_block(current_block_number());
    block.set_parent_hash(parent.hash());
  }

  // handle transactions
  for (auto tx : pending_hlf_transactions) {
    tx.set_block_id(block.number());
    tx.set_tx_index(0);  // hard code 0
    tx.set_status(0);    // find out how to
                         // verify the status

    // calculate hash according to proto bytes
    size_t size = tx.ByteSize();
    uint8_t *txTarget = new uint8_t[size];
    tx.SerializeToArray(txTarget, size);

    // TODO(lukec) replace the EthHash with the way that
    // HLF calculates the hash
    evm_uint256be txId = concord::utils::eth_hash::keccak_hash(txTarget, size);
    Sliver txAddr = hlf_transaction_key(txId.bytes, sizeof(evm_uint256be));

    put(txAddr, Sliver(txTarget, size));

    block.add_transaction(txId.bytes, sizeof(evm_uint256be));
  }

  size_t size = block.ByteSize();
  uint8_t *blkTarget = new uint8_t[size];

  // set block hash
  block.SerializeToArray(blkTarget, size);

  // calculate hash according to proto bytes
  evm_uint256be blockId =
      concord::utils::eth_hash::keccak_hash(blkTarget, size);
  block.set_hash(blockId.bytes, sizeof(evm_uint256be));

  // key = TYPE_HLF_BLOCK + block hash
  Sliver blockAddr = hlf_block_key(blockId.bytes, sizeof(evm_uint256be));
  put(blockAddr, Sliver(blkTarget, size));

  pending_hlf_transactions.clear();

  // Actually write the block
  BlockId outBlockId;
  Status status = blockAppender_->addBlock(updates, outBlockId);
  if (status.isOK()) {
    LOG4CPLUS_DEBUG(logger, "Appended block number " << outBlockId);
  } else {
    LOG4CPLUS_ERROR(logger, "Failed to append block");
  }

  // Prepare to stage another block
  LOG4CPLUS_DEBUG(logger, "ready to reset");
  reset();
  LOG4CPLUS_DEBUG(logger, "finished the reset");
  return status;
}

/**
 * Drop all pending updates.
 */
void KVBHlfStorage::reset() {
  // Slivers release their memory automatically.
  updates.clear();
}

// HLF extent
void KVBHlfStorage::set_hlf_state(const uint8_t *key, size_t length,
                                  string value) {
  com::vmware::concord::hlf::storage::HlfState proto;
  proto.set_version(hlf_state_storage_version);
  proto.set_state(value);

  size_t sersize = proto.ByteSize();
  uint8_t *ser = new uint8_t[sersize];
  proto.SerializeToArray(ser, sersize);
  put(hlf_state_key(key, length), Sliver(ser, sersize));
}

////////////////////////////////////////
// READING

/**
 * Get the number of the block that will be added when write_block is called.
 */
uint64_t KVBHlfStorage::next_block_number() {
  // HLF block number equals KVB block number.
  return roStorage_.getLastBlock() + 1;
}

/**
 * Get the number of the most recent block that was added.
 */
uint64_t KVBHlfStorage::current_block_number() {
  // HLF block number equals KVB block number.
  return roStorage_.getLastBlock();
}

// HLF extent
string KVBHlfStorage::get_hlf_state(const uint8_t *key, size_t length) {
  uint64_t block_number = current_block_number();
  return get_hlf_state(key, length, block_number);
}

string KVBHlfStorage::get_hlf_state(const uint8_t *key, size_t length,
                                    uint64_t &block_number) {
  Sliver kvbkey = hlf_state_key(key, length);
  Sliver value;
  BlockId outBlock;

  // get concord's lastest block
  Status status = get(block_number, kvbkey, value, outBlock);
  // convert proto binary to string
  LOG4CPLUS_DEBUG(logger, "Getting HLF State by Key:"
                              << key << " looking block at: " << block_number
                              << " status: " << status << " key: " << kvbkey
                              << " value.length: " << value.length()
                              << " out block at: " << outBlock);

  if (status.isOK() && value.length() > 0) {
    com::vmware::concord::hlf::storage::HlfState state;
    if (state.ParseFromArray(value.data(), value.length())) {
      if (state.version() == hlf_state_storage_version) {
        return state.state();
      } else {
        // should use other exception, like HLFException
        throw EVMException("Unknown hlf state version");
      }
    }
  }
  return "";
}

Status KVBHlfStorage::get(const Sliver &key, Sliver &value) {
  uint64_t block_number = current_block_number();
  BlockId out;
  return get(block_number, key, value, out);
}

Status KVBHlfStorage::get(const BlockId readVersion, const Sliver &key,
                          Sliver &value, BlockId &outBlock) {
  // TODO(BWF): this search will be very inefficient for a large set of changes
  for (auto &u : updates) {
    if (u.first == key) {
      value = u.second;
      return Status::OK();
    }
  }
  // HLF block number equals KVB block number
  return roStorage_.get(readVersion, key, value, outBlock);
}

}  // namespace hlf
}  // namespace blockchain
}  // namespace concord
