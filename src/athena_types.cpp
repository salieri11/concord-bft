// Copyright 2018 VMware, all rights reserved
//
// Common types passed among Athena components.

#include <string.h>
#include <log4cplus/loggingmacros.h>
#include <keccak.h>

#include "athena_types.hpp"
#include "kvb/slice.h"
#include "athena_storage.pb.h"
#include "common/rlp.hpp"
#include "athena_evm.hpp"
#include "kvb/HexTools.h"

using namespace com::vmware::athena::kvb;

// Byte-wise comparator for evm_uint256be. This allows us to use this type as a
// key in a std::map. Must be in the global namespace.
bool operator<(const evm_uint256be &a, const evm_uint256be &b)
{
   for (int i = 0; i < sizeof(evm_uint256be); ++i) {
      if (a.bytes[i] < b.bytes[i]) {
         return true;
      } else if (a.bytes[i] > b.bytes[i]) {
         return false;
      }
   }

   return false;
}

bool operator!=(const evm_uint256be &a, const evm_uint256be &b)
{
   return !(a == b);
}

bool operator==(const evm_uint256be &a, const evm_uint256be &b)
{
   return memcmp(a.bytes, b.bytes, sizeof(evm_uint256be)) == 0;
}

// Byte-wise comparator for evm_address. This allows us to use this type as a
// key in a std::map. Must be in the global namespace.
bool operator<(const evm_address &a, const evm_address &b)
{
   for (int i = 0; i < sizeof(evm_address); ++i) {
      if (a.bytes[i] < b.bytes[i]) {
         return true;
      } else if (a.bytes[i] > b.bytes[i]) {
         return false;
      }
   }

   return false;
}

bool operator!=(const evm_address &a, const evm_address &b)
{
   return !(a == b);
}

bool operator==(const evm_address &a, const evm_address &b)
{
   return memcmp(a.bytes, b.bytes, sizeof(evm_address)) == 0;
}

/**
 * Compute the hash which will be used to reference the transaction.
 */
evm_uint256be com::vmware::athena::EthTransaction::hash()
{
   /*
    * WARNING: This is not the same as Ethereum's transaction hash right now,
    * but is instead an approximation, in order to provide something to fill API
    * holes. For now, the plan is:
    *
    * RLP([nonce, from, to/contract_address, input])
    */

   RLPBuilder rlpb;
   rlpb.start_list();

   // RLP building is done in reverse order - build flips it for us
   rlpb.add(this->input);
   if (this->contract_address == zero_address) {
      rlpb.add(this->to);
   } else {
      rlpb.add(this->contract_address);
   }
   rlpb.add(this->from);
   rlpb.add(this->nonce);
   std::vector<uint8_t> rlp = rlpb.build();

   // hash it
   return com::vmware::athena::EVM::keccak_hash(rlp);
}

size_t com::vmware::athena::EthTransaction::serialize(char** serialized)
{
   kvb::Transaction out;

   out.set_version(tx_storage_version);
   out.set_block_number(this->block_number);
   out.set_nonce(this->nonce);
   out.set_block_hash(this->block_hash.bytes, sizeof(this->block_hash));
   out.set_from(this->from.bytes, sizeof(this->from));

   if (this->to != zero_address) {
      out.set_to(this->to.bytes, sizeof(this->to));
   }

   if (this->contract_address != zero_address) {
      out.set_contract_address(this->contract_address.bytes,
                               sizeof(this->contract_address));
   }

   if (this->input.size() > 0) {
      out.set_input(std::string(this->input.begin(), this->input.end()));
   }

   out.set_status(this->status);
   out.set_value(this->value);

   size_t size = out.ByteSize();

   *serialized = (char*)malloc(size);
   if (*serialized == NULL) {
      throw new EVMException("Unable to allocate tx serialization");
   }

   out.SerializeToArray(*serialized, size);
   return size;
}

struct com::vmware::athena::EthTransaction
com::vmware::athena::EthTransaction::deserialize(Blockchain::Slice &input)
{
   kvb::Transaction intx;
   intx.ParseFromArray(input.data(), input.size());

   if (intx.version() == tx_storage_version) {
      EthTransaction outtx;

      outtx.block_number = intx.block_number();
      outtx.nonce = intx.nonce();
      std::copy(intx.block_hash().begin(),
                intx.block_hash().end(),
                outtx.block_hash.bytes);

      if (intx.from().size() != sizeof(outtx.from)) {
         LOG4CPLUS_ERROR(log4cplus::Logger::getInstance("com.vmware.athena"),
                         "Invalid address length " << intx.from().size());
         throw new EVMException("Invalid from address length");
      }
      std::copy(intx.from().begin(), intx.from().end(), outtx.from.bytes);

      if (intx.has_to()) {
         //TODO(BWF): check length
         std::copy(intx.to().begin(), intx.to().end(), outtx.to.bytes);
      } else {
         outtx.to = zero_address;
      }

      if (intx.has_contract_address()) {
         //TODO(BWF): check length
         std::copy(intx.contract_address().begin(),
                   intx.contract_address().end(),
                   outtx.contract_address.bytes);
      } else {
         outtx.contract_address = zero_address;
      }

      if (intx.has_input()) {
         std::copy(intx.input().begin(),
                   intx.input().end(),
                   std::back_inserter(outtx.input));
      }

      outtx.status = static_cast<evm_status_code>(intx.status());
      outtx.value = intx.value();

      return outtx;
   } else {
      LOG4CPLUS_ERROR(log4cplus::Logger::getInstance("com.vmware.athena"),
                      "Unknown transaction storage version " << intx.version());
      throw new EVMException("Unkown transaction storage version");
   }
}
