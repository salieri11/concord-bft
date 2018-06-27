// Copyright 2018 VMware, all rights reserved
//
// Ethereum Signature verification.

#include <log4cplus/loggingmacros.h>
#include <secp256k1_recovery.h>

#include "athena_eth_hash.hpp"
#include "athena_eth_sign.hpp"

// TODO: sort out build structure, to pull this from athena_types
const evm_address zero_address{{0}};

com::vmware::athena::EthSign::EthSign()
   : logger(log4cplus::Logger::getInstance("com.vmware.athena.eth_sign"))
{
   ctx = secp256k1_context_create(SECP256K1_CONTEXT_VERIFY);
}

com::vmware::athena::EthSign::~EthSign()
{
   secp256k1_context_destroy(ctx);
}

/**
 * Recover the "from" address from a transaction signature.
 */
evm_address com::vmware::athena::EthSign::ecrecover(const evm_uint256be hash,
                                                    const uint8_t version,
                                                    const evm_uint256be r,
                                                    const evm_uint256be s) const
{
   std::vector<uint8_t> signature;
   std::copy(r.bytes, r.bytes+sizeof(evm_uint256be),
             std::back_inserter(signature));
   std::copy(s.bytes, s.bytes+sizeof(evm_uint256be),
             std::back_inserter(signature));

   secp256k1_ecdsa_recoverable_signature ecsig;
   if (!secp256k1_ecdsa_recoverable_signature_parse_compact(
          ctx, &ecsig, (unsigned char*)&signature[0], version)) {
      return zero_address;
   }

   secp256k1_pubkey ecpubkey;
   if (!secp256k1_ecdsa_recover(ctx, &ecpubkey, &ecsig, hash.bytes)) {
      return zero_address;
   }

   size_t pubkeysize = 65;
   unsigned char pubkey[65];
   secp256k1_ec_pubkey_serialize(ctx, pubkey, &pubkeysize, &ecpubkey,
                                 SECP256K1_EC_UNCOMPRESSED);

   assert(pubkeysize > 1);
   //skip the version byte at [0]
   evm_uint256be pubkeyhash =
      EthHash::keccak_hash((uint8_t*)(pubkey+1), pubkeysize-1);

   evm_address address;
   std::copy(pubkeyhash.bytes,
             pubkeyhash.bytes+(sizeof(evm_uint256be)-sizeof(evm_address)),
             address.bytes);

   return address;
}

/**
 * Verify a transaction's signature.
 */
// bool com::vmware::athena::EthSign::ecverify(const EthTransaction &tx) const
// {
//    evm_address recoveredAddr = ecrecover(tx.hash(), tx.sig_v, tx.sig_r, tx.sig_s);
//    return recoveredAddr != zero_address && recoveredAddr == tx.from;
// }
