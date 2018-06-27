// Copyright 2018 VMware, all rights reserved
//
// Ethereum Signature verification.

#ifndef ATHENA_ETH_SIGN_HPP
#define ATHENA_ETH_SIGN_HPP

#include <log4cplus/loggingmacros.h>
#include <secp256k1.h>

namespace com {
namespace vmware {
namespace athena {

class EthSign {
public:
   EthSign();
   ~EthSign();

   evm_address ecrecover(const evm_uint256be hash,
                         const uint8_t version,
                         const evm_uint256be r,
                         const evm_uint256be s) const;

private:
   log4cplus::Logger logger;
   secp256k1_context *ctx;
};

}
}
}


#endif
