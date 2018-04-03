// Copyright 2018 VMware, all rights reserved
//
// Common types passed among Athena components.

#include "athena_types.hpp"

com::vmware::athena::EthTransaction::EthTransaction(
   const EthTransaction &other)
   : nonce(other.nonce),
     from(other.from),
     to(other.to),
     contract_address(other.contract_address),
     input(other.input),
     status(other.status)
{
}

com::vmware::athena::EthTransaction&
com::vmware::athena::EthTransaction::operator=(
   const EthTransaction &other)
{
   nonce = other.nonce;
   from = other.from;
   to = other.to;
   contract_address = other.contract_address;
   input = other.input;
   status = other.status;
   return *this;
}
