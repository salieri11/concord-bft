// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#ifndef ATHENA_EVM_PARAMS_HPP
#define ATHENA_EVM_PARAMS_HPP

#include <map>
#include <vector>
#include <log4cplus/loggingmacros.h>
#include <stdexcept>
#include <cstdlib>
#include "common/json.hpp"
#include "common/utils.hpp"
#include "evm.h"
#include "athena_types.hpp"

namespace com {
namespace vmware {
namespace athena {

class EVMInitParamException: public std::exception {
public:
   explicit EVMInitParamException(const std::string &what): msg(what) {};

   virtual const char* what() const noexcept override
   {
      return msg.c_str();
   }

private:
   std::string msg;
};


class EVMInitParams {
public:
   EVMInitParams();
   explicit EVMInitParams(std::string genesis_file_path);
   nlohmann::json parse_genesis_block(std::string genesis_file_path);
   uint64_t parse_timestamp(std::string time_str);
   const std::map<evm_address, uint64_t>& get_initial_accounts() const;
   uint64_t get_chainID() const;
   uint64_t get_timestamp() const;
private:
   // chain ID is 1 by default, if genesis block constructor is
   // used then this chainID will be updated from genesis block.
   static const uint64_t DEFAULT_CHAIN_ID = 8147; // VMware IPO date (8/14/2007)

   uint64_t chainID = DEFAULT_CHAIN_ID;
   uint64_t timestamp = 0;
   // The map of initial accounts with their preset balance values
   std::map<evm_address, uint64_t> initial_accounts;

   log4cplus::Logger logger;
};


}
}
}
#endif //ATHENA_EVM_PARAMS_HPP
