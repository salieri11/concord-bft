// Copyright 2018 VMware, all rights reserved
//
// Athena Ethereum VM management.

#ifndef ATHENA_EVM_PARAMS_HPP
#define ATHENA_EVM_PARAMS_HPP

#include <map>
#include <vector>
#include <log4cplus/loggingmacros.h>
#include <stdexcept>
#include "common/json.hpp"
#include "common/utils.hpp"
#include "evm.h"

namespace com {
namespace vmware {
namespace athena {

class EVMInitParams {
public:
   EVMInitParams();
   EVMInitParams(std::string genesis_file_path);
   nlohmann::json parse_genesis_block(std::string genesis_file_path);
   std::map<std::vector<uint8_t>, uint64_t> get_initial_accounts();
   uint64_t get_chainID();
private:
   // chain ID is 1 by default, if genesis block constructor is
   // used then this chainID will be updated from genesis block.
   const uint64_t DEFAULT_CHAIN_ID = 1;

   uint64_t chainID = DEFAULT_CHAIN_ID;
   // The map of initial accounts with their preset balance values
   std::map<std::vector<uint8_t>, uint64_t> initial_accounts;
};


}
}
}
#endif //ATHENA_EVM_PARAMS_HPP
