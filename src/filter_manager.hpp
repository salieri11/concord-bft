// Copyright 2018 VMware, all rights reserved
//

#ifndef FILTER_MANAGER_HPP
#define FILTER_MANAGER_HPP

#include <iostream>
#include <fstream>
#include <map>
#include <boost/program_options.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include "evm.h"
#include "common/utils.hpp"
#include "athena_evm.hpp"

namespace com {
namespace vmware {
namespace athena {


class FilterNotFoundException: public std::exception {
public:
   explicit FilterNotFoundException(const std::string &what): msg(what) {};

   virtual const char* what() const noexcept override
   {
      return msg.c_str();
   }

private:
   std::string msg;
};



// enum-type specifying different possible filters from ethereum domain
enum class EthFilterType {
   LOG_FILTER, // The simple filter to monitor changes to logs
   NEW_BLOCK_FILTER, // Filter for monitoring mining of new blocks
   NEW_PENDING_TRANSACTION_FILTER // filter to monitor pending transactions
};

typedef struct EthFilterParams {
   std::string fromBlock; // starting from block
   std::string toBlock; // till block
   std::vector<evm_address> contract_addresses; // list of contracts to look for
   // TODO: Allow array of of array for topics
   // Topics is generally an array of 256 bytes. However, it can
   // also be an array of array of 256 bytes. This vector can not handle that
   std::vector<evm_uint256be> topics;
} EthFilterParams;

// forward declaration to break circular references between
// athena_evm.hpp and filter_manager.hpp
class EVM;

class FilterManager {

public:
   FilterManager(EVM &evm);

   EthFilterType get_filter_type(evm_uint256be filterId);

   evm_uint256be create_new_block_filter();

   std::vector<evm_uint256be>
   get_new_block_filter_changes(evm_uint256be filterId);

   //TODO: figure out proper storage/return types for log filter
   //and implement below methods.
   evm_uint256be create_new_pending_transaction_filter();

   std::vector<evm_uint256be>
   get_new_pending_transaction_filter_changes(evm_uint256be filterId);

   evm_uint256be create_new_filter();

   void
   get_filter_changes(evm_uint256be filterId);
   // End TODO

   bool uninstall_filter(evm_uint256be filterId);

   evm_uint256be next_filter_id();

private:

   log4cplus::Logger logger;
   // Pointer to the evm which is currently executing
   // note: this can not be a reference otherwise due to circular
   // dependency between EVM & FilterManager compiler won't be able to compile
   // these classes.
   const EVM *executing_evm;
   uint64_t last_filter_id = 0;
   std::map<evm_uint256be, std::pair<uint64_t, EthFilterType>> filters_by_id;
   //TODO: add appropriate maps to hold the filters of type
   // pending transaction and log filters
};

}
}
}

#endif // FILTER_MANAGER_HPP
