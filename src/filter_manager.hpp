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
enum class Eth_FilterType {
   LOG_FILTER, // The simple filter to monitor changes to logs
   NEW_BLOCK_FILTER, // Filter for monitoring mining of new blocks
   NEW_PENDING_TRANSACTION_FILTER // filter to monitor pending transactions
};

typedef struct Eth_FilterParams {
   std::string fromBlock; // starting from block
   std::string toBlock; // till block
   std::vector<evm_address> contract_addresses; // list of contracts to look for
   // TODO: Allow array of of array for topics
   // Topics is generally an array of 256 bytes. However, it can
   // also be an array of array of 256 bytes. This vector can not handle that
   std::vector<evm_uint256be> topics;
} Eth_FilterParams;

// forward declaration to break circular references between
// athena_evm.hpp and filter_manager.hpp
class EVM;

class filter_manager {

public:
   filter_manager(EVM *evm);

   Eth_FilterType get_filter_type(evm_uint256be filterId) {
      if (filter_types.count(filterId) > 0) {
         return filter_types[filterId];
      } else {
         throw FilterNotFoundException("No such filter found!");
      }
   }

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

   void uninstall_filter(evm_uint256be filterId);

   evm_uint256be next_filter_id();

private:

   log4cplus::Logger logger;
   const EVM *executing_evm; // Pointer to the evm which is currently executing
   uint64_t last_filter_id = 0;
   std::map<evm_uint256be, Eth_FilterType> filter_types;
   std::map<evm_uint256be, uint64_t> new_block_filters;
   //TODO: add appropriate maps to hold the filters of type
   // pending transaction and log filters
};

}
}
}

#endif // FILTER_MANAGER_HPP
