// Copyright 2018 VMware, all rights reserved

#include "filter_manager.hpp"


using namespace std;
using namespace com::vmware::athena;
using log4cplus::Logger;

com::vmware::athena::filter_manager::filter_manager(EVM *evm):
   executing_evm(evm),
   logger(Logger::getInstance("com.vmware.athena.filter_manager")){
}

/**
 * creates a new block filter and returns the id corresponding to that filter
 */
evm_uint256be
com::vmware::athena::filter_manager::create_new_block_filter() {
   evm_uint256be &&new_filter = next_filter_id();
   filter_types[new_filter] = Eth_FilterType::NEW_BLOCK_FILTER;
   uint64_t current_block = executing_evm->current_block_number();
   new_block_filters[new_filter] = (current_block > 0) ? current_block - 1 : 0;
   LOG4CPLUS_DEBUG(logger, "Created new block filter with filter ID: " << new_filter);
   return new_filter;
}

/**
 * returns the hash of the blocks that have been mined after the last call
 * to 'getFilterChanges' with this filterId
 */
vector<evm_uint256be>
com::vmware::athena::filter_manager::get_new_block_filter_changes(evm_uint256be filterId) {
   vector<evm_uint256be> ret;
   uint64_t current_block = executing_evm->current_block_number();
   uint64_t new_block_count = current_block - new_block_filters[filterId];
   LOG4CPLUS_DEBUG(logger, "New block filter change request:\n"
                   "current block: " << current_block <<
                   "last update sent: " << new_block_filters[filterId]);
   vector<shared_ptr<EthBlock>> block_list = executing_evm->get_block_list(current_block,
                                                                          new_block_count);
   for (auto it : block_list) {
      ret.push_back(it->hash);
   }
   // update state of this filter
   new_block_filters[filterId] = current_block;
   return ret;
}

evm_uint256be
com::vmware::athena::filter_manager::next_filter_id() {
   // Currently we just return the 'last_filter_id + 1' as the new filter id
   // and increment last_filter_id by 1. However, later on we should change this
   // algorithm to consider the fact that filters can be uninstalled and hence we
   // can reuse the old filter ids
   evm_uint256be id;
   to_evm_uint256be(++last_filter_id, &id);
   return id;
}

Eth_FilterType
com::vmware::athena::filter_manager::get_filter_type(evm_uint256be filterId) {
   if (filter_types.count(filterId) > 0) {
      return filter_types[filterId];
   } else {
      throw FilterNotFoundException("No such filter found!");
   }
}


bool
com::vmware::athena::filter_manager::uninstall_filter(evm_uint256be filterId) {
   new_block_filters.erase(filterId);
   filter_types.erase(filterId);
   return true;
}
