// Copyright 2018 VMware, all rights reserved

#include "filter_manager.hpp"


using namespace std;
using namespace com::vmware::athena;
using log4cplus::Logger;

com::vmware::athena::FilterManager::FilterManager(EVM &evm):
   executing_evm(&evm),
   logger(Logger::getInstance("com.vmware.athena.FilterManager")){
}

/**
 * creates a new block filter and returns the id corresponding to that filter
 */
evm_uint256be
com::vmware::athena::FilterManager::create_new_block_filter() {
   evm_uint256be new_filter_id = next_filter_id();
   uint64_t current_block = executing_evm->current_block_number();
   // Note: Currently athena is single-threaded and hence only handles one
   // request at a time. Also, currently in athena we create a new block for
   // every transaction (Basically, there is no delay in block creation like
   // ethereum). Hence, whenever someone makes a new_block_filter request they
   // are probably looking for the last block, hence we set filter block number
   // to (current_block - 1) so when they call 'filter_changes' they can receive
   // the current block. However, this will not work when athena starts handling
   // multiple requests at a time.  TODO: Fix this when athena starts handling
   // multiple connections
   uint64_t last_read_block = (current_block > 0) ? current_block - 1 : 0;
   pair<uint64_t, EthFilterType> new_filter(last_read_block,
                                            EthFilterType::NEW_BLOCK_FILTER);
   filters_by_id[new_filter_id] = new_filter;
   LOG4CPLUS_DEBUG(logger,
                   "Created new block filter with filter ID: " << new_filter_id);
   return new_filter_id;
}

/**
 * returns the hash of the blocks that have been mined after the last call
 * to 'getFilterChanges' with this filterId
 */
vector<evm_uint256be>
com::vmware::athena::FilterManager::get_new_block_filter_changes(evm_uint256be filterId) {
   vector<evm_uint256be> ret;
   uint64_t current_block = executing_evm->current_block_number();
   uint64_t new_block_count = current_block - filters_by_id[filterId].first;
   LOG4CPLUS_DEBUG(logger, "New block filter change request:\n"
                   "current block: " << current_block <<
                   "last update sent: " << filters_by_id[filterId].first);
   vector<shared_ptr<EthBlock>> block_list =
      executing_evm->get_block_list(current_block,
                                   new_block_count);
   for (auto it : block_list) {
      ret.push_back(it->hash);
   }
   // update state of this filter
   filters_by_id[filterId].first = current_block;
   return ret;
}

evm_uint256be
com::vmware::athena::FilterManager::next_filter_id() {
   // Currently we just return the 'last_filter_id + 1' as the new filter id
   // and increment last_filter_id by 1. However, later on we should change this
   // algorithm to consider the fact that filters can be uninstalled and hence we
   // can reuse the old filter ids
   evm_uint256be id;
   to_evm_uint256be(++last_filter_id, &id);
   return id;
}

EthFilterType
com::vmware::athena::FilterManager::get_filter_type(evm_uint256be filterId) {
   if (filters_by_id.count(filterId) > 0) {
      return filters_by_id[filterId].second;
   } else {
      throw FilterException("No such filter found!");
   }
}


bool
com::vmware::athena::FilterManager::uninstall_filter(evm_uint256be filterId) {
   filters_by_id.erase(filterId);
   return true;
}
