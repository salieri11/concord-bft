// Copyright 2018 VMware, all rights reserved

#include "filter_manager.hpp"
#include "concord_kvb_client.hpp"
#include "concord_types.hpp"

using namespace std;
using namespace com::vmware::concord;
using log4cplus::Logger;

com::vmware::concord::FilterManager::FilterManager():
   logger(Logger::getInstance("com.vmware.concord.FilterManager")){
}

/**
 * creates a new block filter and returns the id corresponding to that filter
 */
evm_uint256be
com::vmware::concord::FilterManager::create_new_block_filter(
   uint64_t current_block)
{
   evm_uint256be new_filter_id = next_filter_id();
   // Note: Currently concord is single-threaded and hence only handles one
   // request at a time. Also, currently in concord we create a new block for
   // every transaction (Basically, there is no delay in block creation like
   // ethereum). Hence, whenever someone makes a new_block_filter request they
   // are probably looking for the last block, hence we set filter block number
   // to (current_block - 1) so when they call 'filter_changes' they can receive
   // the current block. However, this will not work when concord starts handling
   // multiple requests at a time.  TODO: Fix this when concord starts handling
   // multiple connections
   uint64_t last_read_block = (current_block > 0) ? current_block - 1 : 0;
   pair<uint64_t, EthFilterType> new_filter(last_read_block,
                                            EthFilterType::NEW_BLOCK_FILTER);
   // Note: This is an additional hack for geth vs. our zero-delay block
   // creation. web3.js has a bug where the contract-creation monitoring code
   // can't access the filter that its using after the first response to
   // eth_getFilterChanges, if there is nothing else in progress. (The callback
   // is trying to access the return value of the function the callback was
   // passed to, as a captured variable, but the callback may run before the
   // return value is bound.) So, we put new filters in a delay list, and return
   // "no changes" the first time they are queried. During that query, they are
   // moved to the regular list, and they will return changes as expected for
   // subsequent queries.
   geth_delay_filters[new_filter_id] = new_filter;
   LOG4CPLUS_DEBUG(logger,
                   "Created new block filter with filter ID: " << new_filter_id);
   return new_filter_id;
}

/**
 * returns the hash of the blocks that have been mined after the last call
 * to 'getFilterChanges' with this filterId
 */
vector<evm_uint256be>
com::vmware::concord::FilterManager::get_new_block_filter_changes(
   evm_uint256be filterId,
   uint64_t current_block,
   KVBClientPool &clientPool)
{
   vector<evm_uint256be> block_changes;
   if (filters_by_id.count(filterId)) {
      uint64_t new_block_count = current_block - filters_by_id[filterId].first;
      LOG4CPLUS_DEBUG(logger, "New block filter change request:\n"
                      "current block: " << current_block <<
                      "last update sent: " << filters_by_id[filterId].first);

      ConcordRequest request;
      BlockListRequest *blockRequest = request.mutable_block_list_request();
      blockRequest->set_latest(current_block);
      blockRequest->set_count(new_block_count);

      ConcordResponse response;
      if (clientPool.send_request_sync(request, true /* read only */, response)) {
         if (response.has_block_list_response()) {
            BlockListResponse blockResponse = response.block_list_response();

            // TODO: this might not always be true if the filter is far behind
            assert((uint64_t)blockResponse.block_size() == new_block_count);

            // copy all hashes to response
            for (int i = 0; i < blockResponse.block_size(); i++) {
               BlockBrief block = blockResponse.block(i);
               evm_uint256be block_hash;
               std::copy(block.hash().begin(),
                         block.hash().end(),
                         block_hash.bytes);
               block_changes.push_back(block_hash);
            }

            // update last read state of filter
            filters_by_id[filterId].first = current_block;
         } else if (response.error_response_size() > 0) {
            throw FilterException(response.error_response(0).description());
         }
      } else {
         throw FilterException("Error retrieving block list");
      }
   } else if (geth_delay_filters.count(filterId)) {
      // First query against a new filter. Leave response empty, and move it to
      // the regular list to get a proper response next time.
      filters_by_id[filterId] = geth_delay_filters[filterId];
      // remove this filter from delay filter map
      geth_delay_filters.erase(filterId);
   }

   return block_changes;
}

evm_uint256be
com::vmware::concord::FilterManager::next_filter_id() {
   // Currently we just return the 'last_filter_id + 1' as the new filter id
   // and increment last_filter_id by 1. However, later on we should change this
   // algorithm to consider the fact that filters can be uninstalled and hence we
   // can reuse the old filter ids
   evm_uint256be id;
   to_evm_uint256be(++last_filter_id, &id);
   return id;
}

EthFilterType
com::vmware::concord::FilterManager::get_filter_type(evm_uint256be filterId) {
   if (filters_by_id.count(filterId) > 0) {
      return filters_by_id[filterId].second;
   } else if (geth_delay_filters.count(filterId)) {
      return geth_delay_filters[filterId].second;
   } else {
      throw FilterException("No such filter found!");
   }
}


bool
com::vmware::concord::FilterManager::uninstall_filter(evm_uint256be filterId) {
   if (filters_by_id.count(filterId)) {
      filters_by_id.erase(filterId);
   } else {
      geth_delay_filters.erase(filterId);
   }
   return true;
}
