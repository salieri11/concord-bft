// Copyright 2018 VMware, all rights reserved
//
// Aggregator for status events. Feeded by various modules

#ifndef ATHENA_STATUS_AGGREGATOR_HPP
#define ATHENA_STATUS_AGGREGATOR_HPP

#include <vector>
#include <functional>
#include <string>
#include <memory>
#include "StatusInfo.h"

namespace com {
namespace vmware {
namespace athena {

   class StatusAggregator
   {

   public:
      StatusAggregator();

      std::vector<PeerConnectivityStatus>
      get_peers_info();

      /**
       * this function returns actual method that will be called by low level
      */
      UPDATE_CONNECTIVITY_FN
      get_update_connectivity_fn();
   private:
      class Impl;
      std::shared_ptr<Impl> _pImpl;
   };
}
}
}

#endif //ATHENA_STATUS_AGGREGATOR_HPP