// Copyright 2018 VMware, all rights reserved
//
// Aggregator for status events. Feeded by various modules

#ifndef ATHENA_STATUS_AGGREGATOR_HPP
#define ATHENA_STATUS_AGGREGATOR_HPP


#include <vector>
#include <functional>
#include <string>
#include "athena.pb.h"

namespace com {
namespace vmware {
namespace athena {

   typedef void(*UPDATE_CONNECTIVITY_FN)
           (int64_t peerId,
            std::string peerAdress,
            int16_t peerPort,
            std::string state);

   enum class PeerInfoType {
      Connectivity
   };

   struct BasePeerStatus {
   public:
      int64_t peerId;
      std::string peerIp;
      int16_t peerPort;
   };

   struct PeerConnectivityStatus : public BasePeerStatus {
   public:
      std::string peerState;
   };

   // use either queue or async call to release the caller ASAP
   // caller is responsible for allocating and Aggregator for deallocating
   // parameters' memory
   class StatusAggregator {

   public:
      StatusAggregator();
      ~StatusAggregator();

      std::vector<PeerConnectivityStatus>
      get_peers_info();

      UPDATE_CONNECTIVITY_FN
      get_update_connectivity_fn();
   };
}
}
}

#endif //ATHENA_STATUS_AGGREGATOR_HPP
