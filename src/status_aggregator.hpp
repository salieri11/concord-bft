// Copyright 2018 VMware, all rights reserved
//
// Aggregator for status events. Feeded by various modules

#ifndef ATHENA_STATUS_AGGREGATOR_HPP
#define ATHENA_STATUS_AGGREGATOR_HPP

#include <vector>
#include <functional>
#include <string>
#include <memory>

namespace com {
namespace vmware {
namespace athena {

   typedef std::function<void(
              int64_t peerId,
              std::string peerAdress,
              int16_t peerPort,
              std::string state)> UPDATE_CONNECTIVITY_FN;

   enum class PeerInfoType
   {
      Connectivity
   };

   struct BasePeerStatus
   {
   public:
      int64_t peerId;
      std::string peerIp;
      int16_t peerPort;
   };

   struct PeerConnectivityStatus : public BasePeerStatus
   {
   public:
      std::string peerState;
   };

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