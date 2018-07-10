// Copyright 2018 VMware, all rights reserved

#include <unordered_map>
#include "status_aggregator.hpp"
#include <memory>
#include <queue>
#include <mutex>
#include <thread>
#include <boost/asio.hpp>
#include <boost/thread/thread.hpp>

using namespace std;
using namespace com::vmware::athena;
using namespace boost;
using namespace boost::asio;

struct EnumHash {
   template <typename T>
   size_t operator()(T t) const {
      return static_cast<size_t>(t);
   }
};

typedef unordered_map<PeerInfoType, BasePeerStatus*, EnumHash> STAT_MAP;
typedef STAT_MAP * STAT_MAP_PTR;

typedef unordered_map<int64_t, STAT_MAP_PTR> PEER_STAT_MAP;
typedef PEER_STAT_MAP * PEER_STAT_MAP_PTR;


PEER_STAT_MAP_PTR _pPeerStatusMap = nullptr;
std::mutex _inQueueMutex;
std::shared_ptr<io_service> _pIoService = nullptr;
std::shared_ptr<io_service::work> _pWork = nullptr;
thread_group _threadPool;

const int16_t POOL_SIZE = 1;

StatusAggregator::StatusAggregator() {
   // std::this_thread::sleep_for(std::chrono::seconds(20));
   _pPeerStatusMap = new PEER_STAT_MAP();

   _pIoService = std::shared_ptr<io_service>(new io_service());
   _pWork =
           std::shared_ptr<io_service::work>(
                   new io_service::work(*_pIoService));

   for(auto i = 0; i < POOL_SIZE; i++)
      _threadPool.create_thread(boost::bind(&io_service::run, _pIoService));
}

StatusAggregator::~StatusAggregator() {
   if(_pPeerStatusMap) {
      for(auto it=_pPeerStatusMap->begin();
              it != _pPeerStatusMap->end();
              it++) {
         if(it->second)
            delete it->second;
      }

      delete _pPeerStatusMap;
   }

   _pWork.reset();
   _threadPool.join_all();
   _pIoService->stop();
}

void
update_connectivity_internal(int64_t peerId,
                             string adress,
                             int16_t port,
                             string state) {
   std::lock_guard<std::mutex> lock(_inQueueMutex);
   auto it = _pPeerStatusMap->find(peerId);
   if(_pPeerStatusMap->end() != it) {
      auto status = it->second->find(PeerInfoType::Connectivity);
      if(status != it->second->end()) {
         auto st = static_cast<PeerConnectivityStatus*>(status->second);
         st->peerIp = adress;
         st->peerPort = port;
         st->peerState = state;
      } else {
         auto st = new PeerConnectivityStatus();
         st->peerId = peerId;
         st->peerState = state;
         st->peerPort = port;
         st->peerIp = adress;
         it->second->insert({PeerInfoType::Connectivity, st});
      }
   } else {
      auto pStatMap = new STAT_MAP();
      auto st = new PeerConnectivityStatus();
      st->peerId = peerId;
      st->peerState = state;
      st->peerPort = port;
      st->peerIp = adress;
      pStatMap->insert({PeerInfoType::Connectivity, st});
      _pPeerStatusMap->insert({peerId, pStatMap});
   }
};

void
update_connectivity(int64_t peerId, string adress, int16_t port, string state) {
   _pIoService->post(boost::bind(&update_connectivity_internal,
                        peerId,
                        adress,
                        port,
                        state));
}

UPDATE_CONNECTIVITY_FN
StatusAggregator::get_update_connectivity_fn() {
   return update_connectivity;
}

vector<PeerConnectivityStatus>
StatusAggregator::get_peers_info() {
   std::lock_guard<std::mutex> lock(_inQueueMutex);
   vector<PeerConnectivityStatus> res;
   for (auto it = _pPeerStatusMap->begin();
           it != _pPeerStatusMap->end(); it++ ) {
      auto infoMapIt = it->second->find(PeerInfoType::Connectivity);
      if(infoMapIt != it->second->end()) {
         auto stPtr = static_cast<PeerConnectivityStatus*>(infoMapIt->second);
         res.push_back(*stPtr);
      }
   }

   return res;
}