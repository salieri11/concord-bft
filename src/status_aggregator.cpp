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

struct EnumHash
{
   template <typename T>
   size_t operator()(T t) const
   {
      return static_cast<size_t>(t);
   }
};

/*
internal structure -Maps PeerInfoType to actual status.currently only last status is saved
can be easily expanded to keep vector<BasePeerStatus>
*/
typedef unordered_map<PeerInfoType, BasePeerStatus*, EnumHash> STAT_MAP;
typedef STAT_MAP * STAT_MAP_PTR;

/*
main data struct, map of maps
maps NodeId (from SBFT) to the map of it's statuses (see above)
the idea is that UI asks for nodes info or specific node info and it can be
easily accessed using this map and internal maps
*/
typedef unordered_map<int64_t, STAT_MAP_PTR> PEER_STAT_MAP;
typedef PEER_STAT_MAP * PEER_STAT_MAP_PTR;


PEER_STAT_MAP_PTR _pPeerStatusMap = nullptr;
std::mutex _inQueueMutex;

// basic thread pool using boost::asio
// the pool will handle all requests from the low level modules
// to update stats in internal structs
const int16_t POOL_SIZE = 1;
std::shared_ptr<io_service> _pIoService = nullptr;
std::shared_ptr<io_service::work> _pWork = nullptr;
thread_group _threadPool;

StatusAggregator::StatusAggregator()
{
   _pPeerStatusMap = new PEER_STAT_MAP();

   _pIoService = std::shared_ptr<io_service>(new io_service());
   _pWork =
           std::shared_ptr<io_service::work>(
                   new io_service::work(*_pIoService));

   for (auto i = 0; i < POOL_SIZE; i++) {
      _threadPool.create_thread(boost::bind(&io_service::run, _pIoService));
   }
}

StatusAggregator::~StatusAggregator()
{
   if(_pWork)
      _pWork.reset();

   _threadPool.join_all();

   if(_pIoService)
      _pIoService->stop();

   if (_pPeerStatusMap) {
      for ( auto it=_pPeerStatusMap->begin();
            it != _pPeerStatusMap->end();
            it++) {
         if (it->second) {
            delete it->second;
         }
      }

      delete _pPeerStatusMap;
   }
}

void
update_connectivity_internal(int64_t peerId,
                             string adress,
                             int16_t port,
                             string state)
{
   std::lock_guard<std::mutex> lock(_inQueueMutex);
   auto it = _pPeerStatusMap->find(peerId);
   if (_pPeerStatusMap->end() != it) {
      auto status = it->second->find(PeerInfoType::Connectivity);
      if (status != it->second->end()) {
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

// this will post the task to thread pool asynchronously
void
update_connectivity_async(int64_t peerId,
                          string adress,
                          int16_t port,
                          string state)
{
   _pIoService->post(boost::bind(&update_connectivity_internal,
                                 peerId,
                                 adress,
                                 port,
                                 state));
}

UPDATE_CONNECTIVITY_FN
StatusAggregator::get_update_connectivity_fn()
{
   return update_connectivity_async;
}

vector<PeerConnectivityStatus>
StatusAggregator::get_peers_info()
{
   std::lock_guard<std::mutex> lock(_inQueueMutex);
   vector<PeerConnectivityStatus> res;
   for ( auto it = _pPeerStatusMap->begin();
         it != _pPeerStatusMap->end();
         it++ ) {
      auto infoMapIt = it->second->find(PeerInfoType::Connectivity);
      if (infoMapIt != it->second->end()) {
         auto stPtr = static_cast<PeerConnectivityStatus*>(infoMapIt->second);
         res.push_back(*stPtr);
      }
   }

   return res;
}