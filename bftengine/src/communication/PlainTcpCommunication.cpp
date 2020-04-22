// Concord
//
// Copyright (c) 2018 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the
// LICENSE file.

/**
 * This file implements the TLS over TCP communication between concord nodes.
 * There are 2 main classes: AsyncTlsConnection - that represents stateful
 * connection between 2 nodes and TlsTCPCommunication - that uses PIMPL idiom
 * to implement the ICommunication interface.
 * The AsyncTlsConnection uses boost::asio::io_service with 1 worker thread -
 * ensuring serial execution of the callbacks. The internal state variables,
 * _closed, _authenticated and _connected, are accessed from the callbacks
 * only - making them thread safe and eliminating need to synchronize the
 * access
 * */

#include "CommDefs.hpp"
#include <string>
#include <functional>
#include <iostream>
#include <sstream>
#include <thread>
#include <chrono>
#include <mutex>
#include <regex>
#include <cassert>
#include <deque>

#include "boost/bind.hpp"
#include <boost/asio.hpp>
#include <boost/make_unique.hpp>
#include <boost/asio/deadline_timer.hpp>
#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <boost/filesystem.hpp>
#include <boost/range.hpp>
#include "Logger.hpp"

using namespace std;
using namespace concordlogger;
using namespace boost;

namespace bftEngine {

class AsyncTcpConnection;

typedef boost::system::error_code B_ERROR_CODE;
typedef std::shared_ptr<AsyncTcpConnection> ASYNC_CONN_PTR;
typedef std::unique_ptr<asio::ip::tcp::socket> B_TCP_SOCKET_PTR;

enum MessageType : uint16_t {
  Reserved = 0,
  Hello,
  Regular
};

enum ConnType : uint8_t {
  NotDefined = 0,
  Incoming,
  Outgoing
};

/**
 * this class will handle single connection using boost::make_shared idiom
 * will receive the IReceiver as a parameter and call it when new message
 * available.
 * The class is responsible for managing connection lifecycle. If the
 * instance represents incoming connection, when the connection is broken the
 * instance will clean up itself by calling the _fOnError method.
 * The Outgoing connection instance will be disposed and the new one
 * will be created when connection is broken
 */
class AsyncTcpConnection : public
                           std::enable_shared_from_this<AsyncTcpConnection> {
 public:
  // since 0 is legal node number, we must initialize it to some "not
  // defined" value. This approach is fragile. TODO:(IG) use real "not
  // defined" value
  static const NodeNum UNKNOWN_NODE_ID = numeric_limits<NodeNum>::max();

 private:

  struct OutMessage {
    char* data = nullptr;
    size_t length = 0;

    OutMessage(char* msg, uint32_t msgLength) :
        data{msg},
        length{msgLength}
    {
    }

    OutMessage& operator=(OutMessage&& other) {
      if (this != &other) {
        if(data) {
          delete[] data;
        }
        data = other.data;
        length = other.length;
        other.data = nullptr;
        other.length = 0;
      }
      return *this;
    }

    OutMessage(OutMessage&& other) : data{nullptr}, length{0} {
      *this = std::move(other);
    };

    OutMessage& operator=(const OutMessage&) = delete;
    OutMessage(const OutMessage& other) = delete;

    ~OutMessage() {
      if(data) {
        delete[] data;
      }
    }
  };

  // msg header: 4 bytes msg length
  static constexpr uint8_t MSGLENGTH_FIELD_SIZE = 4;
  static constexpr uint8_t MSGTYPE_FIELD_SIZE = 2;
  static constexpr uint8_t MSG_HEADER_SIZE =
      MSGLENGTH_FIELD_SIZE + MSGTYPE_FIELD_SIZE;

  // maybe need to define as a function of the input length per operation?
  static constexpr uint32_t WRITE_TIME_OUT_MILLI = 10000;
  static constexpr uint32_t READ_TIME_OUT_MILLI = 10000;

  bool _isReplica = false;
  bool _destIsReplica = false;
  asio::io_service *_service = nullptr;
  uint32_t _maxMessageLength;
  char *_inBuffer;
  IReceiver *_receiver = nullptr;
  std::function<void(NodeNum)> _fOnError = nullptr;
  std::function<void(NodeNum, ASYNC_CONN_PTR)> _fOnHellOMessage = nullptr;
  uint32_t _bufferLength;
  NodeNum _destId;
  NodeNum _selfId;
  string _ip = "";
  uint16_t _port = 0;
  asio::deadline_timer _connectTimer;
  asio::deadline_timer _writeTimer;
  asio::deadline_timer _readTimer;
  ConnType _connType;
  uint16_t _minTimeout = 256;
  uint16_t _maxTimeout = 8192;
  uint16_t _currentTimeout = _minTimeout;
  B_TCP_SOCKET_PTR _socketPtr = nullptr;
  Logger _logger;
  UPDATE_CONNECTIVITY_FN _statusCallback = nullptr;
  NodeMap _nodes;
  deque<OutMessage> _outQueue;
  mutex _writeLock;

  // internal state
  bool _disposed = false;
  bool _hello_received = false;
  bool _connected = false;
 public:

 private:
  AsyncTcpConnection(asio::io_service *service,
                     function<void(NodeNum)> onError,
                     function<void(NodeNum, ASYNC_CONN_PTR)> onHelloReceived,
                     uint32_t bufferLength,
                     NodeNum destId,
                     NodeNum selfId,
                     ConnType type,
                     NodeMap nodes,
                     UPDATE_CONNECTIVITY_FN statusCallback = nullptr) :
      _service(service),
      _maxMessageLength(bufferLength + MSG_HEADER_SIZE + 1),
      _fOnError(onError),
      _fOnHellOMessage(onHelloReceived),
      _bufferLength(bufferLength),
      _destId{destId},
      _selfId(selfId),
      _connectTimer(*service),
      _writeTimer(*service),
      _readTimer(*service),
      _connType(type),
      _socketPtr{B_TCP_SOCKET_PTR(new asio::ip::tcp::socket(*_service))},
      _logger(Log::getLogger("concord-bft.tcp")),
      _statusCallback{statusCallback},
      _nodes{std::move(nodes)},
      _disposed(false),
      _hello_received{false},
      _connected{false} {
    LOG_DEBUG(_logger, "ctor, node " << _selfId
                                     << ", connType: " << _connType);

    _inBuffer = new char[_bufferLength];
    _connectTimer.expires_at(boost::posix_time::pos_infin);
    _writeTimer.expires_at(boost::posix_time::pos_infin);
    _readTimer.expires_at(boost::posix_time::pos_infin);
    _isReplica = check_replica(selfId);
  }

  void init() {

  }

  void set_disposed(bool value) {
    _disposed = value;
  }

  void set_connected(bool value) {
    _connected = value;
  }

  void set_hello_received(bool value) {
    _hello_received = value;
  }

  bool check_replica(NodeNum node) {
    auto it = _nodes.find(node);
    if (it == _nodes.end()) {
      return false;
    }

    return it->second.isReplica;
  }

  /**
   * returns message length - first 4 bytes of the buffer
   * assumes message length header size is 4 bytes
   * @param buffer Data received from the stream
   * @return Message length, 4 bytes long
   */
  uint32_t get_message_length(const char *buffer) {
    return *(reinterpret_cast<const uint32_t*>(buffer));
  }

  void prepare_output_buffer(
      char* buffer, uint16_t msgType, uint32_t dataLength) {
    uint32_t size = sizeof(msgType) + dataLength;
    memcpy(buffer, &size, MSGLENGTH_FIELD_SIZE);
    memcpy(buffer + MSGLENGTH_FIELD_SIZE,
           &msgType,
           MSGTYPE_FIELD_SIZE);
  }

  bool is_service_message() {
    uint16_t msgType =
        *(static_cast<uint16_t *>(static_cast<void *>(_inBuffer)));
    switch (msgType) {
      case MessageType::Hello:
        _destId =
            *(static_cast<NodeNum *>(
                static_cast<void *>(
                    _inBuffer + MSGTYPE_FIELD_SIZE)));

        LOG_DEBUG(_logger, "node: " << _selfId << " got hello from:" << _destId);
        set_hello_received(true);
        _fOnHellOMessage(_destId, shared_from_this());
        _destIsReplica = check_replica(_destId);
        LOG_DEBUG(_logger, "node: " << _selfId
                                    << " dest is replica: " << _destIsReplica);
        return true;
      default:
        return false;
    }
  }

  /// ****************** cleanup functions ******************** ///

  bool was_error(const B_ERROR_CODE &ec, string where) {
    if (ec) {
      LOG_ERROR(_logger,
                "was_error, where: " << where
                                     << ", node " << _selfId
                                     << ", dest: " << _destId
                                     << ", connected: " << _connected
                                     << ", ex: " << ec.message());
    }
    return (ec != 0);
  }

  /**
   * this method closes the socket and frees the object by calling the _fOnError
   * we rely on boost cleanup and do not shutdown ssl and sockets explicitly
   */
  void dispose_connection() {
    if (_disposed)
      return;

    set_hello_received(false);
    set_connected(false);
    set_disposed(true);

    LOG_DEBUG(_logger,
              "dispose_connection, node " << _selfId
                                          << ", dest: " << _destId
                                          << ", connected: " << _connected
                                          << ", closed: " << _disposed);

    _connectTimer.cancel();
    _writeTimer.cancel();
    _readTimer.cancel();

    // We use _expectedDestId here instead of _destId, because _destId may not
    // be set yet, if the connection failed before authentication completes.
    _fOnError(_destId);
  }

  /**
   * generic error handling function
   */
  void handle_error() {
    assert(_connType != ConnType::NotDefined);

    if (_statusCallback) {
      bool isReplica = check_replica(_destId);
      if (isReplica) {
        PeerConnectivityStatus pcs{};
        pcs.peerId = _destId;
        pcs.statusType = StatusType::Broken;

        // pcs.statusTime = we dont set it since it is set by the aggregator
        // in the upcoming version timestamps should be reviewed
        _statusCallback(pcs);
      }
    }

    dispose_connection();
  }
  /// ****************** cleanup functions* end ******************* ///

  /// ************ connect functions ************************** ///

  void send_hello() {
    LOG_DEBUG(_logger, "sending hello from:" << _selfId
                                             << " to: " << _destId
                                             << ", size: " << sizeof(_selfId));
    send((const char*)&_selfId, sizeof(_selfId), MessageType::Hello);
    _fOnHellOMessage(_destId, shared_from_this());
  }

  /**
   * This function sets the time to wait before the next connection attempt.
   * The timeouts move from 256ms to 8s, multiplied by 2, and then stay at 8s.
   * The rationale here is to try to connect fast at the beginning (4
   * connection attempts in 1 sec) and then if failed probably the peer is
   * not ready and we don't want to try at the same rate introducing overhead
   * for Asio. This logic can be changed in future.
   */
  void set_timeout() {
    _currentTimeout = _currentTimeout == _maxTimeout
                      ? _maxTimeout
                      : _currentTimeout * 2;
  }

  /**
   * This is timer tick handler, if we are here either the timer was
   * cancelled and ec == aborted or the timer has reached the deadline.
   * @param ec
   */
  void connect_timer_tick(const B_ERROR_CODE &ec) {
    if (_disposed || ec == asio::error::operation_aborted) {
      return;
    }

    // deadline reached, try to connect
    connect(_ip, _port);
    LOG_DEBUG(_logger, "connect_timer_tick, node " << _selfId
                                                   << ", dest: " << _destId
                                                   << ", ec: " << ec.message());
  }

  /**
   * occures when async connect completes - need to check the socket & timer
   * states to determine timeout or conenection success
   * @param err
   */
  void connect_completed(const B_ERROR_CODE &err) {
    if (_disposed) {
      return;
    }

    auto res = was_error(err, __func__);

    if (res) {
      // async_connect opens socket on start so we need to close the socket if
      // not connected
      get_socket().close();
      set_connected(false);
      set_timeout();
      _connectTimer.expires_from_now(
          boost::posix_time::millisec(_currentTimeout));
      _connectTimer.async_wait(
          boost::bind(&AsyncTcpConnection::connect_timer_tick,
                      shared_from_this(),
                      boost::asio::placeholders::error));
    } else {
      set_connected(true);
      _connectTimer.cancel();
      LOG_DEBUG(_logger, "connected, node " << _selfId
                                            << ", dest: " << _destId
                                            << ", res: " << res);
      send_hello();
      read_msg_length_async();
    }

    LOG_TRACE(_logger, "exit, node " << _selfId << ", dest: " << _destId);
  }

  /// ************ connect functions end ************************** ///

  /// ************* read functions ******************* ////
  /// the read process is as follows:
  /// 1. read_some of MSG_HEADER_SIZE bytes without starting timer. This
  /// is async call and the callback can be triggered after partial read
  /// 2. start async_read of either the rest part of the header (if was
  /// partial) or the message itself. Start timer for handling the timeout.
  /// 3. when read completed, cancel the timer
  /// 4. if timer ticks - the read hasnt completed, close the connection.

  void on_read_timer_expired(const B_ERROR_CODE &ec) {
    if(_disposed) {
      return;
    }
    // check if the handle is not a result of calling expire_at()
    if(ec != boost::asio::error::operation_aborted) {
      dispose_connection();
    }
  }

  /**
  * occurs when some of msg length bytes are read from the stream
  * @param ec Error code
  * @param bytesRead actual bytes read
  */
  void
  read_msglength_completed(const B_ERROR_CODE &ec,
                           const uint32_t bytesRead,
                           bool first) {
    // if first is true - we came from partial reading, no timer was started
    if(!first) {
      auto res = _readTimer.expires_at(boost::posix_time::pos_infin);
      assert(res < 2); //can cancel at most 1 pending async_wait
    }
    if (_disposed) {
      return;
    }

    auto err = was_error(ec, __func__);
    if (err) {
      handle_error();
      return;
    }

    // if partial read of msg length bytes, continue
    if(first && bytesRead < MSGLENGTH_FIELD_SIZE) {
      asio::async_read(
          *_socketPtr,
          asio::buffer(_inBuffer + bytesRead, MSGLENGTH_FIELD_SIZE - bytesRead),
          boost::bind(&AsyncTcpConnection::read_msglength_completed,
                      shared_from_this(),
                      boost::asio::placeholders::error,
                      boost::asio::placeholders::bytes_transferred,
                      false));
    } else { // start reading completely the whole message
      uint32_t msgLength = get_message_length(_inBuffer);
      if(msgLength == 0 || msgLength > _maxMessageLength - 1 - MSG_HEADER_SIZE){
        handle_error();
        return;
      }
      read_msg_async(msgLength);
    }

    auto res = _readTimer.expires_from_now(
        boost::posix_time::milliseconds(READ_TIME_OUT_MILLI));
    assert(res == 0); //can cancel at most 1 pending async_wait
    _readTimer.async_wait(
        boost::bind(&AsyncTcpConnection::on_read_timer_expired,
                    shared_from_this(),
                    boost::asio::placeholders::error));

    LOG_DEBUG(_logger, "exit, node " << _selfId
                                     << ", dest: " << _destId
                                     << ", connected: " << _connected
                                     << ", is_open: " << get_socket().is_open());
  }

  /**
   * start reading message length bytes from the stream
   */
  void read_msg_length_async() {
    if (_disposed)
      return;

    // since we allow partial reading here, we dont need timeout
    _socketPtr->async_read_some(
        asio::buffer(_inBuffer, MSGLENGTH_FIELD_SIZE),
        boost::bind(&AsyncTcpConnection::read_msglength_completed,
                    shared_from_this(),
                    boost::asio::placeholders::error,
                    boost::asio::placeholders::bytes_transferred,
                    true));

    LOG_DEBUG(_logger,
              "read_msg_length_async, node " << _selfId
                                             << ", dest: " << _destId
                                             << ", connected: " << _connected
                                             << "is_open: " << get_socket().is_open());
  }

  /**
   * occurs when the whole message has been read from the stream
   * @param ec error code
   * @param bytesRead  actual bytes read
   */
  void read_msg_async_completed(const boost::system::error_code &ec,
                                size_t bytesRead) {
    auto res = _readTimer.expires_at(boost::posix_time::pos_infin);
    assert(res < 2); //can cancel at most 1 pending async_wait
    if (_disposed) {
      return;
    }

    if (_disposed) {
      return;
    }

    auto err = was_error(ec, __func__);
    if (err) {
      handle_error();
      return;
    }

    if (!is_service_message()) {
      LOG_DEBUG(_logger, "data msg received, msgLen: " << bytesRead);
      if(_receiver) {
        _receiver->onNewMessage(_destId,
                                _inBuffer + MSGTYPE_FIELD_SIZE,
                                bytesRead - MSGTYPE_FIELD_SIZE);
      }
    }

    read_msg_length_async();

    if (_statusCallback && _destIsReplica) {
      PeerConnectivityStatus pcs{};
      pcs.peerId = _destId;
      pcs.statusType = StatusType::MessageReceived;

      // pcs.statusTime = we dont set it since it is set by the aggregator
      // in the upcoming version timestamps should be reviewed
      _statusCallback(pcs);
    }

    LOG_TRACE(_logger, "exit, node " << _selfId << ", dest: " << _destId);
  }

  /**
   * start reading message bytes after the length header has been read
   * @param msgLength
   */
  void read_msg_async(uint32_t msgLength) {
    if (_disposed) {
      return;
    }

    LOG_DEBUG(_logger, "read_msg_async, node " << _selfId << ", dest: " <<
                                               _destId);

    // async operation will finish when either expectedBytes are read
    // or error occured, this is what Asio guarantees
    async_read(*_socketPtr,
               boost::asio::buffer(_inBuffer, msgLength),
               boost::bind(&AsyncTcpConnection::read_msg_async_completed,
                           shared_from_this(),
                           boost::asio::placeholders::error,
                           boost::asio::placeholders::bytes_transferred));

  }

  /// ************* read functions end ******************* ////

  /// ************* write functions ******************* ////
  /// the write process works as follows:
  /// 1. the send() function copies the data to the outgoing queue and calls
  /// post() - because the async_write_* should be called from one of the asio's
  /// worker threads.
  /// 2. the post callback checks if there is no pending write (queue size is 0
  /// and if true start asycn_write, with timer enabled
  /// 3. when write completed, cancels the timer. check if more messages
  /// are in the out queue - if true, start another async_write with timer.
  /// 4. if timer ticks - the write hasn't completed, close the connection.


  /**
   * If the timer tick occurs - shut down the connection.
   * Probably we need to find the better way to handle timeouts but the
   * current implementation assumes long enough timeouts to allow to write data
   * So if the timer occurs before the async write completes - we have a problem
   * @param ec Error code
   */
  void on_write_timer_expired(const B_ERROR_CODE &ec) {
    if(_disposed) {
      return;
    }
    // check if we the handle is not a result of calling expire_at()
    if(ec != boost::asio::error::operation_aborted) {
      dispose_connection();
    }
  }

  void  start_async_write() {
    asio::async_write(
        *_socketPtr,
        asio::buffer(_outQueue.front().data, _outQueue.front().length),
        boost::bind(
            &AsyncTcpConnection::async_write_complete,
            shared_from_this(),
            boost::asio::placeholders::error,
            boost::asio::placeholders::bytes_transferred));

    // start the timer to handle the write timeout
    auto res = _writeTimer.expires_from_now(
        boost::posix_time::milliseconds(WRITE_TIME_OUT_MILLI));
    assert(res == 0); //should not cancel any pending async wait
    _writeTimer.async_wait(
        boost::bind(&AsyncTcpConnection::on_write_timer_expired,
                    shared_from_this(),
                    boost::asio::placeholders::error));
  }

  /**
   * completion callback for the async write operation
   */
  void async_write_complete(const B_ERROR_CODE &ec, size_t bytesWritten) {
    auto res = _writeTimer.expires_at(boost::posix_time::pos_infin);
    assert(res < 2); //can cancel at most 1 pending async_wait
    if(_disposed) {
      return;
    }
    bool err = was_error(ec, "async_write_complete");
    if(err) {
      dispose_connection();
      return;
    }

    lock_guard<mutex> l(_writeLock);
    //remove the message that has been sent
    _outQueue.pop_front();

    // if there are more messages, continue to send but don' renmove, s.t.
    // the send() method will not trigger concurrent write
    if(_outQueue.size() > 0) {
      start_async_write();
    }
  }

  /**
   * start the async write operation
   * @param data
   * @param length
   */
  void do_write() {
    if (_disposed) {
      return;
    }

    //
    lock_guard<mutex> l(_writeLock);
    if(_outQueue.size() > 0) {
      start_async_write();
    }
  }

  /// ************* write functions end ******************* ////

 public:
  asio::ip::tcp::socket& get_socket() {
    return *_socketPtr;
  }

  /**
   * start connection to the remote peer (Outgoing connection)
   * @param ip remote IP
   * @param port remote port
   * @param isReplica whether the peer is replica or client
   */
  void connect(string ip, uint16_t port) {
    _ip = ip;
    _port = port;

    asio::ip::tcp::endpoint ep(asio::ip::address::from_string(ip), port);

    get_socket().
        async_connect(ep,
                      boost::bind(&AsyncTcpConnection::connect_completed,
                                  shared_from_this(),
                                  boost::asio::placeholders::error));
    LOG_TRACE(_logger, "exit, from: " << _selfId
                                      << " ,to: " << _destId
                                      << ", ip: " << ip
                                      << ", port: " << port);
  }

  void start() {
    read_msg_length_async();
  }

  void send(const char *data, uint32_t length) {
    send(data, length, MessageType::Regular);
  }

  /**
   * mimics the async sending by using Post to Asio working thread
   * this function posts the send request to the asio io service and then it
   * is executed in the worker thread.
   * @param data data to be sent
   * @param length data length
   */
  void send(const char *data, uint32_t length, MessageType type) {
    assert(data);
    assert(length > 0 && length <= _maxMessageLength - MSG_HEADER_SIZE);

    char *buf = new char[length + MSG_HEADER_SIZE];
    memset(buf, 0, length + MSG_HEADER_SIZE);
    prepare_output_buffer(buf, type, length);
    memcpy(buf + MSG_HEADER_SIZE, data, length);

    // here we lock to protect multiple thread access and to synch with callback
    // queue access
    lock_guard<mutex> l(_writeLock);

    // push to the output queue
    OutMessage out = OutMessage(buf, length + MSG_HEADER_SIZE);
    _outQueue.push_back(std::move(out));

    // if there is only one message in the queue there are no pending writes
    // - we can start one
    // we must post to asio service because async operations should be
    // started from asio threads and not during pending async read
    if(_outQueue.size() == 1) {
      _service->post(boost::bind(&AsyncTcpConnection::do_write,
                                 shared_from_this()));
    }

    LOG_DEBUG(_logger, "from: " << _selfId
                                << ", to: " << _destId
                                << ", length: " << length);

    if (_statusCallback && _isReplica) {
      PeerConnectivityStatus pcs{};
      pcs.peerId = _selfId;
      pcs.statusType = StatusType::MessageSent;

      // pcs.statusTime = we dont set it since it is set by the aggregator
      // in the upcoming version timestamps should be reviewed
      _statusCallback(pcs);
    }
  }

  void setReceiver(NodeNum nodeId, IReceiver *rec) {
    _receiver = rec;
  }

  static ASYNC_CONN_PTR create(asio::io_service *service,
                               function<void(NodeNum)> onError,
                               function<void(NodeNum, ASYNC_CONN_PTR)> onReady,
                               uint32_t bufferLength,
                               NodeNum destId,
                               NodeNum selfId,
                               ConnType type,
                               UPDATE_CONNECTIVITY_FN statusCallback,
                               NodeMap nodes) {
    auto res = ASYNC_CONN_PTR(
        new AsyncTcpConnection(service,
                               onError,
                               onReady,
                               bufferLength,
                               destId,
                               selfId,
                               type,
                               nodes,
                               statusCallback));
    res->init();
    return res;
  }

  virtual ~AsyncTcpConnection() {
    LOG_INFO(_logger, "Dtor called, node: " << _selfId << "peer: " << _destId << ", type: " <<
                                            _connType);

    delete[] _inBuffer;

    _receiver = nullptr;
    _fOnError = nullptr;
    _fOnHellOMessage = nullptr;
  }
};

////////////////////////////////////////////////////////////////////////////

/**
 * Implementation class. Is responsible for creating listener on given port,
 * outgoing connections to the lower Id peers and accepting connections from
 *  higher ID peers.
 *  This is default behavior given the clients will always have higher IDs
 *  from the replicas. In this way we assure that clients will not connect to
 *  each other.
 */
class PlainTCPCommunication::PlainTcpImpl :
    public std::enable_shared_from_this<PlainTcpImpl> {
 private:
  unordered_map<NodeNum, ASYNC_CONN_PTR> _connections;

  unique_ptr<asio::ip::tcp::acceptor> _pAcceptor = nullptr;
  std::thread *_pIoThread = nullptr;

  NodeNum _selfId;
  IReceiver *_pReceiver = nullptr;

  // NodeNum mapped to tuple<ip, port> //
  NodeMap _nodes;
  asio::io_service _service;
  uint16_t _listenPort;
  string _listenIp;
  uint32_t _bufferLength;
  uint32_t _maxServerId;
  Logger _logger;
  UPDATE_CONNECTIVITY_FN _statusCallback;

  mutex _connectionsGuard;
  mutable mutex _startStopGuard;

  /**
   * When the connection is broken, this method is called  and the broken
   * connection is removed from the map. If the closed connection was
   * Outcoming this method will initiate a new one.
   * @param peerId ID of the remote peer for the failed connection.
   */
  void on_async_connection_error(NodeNum peerId) {
    LOG_DEBUG(_logger, "on_async_connection_error, peerId: " << peerId);
    lock_guard<mutex> lock(_connectionsGuard);
    if (_connections.find(peerId) != _connections.end()) {
      _connections.erase(peerId);
    }

    // here we check in the nodes map for the peer info and connect again, if
    // needed.
    auto iter = _nodes.find(peerId);
    if(iter != _nodes.end()) {
      if (iter->first < _selfId && iter->first <= _maxServerId) {
        create_outgoing_connection(peerId,
                                   iter->second.host,
                                   iter->second.port);
      }
    } else {
      LOG_ERROR(_logger, "Unknown peer, id: " << peerId);
    }
  }

  /**
   * This function is called when the connection is authenticated and the TLS
   * handshake is done. From this point this connections is secured and can
   * be used by the application. Note, that if we already have the connection
   * from the same peer in the map we reject BOTH (e.g. malicious replica
   * that uses someone's else ID)
   * @param id
   * @param conn
   */
  void on_hello(NodeNum id, ASYNC_CONN_PTR conn) {
    lock_guard<mutex> lock(_connectionsGuard);
    // probably bad replica?? TODO: think how to handle it in a better way
    // for now, just throw away both existing and a new one
    if (_connections.find(id) != _connections.end()) {
      LOG_ERROR(_logger, "new incoming connection with peer id that already "
                         "exists, destroying both, peer: " << id);
      _connections.erase(id);
      return;
    }

    conn->setReceiver(id, _pReceiver);
    _connections.insert(make_pair(id, conn));
  }

  void on_accept(ASYNC_CONN_PTR conn,
                 const B_ERROR_CODE &ec) {
    LOG_DEBUG(_logger, "on_accept, enter, node: " + to_string(_selfId) +
        ", ec: " + ec.message());

    if (!ec) {
      conn->start();
    }

    // When io_service is stopped, the handlers are destroyed and when the
    // io_service dtor runs they will be invoked with operation_aborted error.
    // In this case we dont want to listen again and we rely on the
    // shared_from_this for the cleanup.
    if(ec != asio::error::operation_aborted) {
      start_accept();
    }
  }

  // here need to check how "this" passed to handlers behaves if the object is
  // deleted.
  void start_accept() {
    LOG_DEBUG(_logger, "start_accept, node: " << _selfId);
    auto conn =
        AsyncTcpConnection::create(
            &_service,
            std::bind(
                &PlainTcpImpl::on_async_connection_error,
                shared_from_this(),
                std::placeholders::_1),
            std::bind(
                &PlainTcpImpl::on_hello,
                shared_from_this(),
                std::placeholders::_1,
                std::placeholders::_2),
            _bufferLength,
            AsyncTcpConnection::UNKNOWN_NODE_ID,
            _selfId,
            ConnType::Incoming,
            _statusCallback,
            _nodes);
    _pAcceptor->async_accept(conn->get_socket(),
                             boost::bind(
                                 &PlainTcpImpl::on_accept,
                                 shared_from_this(),
                                 conn,
                                 boost::asio::placeholders::error));
  }

  PlainTcpImpl(const PlainTcpImpl &) = delete;
  PlainTcpImpl(const PlainTcpImpl &&) = delete;
  PlainTcpImpl &operator=(const PlainTcpImpl &) = delete;
  PlainTcpImpl() = delete;

  PlainTcpImpl(NodeNum selfNodeNum,
               NodeMap nodes,
               uint32_t bufferLength,
               uint16_t listenPort,
               uint32_t maxServerId,
               string listenIp,
               UPDATE_CONNECTIVITY_FN statusCallback = nullptr) :
      _selfId(selfNodeNum),
      _listenPort(listenPort),
      _listenIp(listenIp),
      _bufferLength(bufferLength),
      _maxServerId(maxServerId),
      _logger(Log::getLogger("concord.tcp")),
      _statusCallback{statusCallback} {
    //_service = new io_service();
    for (auto &it : nodes) {
      _nodes.insert({it.first, it.second});
    }
  }

  void create_outgoing_connection(
      NodeNum nodeId, string peerIp, uint16_t peerPort) {
    auto conn =
        AsyncTcpConnection::create(
            &_service,
            std::bind(&PlainTcpImpl::on_async_connection_error,
                      shared_from_this(),
                      std::placeholders::_1),

            std::bind(&PlainTcpImpl::on_hello,
                      shared_from_this(),
                      std::placeholders::_1,
                      std::placeholders::_2),
            _bufferLength,
            nodeId,
            _selfId,
            ConnType::Outgoing,
            _statusCallback,
            _nodes);

    conn->connect(peerIp, peerPort);
    LOG_INFO(_logger, "    for node " << _selfId << ", dest: " << nodeId);
  }

 public:
  static std::shared_ptr<PlainTcpImpl> create(NodeNum selfNodeId,
                                              NodeMap nodes,
                                              uint32_t bufferLength,
                                              uint16_t listenPort,
                                              uint32_t tempHighestNodeForConnecting,
                                              string listenIp,
                                              UPDATE_CONNECTIVITY_FN statusCallback) {
    return std::shared_ptr<PlainTcpImpl>(new PlainTcpImpl(selfNodeId,
                                                          nodes,
                                                          bufferLength,
                                                          listenPort,
                                                          tempHighestNodeForConnecting,
                                                          listenIp,
                                                          statusCallback));
  }

  int getMaxMessageSize() {
    return _bufferLength;
  }

  /**
   * logics moved from the ctor to this method to allow shared_from_this
   * @return
   */
  int Start() {
    lock_guard<mutex> l(_startStopGuard);

    if (_pIoThread) {
      return 0; // running
    }

    // all replicas are in listen mode
    if (_selfId <= _maxServerId) {
      // patch, we need to listen to all interfaces in order to support
      // machines with internal/external IPs. Need to add "listen IP" to the BFT
      // config file.
      asio::ip::tcp::endpoint ep(
          asio::ip::address::from_string(_listenIp), _listenPort);
      _pAcceptor = boost::make_unique<asio::ip::tcp::acceptor>(_service, ep);
      start_accept();
    } else // clients don't listen
      LOG_INFO(_logger, "skipping listen for node: " << _selfId);

    // this node should connect only to nodes with lower ID
    // and all nodes with higher ID will connect to this node
    // we don't want that clients will connect to other clients
    for (auto it = _nodes.begin(); it != _nodes.end(); it++) {
      if (_statusCallback && it->second.isReplica) {
        PeerConnectivityStatus pcs{};
        pcs.peerId = it->first;
        pcs.peerHost = it->second.host;
        pcs.peerPort = it->second.port;
        pcs.statusType = StatusType::Started;
        _statusCallback(pcs);
      }

      // connect only to nodes with ID higher than selfId
      // and all nodes with lower ID will connect to this node
      if (it->first < _selfId && it->first <= _maxServerId) {
        create_outgoing_connection(it->first, it->second.host, it->second.port);
      }
    }

    _pIoThread =
        new std::thread(std::bind
                            (static_cast<size_t(boost::asio::io_service::*)()>
                             (&boost::asio::io_service::run),
                             std::ref(_service)));

    return 0;
  }

  /**
  * Stops the object (including its internal threads).
  * On success, returns 0.
  */
  int Stop() {
    lock_guard<mutex> l(_startStopGuard);

    if (!_pIoThread) {
      return 0; // stopped
    }

    _service.stop();
    _pIoThread->join();

    _connections.clear();

    return 0;
  }

  bool isRunning() const {
    lock_guard<mutex> l(_startStopGuard);

    if (!_pIoThread) {
      return false; // stopped
    }

    return true;
  }

  ConnectionStatus
  getCurrentConnectionStatus(const NodeNum node) const {
    return isRunning() ? ConnectionStatus::Connected :
           ConnectionStatus::Disconnected;
  }

  void setReceiver(NodeNum nodeId, IReceiver *rec) {
    _pReceiver = rec;
    for (auto it : _connections) {
      it.second->setReceiver(nodeId, rec);
    }
  }

  /**
  * Sends a message on the underlying communication layer to a given
  * destination node. Asynchronous (non-blocking) method.
  * Returns 0 on success.
  */
  int sendAsyncMessage(const NodeNum destNode,
                       const char *const message,
                       const size_t messageLength) {
    lock_guard<mutex> lock(_connectionsGuard);
    auto temp = _connections.find(destNode);
    if (temp != _connections.end()) {
      temp->second->send(message, messageLength);
    } else {
      LOG_DEBUG(_logger,
                "connection NOT found, from: " << _selfId
                                               << ", to: " << destNode);
    }

    return 0;
  }

  ~PlainTcpImpl() {
    LOG_DEBUG(_logger, "PlainTcpImpl dtor");
    _pIoThread = nullptr;
  }
};

PlainTCPCommunication::~PlainTCPCommunication() {

}

PlainTCPCommunication::PlainTCPCommunication(const PlainTcpConfig &config) {
  _ptrImpl = PlainTcpImpl::create(config.selfId,
                                  config.nodes,
                                  config.bufferLength,
                                  config.listenPort,
                                  config.maxServerId,
                                  config.listenHost,
                                  config.statusCallback);
}

PlainTCPCommunication *PlainTCPCommunication::create(const PlainTcpConfig
                                                     &config) {
  return new PlainTCPCommunication(config);
  return new PlainTCPCommunication(config);
}

int PlainTCPCommunication::getMaxMessageSize() {
  return _ptrImpl->getMaxMessageSize();
}

int PlainTCPCommunication::Start() {
  return _ptrImpl->Start();
}

int PlainTCPCommunication::Stop() {
  if (!_ptrImpl)
    return 0;

  auto res = _ptrImpl->Stop();
  return res;
}

bool PlainTCPCommunication::isRunning() const {
  return _ptrImpl->isRunning();
}

ConnectionStatus
PlainTCPCommunication::getCurrentConnectionStatus(const NodeNum node) const {
  return _ptrImpl->getCurrentConnectionStatus(node);
}

int
PlainTCPCommunication::sendAsyncMessage(const NodeNum destNode,
                                        const char *const message,
                                        const size_t messageLength) {
  return _ptrImpl->sendAsyncMessage(destNode, message, messageLength);
}

void
PlainTCPCommunication::setReceiver(NodeNum receiverNum, IReceiver *receiver) {
  _ptrImpl->setReceiver(receiverNum, receiver);
}
} // namespace bftEngine