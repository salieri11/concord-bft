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
 * Therea are 2 main classes: AsyncTlsConnection - that represents stateful
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

#include "boost/bind.hpp"
#include <boost/asio.hpp>
#include <boost/make_unique.hpp>
#include <boost/asio/deadline_timer.hpp>
#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <boost/filesystem.hpp>
#include <boost/asio/ssl.hpp>
#include "openssl/ssl.h"
#include <openssl/x509.h>
#include <openssl/x509v3.h>
#include "Logging.hpp"

using namespace std;
using namespace concordlogger;
using namespace boost;

namespace bftEngine {

class AsyncTlsConnection;

typedef boost::system::error_code B_ERROR_CODE;
typedef std::shared_ptr<AsyncTlsConnection> ASYNC_CONN_PTR;
typedef asio::ssl::stream<asio::ip::tcp::socket> SSL_SOCKET;
typedef unique_ptr<SSL_SOCKET> B_TLS_SOCKET_PTR;

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
 * instance represents Incoming connection, when the connection is broken the
 * instance will clean up itself by calling the _fOnError method.
 * The Outgoing connection instance will reset the state and try to reconnet
 * using the timer (goes from 512 ms to 8 sec timeouts, cyclic
 */
class AsyncTlsConnection : public
                           std::enable_shared_from_this<AsyncTlsConnection> {
 public:
  // since 0 is legal node number, we must initialize it to some "not
  // defined" value. This approach is fragile. TODO:(IG) use real "not
  // defined" value
  static const NodeNum UNKNOWN_NODE_ID = numeric_limits<NodeNum>::max();

 private:
  // msg header: 4 bytes msg length
  static constexpr uint8_t MSG_LENGTH_FIELD_SIZE = 4;
  static constexpr uint8_t MSG_HEADER_SIZE = MSG_LENGTH_FIELD_SIZE;

  bool _isReplica = false;
  bool _destIsReplica = false;
  recursive_mutex _connectionStateGuard;
  asio::io_service *_service = nullptr;
  uint32_t _maxMessageLength;
  char *_inBuffer;
  char *_outBuffer;
  IReceiver *_receiver = nullptr;
  function<void(NodeNum)> _fOnError = nullptr;
  function<void(NodeNum, ASYNC_CONN_PTR)> _fOnTlsReady = nullptr;
  NodeNum _expectedDestId = AsyncTlsConnection::UNKNOWN_NODE_ID;
  NodeNum _destId = AsyncTlsConnection::UNKNOWN_NODE_ID;
  NodeNum _selfId;
  string _ip;
  uint16_t _port;
  asio::deadline_timer _connectTimer;
  ConnType _connType;
  uint16_t _minTimeout = 256;
  uint16_t _maxTimeout = 8192;
  uint16_t _currentTimeout = _minTimeout;
  B_TLS_SOCKET_PTR _socket = nullptr;
  string _certificatesRootFolder;
  Logger _logger;
  UPDATE_CONNECTIVITY_FN _statusCallback = nullptr;
  NodeMap _nodes;
  asio::ssl::context _sslContext;

  // the following is assumed for these state variables: write access - only
  // from the single io_service worker thread guarded by mutex due to
  // possible external read, read - either from the same single io_service
  // worker thread or from an external thread (guarded by mutex)
  bool _closed;
  bool _authenticated;
  bool _connected;
 public:

 private:
  AsyncTlsConnection(asio::io_service *service,
                     function<void(NodeNum)> onError,
                     function<void(NodeNum, ASYNC_CONN_PTR)> onAuthenticated,
                     uint32_t bufferLength,
                     NodeNum destId,
                     NodeNum selfId,
                     string certificatesRootFolder,
                     ConnType type,
                     NodeMap nodes,
                     UPDATE_CONNECTIVITY_FN statusCallback = nullptr) :
      _service(service),
      _maxMessageLength(bufferLength + MSG_HEADER_SIZE + 1),
      _fOnError(onError),
      _fOnTlsReady(onAuthenticated),
      _expectedDestId(destId),
      _selfId(selfId),
      _connectTimer(*service),
      _connType(type),
      _certificatesRootFolder(certificatesRootFolder),
      _logger(Logger::getLogger("concord-bft.tls")),
      _statusCallback{statusCallback},
      _nodes{std::move(nodes)},
      _sslContext{asio::ssl::context(type == ConnType::Incoming
                                     ? asio::ssl::context::tlsv12_server
                                     : asio::ssl::context::tlsv12_client)},
      _closed(false),
      _authenticated{false},
      _connected{false} {
    LOG_DEBUG(_logger, "ctor, node " << _selfId << ", connType: " << _destId);

    _inBuffer = new char[bufferLength];
    _outBuffer = new char[bufferLength];

    set_tls();

    _socket = B_TLS_SOCKET_PTR(new SSL_SOCKET(*service, _sslContext));
    _connectTimer.expires_at(boost::posix_time::pos_infin);
  }

  void set_closed(bool value) {
    lock_guard<recursive_mutex> lock(_connectionStateGuard);
    _closed = value;
  }

  void set_connected(bool value) {
    lock_guard<recursive_mutex> lock(_connectionStateGuard);
    _connected = value;
  }

  void set_authenticated(bool value) {
    lock_guard<recursive_mutex> lock(_connectionStateGuard);
    _authenticated = value;
  }

  bool check_replica(NodeNum node) {
    auto it = _nodes.find(node);
    if (it == _nodes.end()) {
      return false;
    }

    return it->second.isReplica;
  }

  uint32_t get_message_length(const char *buffer,
                              size_t length) {
    return *(static_cast<const uint32_t *>(
        static_cast<const void *>(buffer)));
  }

  void put_message_header(char *data, uint32_t dataLength) {
    memcpy(data, &dataLength, MSG_LENGTH_FIELD_SIZE);
  }

  void set_tls() {
    assert(_connType != ConnType::NotDefined);

    if (ConnType::Incoming == _connType)
      set_tls_server();
    else
      set_tls_client();

    SSL_CTX_set_cipher_list(_sslContext.native_handle(),
                            "SHA384");
  }

  void set_tls_server() {
    _sslContext.set_verify_mode(asio::ssl::verify_peer |
        asio::ssl::verify_fail_if_no_peer_cert);
    _sslContext.set_options(
        boost::asio::ssl::context::default_workarounds
            | boost::asio::ssl::context::no_sslv2
            | boost::asio::ssl::context::no_sslv3
            | boost::asio::ssl::context::no_tlsv1
            | boost::asio::ssl::context::no_tlsv1_1
            | boost::asio::ssl::context::single_dh_use);

    _sslContext.set_verify_callback(
        boost::bind(&AsyncTlsConnection::verify_certificate_server,
                    this,
                    _1,
                    _2));

    namespace fs = boost::filesystem;
    auto path = fs::path(_certificatesRootFolder) /
        fs::path(to_string(_selfId)) /
        fs::path("server");
    _sslContext.use_certificate_chain_file((path / fs::path("server.cert")

                                           ).string());
    _sslContext.use_private_key_file((path / fs::path("pk.pem")
                                     ).string(),
                                     boost::asio::ssl::context::pem);

    // if we cant create EC DH params, it may mean that SSL version old or
    // any other crypto related errors, we can't continue with TLS
    EC_KEY *ecdh = EC_KEY_new_by_curve_name(NID_secp384r1);
    if (!ecdh) {
      LOG_ERROR(_logger, "Unable to create EC");
      abort();
    }

    if (1 != SSL_CTX_set_tmp_ecdh (_sslContext.native_handle(), ecdh)) {
      LOG_ERROR(_logger, "Unable to set temp EC params");
      abort();
    }

    // as the OpenSSL does reference counting, it should be safe to free the key
    // however, no explicit info on this point in the openssl docs.
    // this info is from various online sources and examples
    EC_KEY_free(ecdh);
  }

  void set_tls_client() {
    _sslContext.set_verify_mode(asio::ssl::verify_peer);

    namespace fs = boost::filesystem;
    auto path = fs::path(_certificatesRootFolder) /
        fs::path(to_string(_selfId)) /
        "client";
    auto serverPath = fs::path(_certificatesRootFolder) /
        fs::path(to_string(_expectedDestId)) /
        "server";

    _sslContext.set_verify_callback(
        boost::bind(&AsyncTlsConnection::verify_certificate_client,
                    this,
                    _1,
                    _2));

    _sslContext.use_certificate_chain_file((path / "client.cert").string());
    _sslContext.use_private_key_file((path / "pk.pem").string(),
                                     boost::asio::ssl::context::pem);

    _sslContext.load_verify_file((serverPath / "server.cert").string());
  }

  bool verify_certificate_server(bool preverified,
                                 boost::asio::ssl::verify_context &ctx) {
    char subject[512];
    X509 *cert = X509_STORE_CTX_get_current_cert(ctx.native_handle());
    if (!cert) {
      LOG_ERROR(_logger, "no certificate from client");
      return false;
    } else {
      X509_NAME_oneline(X509_get_subject_name(cert), subject, 512);
      LOG_DEBUG(_logger,
                "Verifying client: " << subject << ", " << preverified);
      bool res = check_certificate(cert, "client", string(subject),
                                   UNKNOWN_NODE_ID);
      LOG_DEBUG(_logger,
                "Manual verifying client: " << subject
                                            << ", authenticated: " << res);
      return res;
    }
  }

  bool verify_certificate_client(bool preverified,
                                 boost::asio::ssl::verify_context &ctx) {
    char subject[256];
    X509 *cert = X509_STORE_CTX_get_current_cert(ctx.native_handle());
    if (!cert) {
      LOG_ERROR(_logger, "no certificate from server");
      return false;
    } else {
      X509_NAME_oneline(X509_get_subject_name(cert), subject, 256);
      LOG_DEBUG(_logger,
                "Verifying server: " << subject << ", " << preverified);

      bool res = check_certificate(
          cert, "server", string(subject), _expectedDestId);
      LOG_DEBUG(_logger,
                "Manual verifying server: " << subject
                                            << ", authenticated: " << res);
      return res;
    }
  }

  /**
   * certificate pinning
   * check for specific certificate and do not rely on the chain authentication
   * if verified, it sets explicitly the _destId
   */
  bool check_certificate(X509 *receivedCert,
                         string connectionType,
                         string subject,
                         NodeNum expectedPeerId) {
    // first, basic sanity test, just to eliminate disk read if the certificate
    // is unknown.
    // the certificate must have node id, as we put it in OU field on creation.
    // since we use pinning we must know who is the remote peer.
    // peerIdPrefixLength stands for the length of 'OU=' substring
    size_t peerIdPrefixLength = 3;
    std::regex r("OU=\\d*", std::regex_constants::icase);
    std::smatch sm;
    regex_search(subject, sm, r);
    if (sm.length() <= peerIdPrefixLength) {
      LOG_ERROR(_logger, "OU not found or empty: " << subject);
      return false;
    }

    string remPeer =
        sm.str().substr(
            peerIdPrefixLength, sm.str().length() - peerIdPrefixLength);
    if (0 == remPeer.length()) {
      LOG_ERROR(_logger, "OU empty " << subject);
      return false;
    }

    NodeNum remotePeerId;
    try {
      remotePeerId = stoul(remPeer, nullptr);
    } catch (const std::invalid_argument &ia) {
      LOG_ERROR(_logger, "cannot convert OU, " << subject << ", " << ia.what());
      return false;
    } catch (const std::out_of_range &e) {
      LOG_ERROR(_logger, "cannot convert OU, " << subject << ", " << e.what());
      return false;
    }

    // if server has been verified, check peers match
    if (UNKNOWN_NODE_ID != expectedPeerId) {
      if (remotePeerId != expectedPeerId) {
        LOG_ERROR(_logger, "peers doesnt match, expected: " << expectedPeerId
                                                            << ", received: "
                                                            << remPeer);
        return false;
      }
    }

    // the actual pinning - read the correct certificate from the disk and
    // compare it to the received one
    namespace fs = boost::filesystem;
    auto path = fs::path(_certificatesRootFolder) / to_string(remotePeerId)
        / connectionType
        / string(connectionType + ".cert");

    FILE *fp = fopen(path.c_str(), "r");
    if (!fp) {
      LOG_ERROR(_logger, "certificate file not found, path: " << path);
      return false;
    }

    X509 *localCert = PEM_read_X509(fp, NULL, NULL, NULL);
    if (!localCert) {
      LOG_ERROR(_logger, "cannot parse certificate, path: " << path);
      fclose(fp);
      return false;
    }

    // this is actual comparison, compares hash of 2 certs
    int res = X509_cmp(receivedCert, localCert);
    if (res == 0) {
      if (_destId == AsyncTlsConnection::UNKNOWN_NODE_ID) {
        LOG_INFO(_logger,
                 "connection authenticated, type: " << _connType
                                                    << "peer: "
                                                    << remotePeerId);
      }

      _destId = remotePeerId;
    }

    X509_free(localCert);
    fclose(fp);

    return res == 0;
  }

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
   * this method closes the socket for reconnecting to the remote peer
   */
  void close_socket() {
    if (_closed)
      return;

    set_connected(false);
    set_authenticated(false);

    LOG_DEBUG(_logger,
              "close_socket, node " << _selfId << ", dest: " << _destId
                                    << ", connected: " << _connected
                                    << ", closed: " << _closed);
    B_ERROR_CODE ec;
    try {
      if (_socket) {
        _socket->shutdown();
        get_socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
        was_error(ec, "close_socket,shutdown");
      }
    } catch (std::exception &e) {
      LOG_ERROR(_logger,
                "close_socket, exception, node " << _selfId << ", "
                                                               "dest: "
                                                 << _destId << ", connected: "
                                                 << _connected
                                                 << ", ex: " << e.what());
    }

    get_socket().close(ec);
    was_error(ec, "close_socket");
  }

  /**
   * this method closes the socket and frees the object by calling the _fOnError
   */
  void close() {
    if (_closed)
      return;

    set_authenticated(false);
    set_connected(false);
    set_closed(true);

    LOG_DEBUG(_logger, "close, node " << _selfId << ", dest: " << _destId
                                      << ", connected: " << _connected
                                      << ", closed: " << _closed);

    _connectTimer.cancel();

    B_ERROR_CODE ec;
    try {
      if (_socket) {
        _socket->shutdown();
        get_socket().shutdown(
            boost::asio::ip::tcp::socket::shutdown_both, ec);
        was_error(ec, "close,shutdown");
      }
    } catch (std::exception &e) {
      LOG_ERROR(_logger, "exception, node " << _selfId << ", dest: " << _destId
                                            << ", connected: " << _connected
                                            << ", ex: " << e.what());
    }

    get_socket().close(ec);
    was_error(ec, "close");

    if (_fOnError) {
      _fOnError(_destId);
    }
  }

  /**
   * reconnects to tje remote peer (Outgoind connection) after resetting state
   */
  void reconnect() {
    if (_closed)
      return;

    assert(_connType != ConnType::NotDefined);

    LOG_DEBUG(_logger,
              "reconnect, node " << _selfId
                                 << ", dest: " << _destId
                                 << ", connected: " << _connected << "is_open: "
                                 << get_socket().is_open());

    set_authenticated(false);
    set_connected(false);

    close_socket();
    _sslContext = asio::ssl::context(_connType == ConnType::Incoming
                                     ? asio::ssl::context::tls_server
                                     : asio::ssl::context::tls_client);
    set_tls();
    _socket.reset(new SSL_SOCKET(*_service, _sslContext));

    setTimeOut();
    if (_fOnError) {
      _fOnError(_destId);
    }
    connect(_ip, _port, _destIsReplica);

    LOG_TRACE(_logger, "reconnect exit, node " << _selfId
              << ", dest: " << _destId
              << ", connected: " << _connected
              << "is_open: " << get_socket().is_open());
  }

  /**
   * generic error handling function
   */
  void handle_error() {
    assert(_connType != ConnType::NotDefined);

    if (ConnType::Incoming == _connType) {
      close();
    } else {
      reconnect();
      if (_statusCallback) {
        bool isReplica = check_replica(_selfId);
        if (isReplica) {
          PeerConnectivityStatus pcs;
          pcs.peerId = _selfId;
          pcs.statusType = StatusType::Broken;

          // pcs.statusTime = we dont set it since it is set by the aggregator
          // in the upcoming version timestamps should be reviewed
          _statusCallback(pcs);
        }
      }
    }
  }

  /**
   * occurs when msg length bytes are read from the stream to
   * determine themessage length
   * @param ec Error code
   * @param bytesRead actual bytes read
   */
  void
  read_msglength_completed(const B_ERROR_CODE &ec,
                           const uint32_t bytesRead) {
    if (_closed) {
      return;
    }

    auto err = was_error(ec, __func__);
    if (err) {
      handle_error();
      return;
    }

    uint32_t msgLength =
        get_message_length(_inBuffer, bytesRead);
    if (msgLength == 0 || msgLength > _maxMessageLength - 1 - MSG_HEADER_SIZE) {
      LOG_ERROR(_logger,
                "on_read_async_header_completed, msgLen: " << msgLength);
      read_msg_length_async();
    }

    read_msg_async(msgLength);

    LOG_TRACE(_logger, "exit, node " << _selfId
              << ", dest: " << _destId
              << ", connected: " << _connected
              << "is_open: " << get_socket().is_open());
  }

  /**
   * start reading message length bytes from the stream
   */
  void read_msg_length_async() {
    if (_closed)
      return;

   async_read(*_socket,
               asio::buffer(_inBuffer, MSG_LENGTH_FIELD_SIZE),
               boost::bind(&AsyncTlsConnection::read_msglength_completed,
                           shared_from_this(),
                           boost::asio::placeholders::error,
                           boost::asio::placeholders::bytes_transferred));

    LOG_TRACE(_logger, "read_msg_length_async, node " << _selfId
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
    if (_closed) {
      return;
    }

    auto err = was_error(ec, __func__);
    if (err) {
      handle_error();
      return;
    }

    try {
      if (_receiver) {
        _receiver->onNewMessage(_destId, _inBuffer, bytesRead);
      }
    } catch (std::exception &e) {
      LOG_ERROR(_logger, "read_msg_async_completed, exception:" << e.what());
    }

    read_msg_length_async();

    if (_statusCallback && _destIsReplica) {
      PeerConnectivityStatus pcs;
      pcs.peerId = _destId;
      pcs.peerIp = _ip;
      pcs.peerPort = _port;
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
    if (_closed) {
      return;
    }

    LOG_DEBUG(_logger, "read_msg_async, node " << _selfId << ", dest: " <<
                                               _destId);

    // async operation will finish when either expectedBytes are read
    // or error occured, this is what Asio guarantees
    async_read(*_socket,
               boost::asio::buffer(_inBuffer, msgLength),
               boost::bind(&AsyncTlsConnection::read_msg_async_completed,
                           shared_from_this(),
                           boost::asio::placeholders::error,
                           boost::asio::placeholders::bytes_transferred));

  }

  /**
   * occurs when data has been written to the stream
   * @param data
   * @param length
   */
  void write_completed(
      const B_ERROR_CODE &ec, size_t bytesTransferred, const char *data) {
    // safe to delete. asio guarantees that either all the data has been sent or
    // there was an error
    if(data) {
      delete[] data;
    }

    bool err = was_error(ec, "write_completed");
    if (err) {
      handle_error();
    }
  }

  void setTimeOut() {
    _currentTimeout = _currentTimeout == _maxTimeout
                      ? _minTimeout
                      : _currentTimeout * 2;
  }

  void connect_timer_tick(const B_ERROR_CODE &ec) {
    if (_closed) {
      return;
    }

    if (_connected) {
      LOG_DEBUG(_logger, "connect_timer_tick, connected, node " << _selfId
                                                                << ", dest: "
                                                                << _destId
                                                                << ", ec: "
                                                                << ec);
      _connectTimer.expires_at(boost::posix_time::pos_infin);
    } else if (_connectTimer.expires_at() <=
        asio::deadline_timer::traits_type::now()) {
      LOG_DEBUG(_logger, "connect_timer_tick, reconnecting, node " << _selfId
                                                                   << ", dest: "
                                                                   << _destId
                                                                   << ", ec: "
                                                                   << ec);
      reconnect();
    } else {
      LOG_DEBUG(_logger, "connect_timer_tick else, node " << _selfId
                                                          << ", dest: "
                                                          << _destId
                                                          << ", ec: "
                                                          << ec.message());
    }

    _connectTimer.async_wait(
        boost::bind(&AsyncTlsConnection::connect_timer_tick,
                    shared_from_this(),
                    boost::asio::placeholders::error));

    LOG_TRACE(_logger, "connect_timer_tick, node " << _selfId
              << ", dest: " << _destId
              << ", ec: " << ec.message());
  }

  /**
   * occures when async connect completes - need to chectk the socket & timer
   * states to determine timeout or conenection success
   * @param err
   */
  void connect_completed(const B_ERROR_CODE &err) {
    if (_closed) {
      return;
    }

    auto res = was_error(err, __func__);
    if (!get_socket().is_open()) {
      // async_connect opens socket on start so
      //nothing to do here since timeout occured and closed the socket
      if (_connected) {
        LOG_DEBUG(_logger,
                  "node " << _selfId << " is DISCONNECTED from node "
                          << _destId);
      }
      set_connected(false);
    } else if (res) {
      set_connected(false);
      //timeout didnt happen yet but the connection failed
      // nothig to do here, left for clarity
    } else {
      set_connected(true);
      LOG_DEBUG(_logger, "connected, node " << _selfId
                                            << ", dest: " << _destId
                                            << ", res: " << res);

      boost::asio::socket_base::linger o(true, 0);
      get_socket().set_option(o);

      _socket->async_handshake(boost::asio::ssl::stream_base::client,
                               boost::bind(
                                   &AsyncTlsConnection::on_handshake_complete_outbound,
                                   this,
                                   boost::asio::placeholders::error));

    }

    LOG_TRACE(_logger, "exit, node " << _selfId << ", dest: " << _destId);
  }

  /**
   * tls handhshake callback for the client
   * @param error code
   */
  void on_handshake_complete_outbound(const B_ERROR_CODE &ec) {
    bool err = was_error(ec, "on_handshake_complete_outbound");
    if (err) {
      handle_error();
      return;
    }

    set_authenticated(true);
    _connectTimer.expires_at(boost::posix_time::pos_infin);
    _currentTimeout = _minTimeout;
    if (_fOnTlsReady) {
      _fOnTlsReady(_destId, shared_from_this());
      read_msg_length_async();
    }
  }

  /**
   * tls handhshake callback for the server
   * @param error code
   */
  void on_handshake_complete_inbound(const B_ERROR_CODE &ec) {
    bool err = was_error(ec, "on_handshake_complete_inbound");
    if (err) {
      handle_error();
      return;
    }

    set_authenticated(true);
    if (_fOnTlsReady) {
      _fOnTlsReady(_destId, shared_from_this());
      read_msg_length_async();
    }
  }

  void init() {
    _connectTimer.async_wait(
        boost::bind(&AsyncTlsConnection::connect_timer_tick,
                    shared_from_this(),
                    boost::asio::placeholders::error));
  }

 public:
  SSL_SOCKET::lowest_layer_type &get_socket() {
    return _socket->lowest_layer();
  }

  /**
   * start connection to the remote peer (Outgoing connection)
   * @param ip remote IP
   * @param port remote port
   * @param isReplica whether the peer is replica or client
   */
  void connect(string ip, uint16_t port, bool isReplica) {
    _ip = ip;
    _port = port;
    _destIsReplica = isReplica;

    asio::ip::tcp::endpoint ep(asio::ip::address::from_string(ip), port);
    _connectTimer.expires_from_now(
        boost::posix_time::millisec(_currentTimeout));

    get_socket().
        async_connect(ep,
                      boost::bind(&AsyncTlsConnection::connect_completed,
                                  shared_from_this(),
                                  boost::asio::placeholders::error));
    LOG_TRACE(_logger, "exit, from: " << _selfId
              << " ,to: " << _destId
              << ", ip: " << ip
              << ", port: " << port);
  }

  void start() {
    boost::asio::socket_base::linger o(true, 0);
    get_socket().set_option(o);
    set_connected(true);
    _socket->async_handshake(boost::asio::ssl::stream_base::server,
                             boost::bind(&AsyncTlsConnection::on_handshake_complete_inbound,
                                         this,
                                         boost::asio::placeholders::error));
  }

  /**
   * mimics the async sending by using Post to Asio working thread
   *
   * @param data data to be sent
   * @param length data length
   */
  bool send(const char *data, uint32_t length) {
    lock_guard<recursive_mutex> lock(_connectionStateGuard);

    // this is the only access to the state variables from external thread
    if (_closed || !_connected || !_authenticated) {
      LOG_DEBUG(_logger, "send(), from: " << _selfId
                 << ", to: " << _destId
                 << ", length: " << length
                 << ", connected: " << _connected
                 << ", closed: " << _closed
                 << ", authenticated: " << _authenticated);
      return false;
    }

    assert(data);
    assert(length > 0 && length <= _maxMessageLength - MSG_HEADER_SIZE);

    char *buf = new char[length + MSG_HEADER_SIZE];
    memset(buf, 0, length + MSG_HEADER_SIZE);
    put_message_header(buf, length);
    memcpy(buf + MSG_HEADER_SIZE, data, length);
    boost::asio::async_write(
        *_socket,
        asio::buffer(buf, length + MSG_HEADER_SIZE),
        boost::bind(&AsyncTlsConnection::write_completed,
            shared_from_this(),
            asio::placeholders::error,
            asio::placeholders::bytes_transferred,
            buf));

    LOG_DEBUG(_logger, "from: " << _selfId
                                << ", to: " << _destId
                                << ", length: " << length);

    if (_statusCallback && _isReplica) {
      PeerConnectivityStatus pcs;
      pcs.peerId = _selfId;
      pcs.statusType = StatusType::MessageSent;

      // pcs.statusTime = we dont set it since it is set by the aggregator
      // in the upcoming version timestamps should be reviewed
      _statusCallback(pcs);
    }

    return true;
  }

  void setReceiver(NodeNum nodeId, IReceiver *rec) {
    _receiver = rec;
  }

  static ASYNC_CONN_PTR create(asio::io_service *service,
                               function<void(NodeNum)> onError,
                               function<void(NodeNum, ASYNC_CONN_PTR)> onHello,
                               uint32_t bufferLength,
                               NodeNum destId,
                               NodeNum selfId,
                               string certificatesRootFolder,
                               ConnType type,
                               UPDATE_CONNECTIVITY_FN statusCallback,
                               NodeMap nodes) {
    auto res = ASYNC_CONN_PTR(
        new AsyncTlsConnection(service,
                               onError,
                               onHello,
                               bufferLength,
                               destId,
                               selfId,
                               certificatesRootFolder,
                               type,
                               nodes,
                               statusCallback));
    res->init();
    return res;
  }

  virtual ~AsyncTlsConnection() {
    LOG_DEBUG(_logger, "Dtor called, peer: " << _destId << ", type: " <<
                                             _connType);

    delete[] _inBuffer;
    delete[] _outBuffer;

    _receiver = nullptr;
    _fOnError = nullptr;
    _fOnTlsReady = nullptr;
  }
};

////////////////////////////////////////////////////////////////////////////

/**
 * Implementation class. Is reponsible for creating listener on given port,
 * outgoing connections to the lower Id peers and accepting connections from
 *  higher ID peers.
 *  This is default behavior given the clients will always have higher IDs
 *  from the replicas. In this way we assure that clients will not connect to
 *  each other.
 */
class TlsTCPCommunication::TlsTcpImpl {
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
  string _certRootFolder;
  Logger _logger;
  UPDATE_CONNECTIVITY_FN _statusCallback;

  recursive_mutex _connectionsGuard;

  /**
   * When the connection is broken, this methids is called for the Incoming
   * connections and the connection is removed from the map.
   * @param peerId ID of the remote peer for the failed connection.
   */
  void on_async_connection_error(NodeNum peerId) {
    LOG_DEBUG(_logger, "on_async_connection_error, peerId: " << peerId);
    lock_guard<recursive_mutex> lock(_connectionsGuard);
    if (_connections.find(peerId) != _connections.end()) {
      _connections.erase(peerId);
    }
    else LOG_DEBUG(_logger,
                   "on_async_connection_error, no key, peerId: " << peerId);
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
  void on_connection_authenticated(NodeNum id, ASYNC_CONN_PTR conn) {
    lock_guard<recursive_mutex> lock(_connectionsGuard);
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

    //LOG4CPLUS_DEBUG(logger_, "handle_accept before start_accept");
    start_accept();
  }

  // here need to check how "this" passed to handlers behaves if the object is
  // deleted.
  void start_accept() {
    LOG_DEBUG(_logger, "start_accept, node: " << _selfId);
    auto conn =
        AsyncTlsConnection::create(
            &_service,
            std::bind(
                &TlsTcpImpl::on_async_connection_error,
                this,
                std::placeholders::_1),
            std::bind(
                &TlsTcpImpl::on_connection_authenticated,
                this,
                std::placeholders::_1,
                std::placeholders::_2),
            _bufferLength,
            AsyncTlsConnection::UNKNOWN_NODE_ID,
            _selfId,
            _certRootFolder,
            ConnType::Incoming,
            _statusCallback,
            _nodes);
    _pAcceptor->async_accept(conn->get_socket().lowest_layer(),
                             boost::bind(
                                 &TlsTcpImpl::on_accept,
                                 this,
                                 conn,
                                 boost::asio::placeholders::error));
  }

  TlsTcpImpl(const TlsTcpImpl &) = delete;
  TlsTcpImpl(const TlsTcpImpl &&) = delete;
  TlsTcpImpl &operator=(const TlsTcpImpl &) = delete;
  TlsTcpImpl() = delete;

  TlsTcpImpl(NodeNum selfNodeNum,
             NodeMap nodes,
             uint32_t bufferLength,
             uint16_t listenPort,
             uint32_t maxServerId,
             string listenIp,
             string certRootFolder,
             UPDATE_CONNECTIVITY_FN statusCallback = nullptr) :
      _selfId(selfNodeNum),
      _listenPort(listenPort),
      _listenIp(listenIp),
      _bufferLength(bufferLength),
      _maxServerId(maxServerId),
      _certRootFolder(certRootFolder),
      _logger(Logger::getLogger("concord.tls")),
      _statusCallback{statusCallback} {
    //_service = new io_service();
    for (auto it = nodes.begin(); it != nodes.end(); it++) {
      _nodes.insert({it->first, it->second});
    }

    // all replicas are in listen mode
    if (_selfId <= _maxServerId) {
      // patch, we need to listen to all interfaces in order to support
      // machines with internal/external IPs. Need to add "listen IP" to the BFT
      // config file.
      asio::ip::tcp::endpoint ep(asio::ip::address::from_string("0.0.0.0"),
                                 _listenPort);
      _pAcceptor = boost::make_unique<asio::ip::tcp::acceptor>(_service, ep);
      start_accept();
    } else // clients don't listen
    LOG_INFO(_logger, "skipping listen for node: " << _selfId);

    // this node should connect only to nodes with lower ID
    // and all nodes with higher ID will connect to this node
    // we don't want that clients will connect to other clients
    for (auto it = _nodes.begin(); it != _nodes.end(); it++) {
      // connect only to nodes with ID higher than selfId
      // and all nodes with lower ID will connect to this node
      if (it->first < _selfId && it->first <= maxServerId) {
        auto conn =
            AsyncTlsConnection::create(
                &_service,
                std::bind(&TlsTcpImpl::on_async_connection_error,
                          this,
                          std::placeholders::_1),

                std::bind(&TlsTcpImpl::on_connection_authenticated,
                          this,
                          std::placeholders::_1,
                          std::placeholders::_2),
                _bufferLength,
                it->first,
                _selfId,
                _certRootFolder,
                ConnType::Outgoing,
                _statusCallback,
                nodes);

        string peerIp = it->second.ip;
        uint16_t peerPort = it->second.port;
        conn->connect(peerIp, peerPort, it->second.isReplica);
        LOG_TRACE(_logger, "connect called for node " << to_string(it->first));
      }
    }
  }

 public:
  static TlsTcpImpl *create(NodeNum selfNodeId,
      // tuple {ip, listen port}
                            NodeMap nodes,
                            uint32_t bufferLength,
                            uint16_t listenPort,
                            uint32_t tempHighestNodeForConnecting,
                            string listenIp,
                            string certRootFolder) {
    return new TlsTcpImpl(selfNodeId,
                          nodes,
                          bufferLength,
                          listenPort,
                          tempHighestNodeForConnecting,
                          listenIp,
                          certRootFolder);
  }

  int getMaxMessageSize() {
    return _bufferLength;
  }

  int Start() {
    if (_pIoThread) {
      return 0; // running
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
    if (!_pIoThread) {
      return 0; // stopped
    }

    _service.stop();
    _pIoThread->join();

    _connections.clear();

    return 0;
  }

  bool isRunning() const {
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
    lock_guard<recursive_mutex> lock(_connectionsGuard);
    auto temp = _connections.find(destNode);
    if (temp != _connections.end()) {
      LOG_DEBUG(_logger,
                "connection found, from: " << _selfId << ", to: " << destNode);

     bool res = temp->second->send(message, messageLength);
     if(!res) {
       LOG_DEBUG(_logger, "connection found but send failed");
     }
    }

    LOG_TRACE(_logger, "exit sendAsyncMessage, from: " << _selfId
              << ", to: " << to_string(destNode));
    return 0;
  }

  ~TlsTcpImpl() {
    LOG_DEBUG(_logger, "TlsTCPDtor");
    _pIoThread = nullptr;
  }
};

TlsTCPCommunication::~TlsTCPCommunication() {
  if (_ptrImpl) {
    delete _ptrImpl;
  }
}

TlsTCPCommunication::TlsTCPCommunication(const TlsTcpConfig &config) {
  _ptrImpl = TlsTcpImpl::create(config.selfId,
                                config.nodes,
                                config.bufferLength,
                                config.listenPort,
                                config.maxServerId,
                                config.listenIp,
                                config.certificatesRootPath);
}

TlsTCPCommunication *TlsTCPCommunication::create(const TlsTcpConfig &config) {
  return new TlsTCPCommunication(config);
}

int TlsTCPCommunication::getMaxMessageSize() {
  return _ptrImpl->getMaxMessageSize();
}

int TlsTCPCommunication::Start() {
  return _ptrImpl->Start();
}

int TlsTCPCommunication::Stop() {
  if (!_ptrImpl)
    return 0;

  auto res = _ptrImpl->Stop();
  return res;
}

bool TlsTCPCommunication::isRunning() const {
  return _ptrImpl->isRunning();
}

ConnectionStatus
TlsTCPCommunication::getCurrentConnectionStatus(const NodeNum node) const {
  return _ptrImpl->getCurrentConnectionStatus(node);
}

int
TlsTCPCommunication::sendAsyncMessage(const NodeNum destNode,
                                      const char *const message,
                                      const size_t messageLength) {
  return _ptrImpl->sendAsyncMessage(destNode, message, messageLength);
}

void
TlsTCPCommunication::setReceiver(NodeNum receiverNum, IReceiver *receiver) {
  _ptrImpl->setReceiver(receiverNum, receiver);
}
} // namespace bftEngine
