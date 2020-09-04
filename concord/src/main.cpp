// Copyright 2018-2020 VMware, all rights reserved
//
// Concord node startup.

#include <grpcpp/grpcpp.h>
#include <grpcpp/resource_quota.h>
#include <jaegertracing/Tracer.h>
#include <sys/stat.h>
#include <MetricsServer.hpp>
#include <boost/asio.hpp>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>
#include <csignal>
#include <iostream>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <thread>
#include "ClientImp.h"
#include "KVBCInterfaces.h"
#include "Logger.hpp"
#include "OpenTracing.hpp"
#include "ReplicaImp.h"
#include "api/api_acceptor.hpp"
#include "bftengine/ReplicaConfig.hpp"
#include "common/concord_exception.hpp"
#include "common/status_aggregator.hpp"
#include "config/configuration_manager.hpp"
#include "consensus/bft_configuration.hpp"
#include "daml/daml_init_params.hpp"
#include "daml/daml_kvb_commands_handler.hpp"
#include "daml/daml_validator_client.hpp"
#include "daml/grpc_services.hpp"
#include "daml_commit.grpc.pb.h"
#include "diagnostics.h"
#include "diagnostics_server.h"
#include "direct_kv_storage_factory.h"
#include "ethereum/concord_evm.hpp"
#include "ethereum/eth_kvb_commands_handler.hpp"
#include "ethereum/eth_kvb_storage.hpp"
#include "ethereum/evm_init_params.hpp"
#include "hlf/grpc_services.hpp"
#include "hlf/kvb_commands_handler.hpp"
#include "hlf/kvb_storage.hpp"
#include "kv_types.hpp"
#include "merkle_tree_storage_factory.h"
#include "performance/perf_handler.hpp"
#include "performance/perf_service.hpp"
#include "replica_state_sync_imp.hpp"
#include "rocksdb/client.h"
#include "rocksdb/key_comparator.h"
#include "storage/concord_block_metadata.h"
#include "storage/kvb_key_types.h"
#include "storage_factory_interface.h"
#include "tee/grpc_services.hpp"
#include "tee/tee_commands_handler.hpp"
#include "thin_replica/grpc_services.hpp"
#include "thin_replica/subscription_buffer.hpp"
#include "thin_replica/thin_replica_impl.hpp"
#include "time/time_pusher.hpp"
#include "time/time_reading.hpp"
#include "utils/concord_eth_sign.hpp"
#include "utils/concord_prometheus_metrics.hpp"

using namespace boost::program_options;
using namespace std;

using boost::asio::io_service;
using boost::asio::ip::address;
using boost::asio::ip::tcp;
using logging::Logger;
using std::chrono::milliseconds;

using concord::api::ApiAcceptor;
using concord::common::EthLog;
using concord::common::EthTransaction;
using concord::common::EVMException;
using concord::common::StatusAggregator;
using concord::common::zero_address;
using concord::common::zero_hash;
using concord::config::ConcordConfiguration;
using concord::ethereum::EthKvbStorage;
using concord::common::operator<<;
using bftEngine::ReplicaConfig;
using concord::config::CommConfig;
using concord::consensus::KVBClient;
using concord::consensus::KVBClientPool;
using concord::ethereum::EthKvbCommandsHandler;
using concord::ethereum::EVM;
using concord::ethereum::EVMInitParams;
using concord::kvbc::BlockId;
using concord::kvbc::ClientConfig;
using concord::kvbc::IBlocksAppender;
using concord::kvbc::IClient;
using concord::kvbc::ICommandsHandler;
using concord::kvbc::ILocalKeyValueStorageReadOnly;
using concord::kvbc::IReplica;
using concord::kvbc::IStorageFactory;
using concord::kvbc::ReplicaImp;
using concord::kvbc::ReplicaStateSyncImp;
using concord::kvbc::SetOfKeyValuePairs;
using concord::storage::ConcordBlockMetadata;

using concordUtils::Status;

using concord::hlf::ChaincodeInvoker;
using concord::hlf::HlfKvbCommandsHandler;
using concord::hlf::HlfKvbStorage;
using concord::hlf::RunHlfGrpcServer;

using concord::time::TimePusher;
using concord::utils::EthSign;

using concord::daml::CommitServiceImpl;
using concord::daml::DamlKvbCommandsHandler;
using concord::daml::DamlValidatorClient;

using concord::thin_replica::SubBufferList;
using concord::thin_replica::ThinReplicaImpl;
using concord::thin_replica::ThinReplicaService;

using concord::tee::TeeServiceImpl;

// Parse BFT configuration
using concord::consensus::InitializeSbftConfiguration;

using concord::tee::TeeCommandsHandler;

static unique_ptr<grpc::Server> daml_grpc_server = nullptr;
// the Boost service hosting our Helen connections
static io_service *api_service = nullptr;
static boost::thread_group worker_pool;
// 50 MiBytes
static const int kDamlServerMsgSizeMax = 50 * 1024 * 1024;
static unique_ptr<grpc::Server> tee_grpc_server = nullptr;
static unique_ptr<grpc::Server> perf_grpc_server = nullptr;
static unique_ptr<concordMetrics::Server> bft_metrics_server = nullptr;

static void signalHandler(int signum) {
  try {
    Logger logger = Logger::getInstance("com.vmware.concord.main");
    LOG_INFO(logger, "Signal received (" << signum << ")");

    if (api_service) {
      LOG_INFO(logger, "Stopping API service");
      api_service->stop();
    }
    if (daml_grpc_server) {
      LOG_INFO(logger, "Stopping DAML gRPC service");
      daml_grpc_server->Shutdown();
    }
    if (tee_grpc_server) {
      LOG_INFO(logger, "Stopping TEE gRPC service");
      tee_grpc_server->Shutdown();
    }
    if (bft_metrics_server) {
      LOG_INFO(logger, "Stopping BFT metrics server");
      bft_metrics_server->Stop();
    }
    if (perf_grpc_server) {
      LOG_INFO(logger, "Stopping Performance gRPC service");
      perf_grpc_server->Shutdown();
    }
  } catch (exception &e) {
    cout << "Exception in signal handler: " << e.what() << endl;
  }
}

// Directs Jaeger log messages to Log4cpp
class JaegerLogger : public jaegertracing::logging::Logger {
 private:
  logging::Logger logger = logging::getLogger("jaeger");

 public:
  void error(const std::string &message) override {
    LOG_ERROR(logger, message);
  }

  void info(const std::string &message) override { LOG_INFO(logger, message); }
};

const std::string kDefaultJaegerAgent = "127.0.0.1:6831";

std::string resolve_host(std::string &host_port, Logger &logger) {
  std::string host, port;

  int colon = host_port.find(":");
  if (colon >= 0) {
    host = host_port.substr(0, colon);
    port = host_port.substr(colon + 1, host_port.length());
  } else {
    host = host_port;
    port = "6831";
  }

  tcp::resolver::query query(tcp::v4(), host, port);
  boost::asio::io_service service;
  tcp::resolver resolver(service);
  boost::system::error_code ec;
  tcp::resolver::iterator results = resolver.resolve(query, ec);
  if (!ec && results != tcp::resolver::iterator()) {
    tcp::endpoint ep = *results;
    return ep.address().to_string() + ":" + std::to_string(ep.port());
  } else {
    LOG_WARN(logger, "Unable to resolve host " << host_port);
    return kDefaultJaegerAgent;
  }
}

void initialize_tracing(ConcordConfiguration &nodeConfig, Logger &logger) {
  std::string jaeger_agent =
      nodeConfig.hasValue<std::string>("jaeger_agent")
          ? nodeConfig.getValue<std::string>("jaeger_agent")
          : jaegertracing::reporters::Config::kDefaultLocalAgentHostPort;

  // Yes, this is overly broad. Just trying to avoid lookup if the obvious
  // ip:port is specified.
  std::regex ipv4_with_port("^[0-9:.]*$");
  if (!std::regex_match(jaeger_agent, ipv4_with_port)) {
    jaeger_agent = resolve_host(jaeger_agent, logger);
  }

  LOG_INFO(logger, "Tracing to jaeger agent: " << jaeger_agent);

  // No sampling for now - report all traces
  jaegertracing::samplers::Config sampler_config(
      jaegertracing::kSamplerTypeConst, 1.0);
  jaegertracing::reporters::Config reporter_config(
      jaegertracing::reporters::Config::kDefaultQueueSize,
      jaegertracing::reporters::Config::defaultBufferFlushInterval(),
      false /* do not log spans */, jaeger_agent);
  jaegertracing::Config config(false /* not disabled */, sampler_config,
                               reporter_config);
  auto tracer = jaegertracing::Tracer::make(
      "concord", config,
      std::unique_ptr<jaegertracing::logging::Logger>(new JaegerLogger()));
  opentracing::Tracer::InitGlobal(
      std::static_pointer_cast<opentracing::Tracer>(tracer));
}

std::unique_ptr<IStorageFactory> create_storage_factory(
    const ConcordConfiguration &nodeConfig, Logger logger) {
  if (!nodeConfig.hasValue<std::string>("blockchain_db_impl")) {
    LOG_FATAL(logger, "Missing blockchain_db_impl config");
    throw EVMException("Missing blockchain_db_impl config");
  }

  const auto db_impl_name =
      nodeConfig.getValue<std::string>("blockchain_db_impl");
  auto storage_type = std::string{};
  if (nodeConfig.hasValue<std::string>("blockchain_storage_type")) {
    storage_type = nodeConfig.getValue<std::string>("blockchain_storage_type");
  }

  if (db_impl_name == "memory") {
    if (storage_type == "basic") {
      LOG_INFO(logger, "Using in-memory basic blockchain storage");
      return std::make_unique<
          concord::kvbc::v1DirectKeyValue::MemoryDBStorageFactory>();
    }
    LOG_INFO(logger, "Using in-memory merkle blockchain storage");
    return std::make_unique<
        concord::kvbc::v2MerkleTree::MemoryDBStorageFactory>();
#ifdef USE_ROCKSDB
  } else if (db_impl_name == "rocksdb") {
    const auto rocks_path =
        nodeConfig.getValue<std::string>("blockchain_db_path");
    if (storage_type == "basic") {
      LOG_INFO(logger, "Using rocksdb basic blockchain storage");
      return std::make_unique<
          concord::kvbc::v1DirectKeyValue::RocksDBStorageFactory>(rocks_path);
    }
    LOG_INFO(logger, "Using rocksdb merkle blockchain storage");
    return std::make_unique<concord::kvbc::v2MerkleTree::RocksDBStorageFactory>(
        rocks_path);
#endif
  } else {
    LOG_FATAL(logger, "Unknown blockchain_db_impl " << db_impl_name);
    throw EVMException("Unknown blockchain_db_impl");
  }
}

/**
 * IdleBlockAppender is a shim to wrap IReplica::addBlocktoIdleReplica in an
 * IBlocksAppender interface, so that it can be rewrapped in a EthKvbStorage
 * object, thus allowing the create_ethereum_genesis_block function to use the
 * same functions as concord_evm to put data in the genesis block.
 */
class IdleBlockAppender : public IBlocksAppender {
 private:
  IReplica *replica_;

 public:
  IdleBlockAppender(IReplica *replica) : replica_(replica) {}

  concordUtils::Status addBlock(const SetOfKeyValuePairs &updates,
                                BlockId &outBlockId,
                                const concordUtils::SpanWrapper &parent_span =
                                    concordUtils::SpanWrapper{}) override {
    outBlockId = 0;  // genesis only!
    return replica_->addBlockToIdleReplica(updates);
  }
};

static concordUtils::Status create_daml_genesis_block(
    IReplica *replica, ConcordConfiguration &nodeConfig, Logger logger) {
  if (replica->getReadOnlyStorage().getLastBlock() > 0) {
    LOG_INFO(logger, "Blocks already loaded, skipping genesis");
    return concordUtils::Status::OK();
  }
  if (!nodeConfig.hasValue<std::string>("genesis_block")) {
    throw concord::daml::DamlInitParamException("Genesis block path is absent");
  }
  const auto &genesis_file_path =
      nodeConfig.getValue<std::string>("genesis_block");
  if (access(genesis_file_path.c_str(), F_OK) == -1) {
    LOG_WARN(logger, "Genesis config specified but doesn't exist: "
                         << genesis_file_path);
    return concordUtils::Status::OK();
  }
  concord::daml::DamlInitParams init_params(genesis_file_path);
  const auto &key = concord::kvbc::Key(
      std::string({concord::storage::kKvbKeyAdminIdentifier}));
  return replica->addBlockToIdleReplica(
      SetOfKeyValuePairs{{key, init_params.get_admin_authentication_key()}});
}

/**
 * Create the initial transactions and a genesis block based on the
 * genesis file.
 */
static concordUtils::Status create_ethereum_genesis_block(IReplica *replica,
                                                          EVMInitParams params,
                                                          Logger logger) {
  const ILocalKeyValueStorageReadOnly &storage = replica->getReadOnlyStorage();
  IdleBlockAppender blockAppender(replica);
  EthKvbStorage kvbStorage(storage, &blockAppender);

  if (storage.getLastBlock() > 0) {
    LOG_INFO(logger, "Blocks already loaded, skipping genesis");
    return concordUtils::Status::OK();
  }

  std::map<evm_address, evm_uint256be> genesis_acts =
      params.get_initial_accounts();
  uint64_t nonce = 0;
  uint64_t chainID = params.get_chainID();
  for (auto it = genesis_acts.begin(); it != genesis_acts.end(); ++it) {
    // store a transaction for each initial balance in the genesis block
    // defintition
    EthTransaction tx = {
        nonce,                   // nonce
        zero_hash,               // block_hash: will be set in write_block
        0,                       // block_number
        zero_address,            // from
        it->first,               // to
        zero_address,            // contract_address
        std::vector<uint8_t>(),  // input
        EVM_SUCCESS,             // status
        it->second,              // value
        0,                       // gas_price
        0,                       // gas_limit
        0,                       // gas_used
        std::vector<concord::common::EthLog>(),  // logs
        zero_hash,          // sig_r (no signature for genesis)
        zero_hash,          // sig_s (no signature for genesis)
        (chainID * 2 + 35)  // sig_v
    };
    evm_uint256be txhash = tx.hash();
    LOG_INFO(logger, "Created genesis transaction "
                         << txhash << " to address " << it->first
                         << " with value = " << tx.value);
    kvbStorage.add_transaction(tx);

    // also set the balance record
    kvbStorage.set_balance(it->first, it->second);
    nonce++;
  }
  kvbStorage.set_nonce(zero_address, nonce);

  uint64_t timestamp = params.get_timestamp();
  uint64_t gasLimit = params.get_gas_limit();

  // Genesis is always proposed and accepted at the same time.
  return kvbStorage.write_block(timestamp, gasLimit);
}

static concordUtils::Status create_tee_genesis_block(
    IReplica *replica, ConcordConfiguration &nodeConfig, Logger logger) {
  if (replica->getReadOnlyStorage().getLastBlock() > 0) {
    LOG_INFO(logger, "Blocks already loaded, skipping genesis");
    return concordUtils::Status::OK();
  }

  string genesis_string = "test execution engine genesis";
  if (nodeConfig.hasValue<std::string>("genesis_block")) {
    const auto &genesis_file_path =
        nodeConfig.getValue<std::string>("genesis_block");
    std::ifstream genesis_stream(genesis_file_path);
    if (!genesis_stream.good()) {
      LOG_WARN(logger, "Error reading genesis file at " << genesis_file_path);
    } else {
      nlohmann::json genesis_block;
      genesis_stream >> genesis_block;
      genesis_string = genesis_block.dump();
    }
  }

  const auto &key = concord::kvbc::Key(
      std::string({concord::storage::kKvbKeyAdminIdentifier}));
  const auto &status = replica->addBlockToIdleReplica(
      SetOfKeyValuePairs{{key, std::string{genesis_string}}});

  if (status.isOK()) {
    LOG_INFO(logger, "Successfully loaded TEE genesis block");
  } else {
    throw "Unable to add TEE genesis block";
  }

  return concordUtils::Status::OK();
}

/*
 * Starts a set of worker threads which will call api_service.run() method.
 * This will allow us to have multiple threads accepting tcp connections
 * and passing the requests to KVBClient.
 */
void start_worker_threads(int number) {
  Logger logger = Logger::getInstance("com.vmware.concord.main");
  LOG_INFO(logger, "Starting " << number << " new API worker threads");
  assert(api_service);
  for (int i = 0; i < number; i++) {
    boost::thread *t = new boost::thread(
        boost::bind(&boost::asio::io_service::run, api_service));
    worker_pool.add_thread(t);
  }
}

void RunDamlGrpcServer(
    std::string server_address, KVBClientPool &pool,
    const ConcordConfiguration &config,
    const ILocalKeyValueStorageReadOnly *ro_storage,
    SubBufferList &subscriber_list, int max_num_threads,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry) {
  Logger logger = Logger::getInstance("com.vmware.concord.daml");

  auto commitService = std::make_unique<CommitServiceImpl>(pool, config);
  auto thinReplicaServiceImpl = std::make_unique<ThinReplicaImpl>(
      ro_storage, subscriber_list, prometheus_registry);
  auto thinReplicaService =
      std::make_unique<ThinReplicaService>(std::move(thinReplicaServiceImpl));

  grpc::ResourceQuota quota;
  quota.SetMaxThreads(max_num_threads);

  grpc::ServerBuilder builder;
  builder.SetResourceQuota(quota);
  builder.SetMaxMessageSize(kDamlServerMsgSizeMax);
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(commitService.get());
  builder.RegisterService(thinReplicaService.get());

  daml_grpc_server = unique_ptr<grpc::Server>(builder.BuildAndStart());

  LOG_INFO(logger, "DAML gRPC server listening on " << server_address);
  daml_grpc_server->Wait();
}

void RunTeeGrpcServer(std::string server_address, KVBClientPool &pool,
                      const ILocalKeyValueStorageReadOnly *ro_storage,
                      SubBufferList &subscriber_list, int max_num_threads) {
  Logger logger = Logger::getInstance("com.vmware.concord.tee");

  auto teeService = std::make_unique<TeeServiceImpl>(pool);
  auto thinReplicaServiceImpl =
      std::make_unique<ThinReplicaImpl>(ro_storage, subscriber_list);
  auto thinReplicaService =
      std::make_unique<ThinReplicaService>(std::move(thinReplicaServiceImpl));

  grpc::ResourceQuota quota;
  quota.SetMaxThreads(max_num_threads);

  grpc::ServerBuilder builder;
  builder.SetResourceQuota(quota);
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(teeService.get());
  builder.RegisterService(thinReplicaService.get());

  tee_grpc_server = unique_ptr<grpc::Server>(builder.BuildAndStart());

  LOG_INFO(logger, "TEE gRPC server listening on " << server_address);
  tee_grpc_server->Wait();
}

void RunPerfGrpcServer(std::string server_address, KVBClientPool &pool,
                       const ILocalKeyValueStorageReadOnly *ro_storage,
                       SubBufferList &subscriber_list, int max_num_threads) {
  Logger logger = Logger::getInstance("concord.perf.grpc");

  auto perfService =
      std::make_unique<concord::performance::PerformanceServiceImp>(pool);
  auto thinReplicaServiceImpl =
      std::make_unique<ThinReplicaImpl>(ro_storage, subscriber_list);
  auto thinReplicaService =
      std::make_unique<ThinReplicaService>(std::move(thinReplicaServiceImpl));

  grpc::ResourceQuota quota;
  quota.SetMaxThreads(max_num_threads);

  grpc::ServerBuilder builder;
  builder.SetResourceQuota(quota);
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(perfService.get());
  builder.RegisterService(thinReplicaService.get());

  perf_grpc_server = unique_ptr<grpc::Server>(builder.BuildAndStart());

  LOG_INFO(logger, "Performance gRPC server listening on " << server_address);
  perf_grpc_server->Wait();
}

/*
 * Check whether one and only one out of all provided booleans is true.
 */
bool OnlyOneTrue(bool a, bool b, bool c, bool d, bool e) {
  return (a + b + c + d + e) == 1;
}

std::shared_ptr<concord::utils::PrometheusRegistry>
initialize_prometheus_metrics(ConcordConfiguration &config, Logger &logger) {
  uint16_t prometheus_port = config.hasValue<uint16_t>("prometheus_port")
                                 ? config.getValue<uint16_t>("prometheus_port")
                                 : 9891;
  uint64_t dump_time_interval =
      config.hasValue<uint64_t>("dump_metrics_interval_sec")
          ? config.getValue<uint64_t>("dump_metrics_interval_sec")
          : 600;
  std::string host = config.getValue<std::string>("service_host");
  std::string prom_bindaddress = host + ":" + std::to_string(prometheus_port);
  LOG_INFO(logger, "prometheus metrics address is: "
                       << prom_bindaddress
                       << " dumping metrics interval: " << dump_time_interval);
  return std::make_shared<concord::utils::PrometheusRegistry>(
      prom_bindaddress, dump_time_interval);
}

static concordUtils::Status create_perf_genesis_block(
    IReplica *replica, ConcordConfiguration &nodeConfig, Logger logger) {
  if (replica->getReadOnlyStorage().getLastBlock() > 0) {
    LOG_INFO(logger, "Blocks already loaded, skipping genesis");
    return concordUtils::Status::OK();
  }

  string genesis_string = "performance execution engine genesis";
  if (nodeConfig.hasValue<std::string>("genesis_block")) {
    const auto &genesis_file_path =
        nodeConfig.getValue<std::string>("genesis_block");
    std::ifstream genesis_stream(genesis_file_path);
    if (!genesis_stream.good()) {
      LOG_WARN(logger, "Error reading genesis file at " << genesis_file_path);
    } else {
      nlohmann::json genesis_block;
      genesis_stream >> genesis_block;
      genesis_string = genesis_block.dump();
    }
  }

  const auto &key = concord::kvbc::Key(
      std::string({concord::storage::kKvbKeyAdminIdentifier}));
  const auto &status = replica->addBlockToIdleReplica(
      SetOfKeyValuePairs{{key, std::string{genesis_string}}});

  if (status.isOK()) {
    LOG_INFO(logger, "Successfully loaded performance genesis block");
  } else {
    throw std::runtime_error("Unable to add performance genesis block");
  }

  return concordUtils::Status::OK();
}

/*
 * Start the service that listens for connections from Helen.
 */
int run_service(ConcordConfiguration &config, ConcordConfiguration &nodeConfig,
                Logger &logger) {
  unique_ptr<EVM> concevm;
  unique_ptr<EthSign> ethVerifier;
  EVMInitParams params;
  uint64_t chainID;

  // List of ring buffers (one per subscriber) for thin replica subscription
  // service.
  SubBufferList subscriber_list;

  bool daml_enabled = config.getValue<bool>("daml_enable");
  bool hlf_enabled = config.getValue<bool>("hlf_enable");
  bool eth_enabled = config.getValue<bool>("eth_enable");
  bool tee_enabled = config.getValue<bool>("tee_enable");
  bool perf_enabled = config.getValue<bool>("perf_enable");

  if (!OnlyOneTrue(daml_enabled, hlf_enabled, eth_enabled, tee_enabled,
                   perf_enabled)) {
    LOG_WARN(logger,
             "Make sure one and only one execution engine (DAML, Eth, "
             "HLF, TEE or Perf) is set");
    return 0;
  }
  auto prometheus_registry = initialize_prometheus_metrics(nodeConfig, logger);
  auto prometheus_for_concordbft =
      std::make_shared<concord::utils::ConcordBftPrometheusCollector>();
  prometheus_registry->scrapeRegistry(prometheus_for_concordbft);
  auto bft_stat_collector =
      std::make_shared<concord::utils::ConcordBftStatisticsCollector>();
  prometheus_registry->scrapeRegistry(bft_stat_collector);

  try {
    if (eth_enabled) {
      // The genesis parsing is Eth specific.
      if (nodeConfig.hasValue<std::string>("genesis_block")) {
        string genesis_file_path =
            nodeConfig.getValue<std::string>("genesis_block");
        LOG_INFO(logger, "Reading genesis block from " << genesis_file_path);
        params = EVMInitParams(genesis_file_path);
        chainID = params.get_chainID();
        // throws an exception if it fails
        concevm = unique_ptr<EVM>(new EVM(params));
        ethVerifier = unique_ptr<EthSign>(new EthSign());
      } else {
        LOG_WARN(logger, "No genesis block provided");
      }
    }

    // Replica and communication config
    CommConfig commConfig;
    StatusAggregator sag;
    commConfig.statusCallback = sag.get_update_connectivity_fn();
    ReplicaConfig replicaConfig;

    bool success = InitializeSbftConfiguration(config, nodeConfig, &commConfig,
                                               nullptr, 0, &replicaConfig);
    assert(success);

    LOG_INFO(logger,
             "N = 3F + 2C + 1 with F=" << replicaConfig.fVal
                                       << " and C=" << replicaConfig.cVal);
    LOG_INFO(logger, "Direct proofs are not supported");

    // Replica
    //
    // TODO(IG): since ReplicaImpl is used as an implementation of few
    // interfaces, this object will be used for constructing
    // EthKvbCommandsHandler and thus we can't use IReplica here. Need to
    // restructure the code, to split interfaces implementation and to construct
    // objects in more clear way
    bft::communication::ICommunication *icomm = nullptr;
    if (commConfig.commType == "tls") {
      bft::communication::TlsTcpConfig configuration(
          commConfig.listenIp, commConfig.listenPort, commConfig.bufferLength,
          commConfig.nodes, commConfig.maxServerId, commConfig.selfId,
          commConfig.certificatesRootPath, commConfig.cipherSuite,
          commConfig.statusCallback);
      icomm = bft::communication::CommFactory::create(configuration);
    } else if (commConfig.commType == "udp") {
      bft::communication::PlainUdpConfig configuration(
          commConfig.listenIp, commConfig.listenPort, commConfig.bufferLength,
          commConfig.nodes, commConfig.selfId, commConfig.statusCallback);
      icomm = bft::communication::CommFactory::create(configuration);
    } else {
      throw std::invalid_argument("Unknown communication module type" +
                                  commConfig.commType);
    }

    std::shared_ptr<concordMetrics::Aggregator> bft_metrics_aggregator;
    if (nodeConfig.hasValue<uint16_t>("bft_metrics_udp_port")) {
      auto bft_metrics_udp_port =
          nodeConfig.getValue<uint16_t>("bft_metrics_udp_port");
      bft_metrics_server =
          std::make_unique<concordMetrics::Server>(bft_metrics_udp_port);
      bft_metrics_aggregator = bft_metrics_server->GetAggregator();
      bft_metrics_server->Start();
      LOG_INFO(logger, "Exposing BFT metrics via UDP server, listening on port "
                           << bft_metrics_udp_port);
    } else {
      bft_metrics_aggregator = prometheus_for_concordbft->getAggregator();
      LOG_INFO(logger, "Exposing BFT metrics via Prometheus.");
    }

    ReplicaImp replica(icomm, replicaConfig,
                       create_storage_factory(nodeConfig, logger),
                       bft_metrics_aggregator);
    replica.setReplicaStateSync(
        new ReplicaStateSyncImp(new ConcordBlockMetadata(replica)));

    unique_ptr<ICommandsHandler> kvb_commands_handler;
    if (daml_enabled) {
      grpc::ChannelArguments chArgs;
      chArgs.SetMaxReceiveMessageSize(kDamlServerMsgSizeMax);
      unique_ptr<DamlValidatorClient> daml_validator(new DamlValidatorClient(
          replicaConfig.replicaId,
          com::digitalasset::kvbc::ValidationService::NewStub(
              grpc::CreateCustomChannel(
                  nodeConfig.getValue<string>("daml_execution_engine_addr"),
                  grpc::InsecureChannelCredentials(), chArgs))
              .release()));
      kvb_commands_handler =
          unique_ptr<ICommandsHandler>(new DamlKvbCommandsHandler(
              config, nodeConfig, replica, replica, replica,
              replica.getStateTransfer(), subscriber_list,
              std::move(daml_validator), prometheus_registry));
      const auto &status =
          create_daml_genesis_block(&replica, nodeConfig, logger);
      if (status.isOK()) {
        LOG_INFO(logger, "Successfully loaded DAML genesis block");
      } else {
        throw concord::daml::DamlInitParamException(
            "Unable to load DAML genesis block: " + status.toString());
      }
    } else if (hlf_enabled) {
      LOG_INFO(logger, "Hyperledger Fabric feature is enabled");

      // HLF
      //
      // Time service feature is causing HLF chaincode transactions
      // to hang in consensus. FEATURE_time_service needs to be set to false,
      // this is currently hardcoded for concord deployment as its the only
      // custom parameter
      // TODO(JB): debug issue & create task for time service with HLF
      if (concord::time::IsTimeServiceEnabled(config)) {
        LOG_WARN(
            logger,
            "Time Service Enabled ignored..not supported with HLF enabled");
        config.loadValue("FEATURE_time_service", "false");
      }

      assert(!concord::time::IsTimeServiceEnabled(config));

      // Init chaincode invoker
      ChaincodeInvoker *chaincode_invoker = new ChaincodeInvoker(nodeConfig);

      kvb_commands_handler =
          unique_ptr<ICommandsHandler>(new HlfKvbCommandsHandler(
              chaincode_invoker, config, nodeConfig, replica, replica, replica,
              replica.getStateTransfer(), subscriber_list,
              prometheus_registry));
    } else if (tee_enabled) {
      kvb_commands_handler = unique_ptr<ICommandsHandler>(
          new TeeCommandsHandler(config, nodeConfig, replica, replica, replica,
                                 replica.getStateTransfer(), subscriber_list,
                                 prometheus_registry));

      auto should_create_tee_genesis_block = [&config]() {
        // if the parameter is not set - create the block
        if (!config.hasValue<bool>("create_tee_genesis_block"))
          return true;
        else
          return config.getValue<bool>("create_tee_genesis_block");
      };

      if (should_create_tee_genesis_block())
        create_tee_genesis_block(&replica, nodeConfig, logger);
    } else if (perf_enabled) {
      kvb_commands_handler = unique_ptr<ICommandsHandler>(
          new concord::performance::PerformanceCommandsHandler(
              config, nodeConfig, replica, replica, replica,
              replica.getStateTransfer(), subscriber_list,
              prometheus_registry));
      create_perf_genesis_block(&replica, nodeConfig, logger);
    } else {
      assert(eth_enabled);
      kvb_commands_handler =
          unique_ptr<ICommandsHandler>(new EthKvbCommandsHandler(
              *concevm, *ethVerifier, config, nodeConfig, replica, replica,
              replica, replica.getStateTransfer(), subscriber_list,
              prometheus_registry));
      // Genesis must be added before the replica is started.
      concordUtils::Status genesis_status =
          create_ethereum_genesis_block(&replica, params, logger);
      if (!genesis_status.isOK()) {
        LOG_FATAL(logger, "Unable to load genesis block: " << genesis_status);
        throw EVMException("Unable to load genesis block");
      }
    }

    replica.set_command_handler(kvb_commands_handler.get());
    replica.start();

    // Start the diagnostics server
    concord::diagnostics::Server diagnostics_server;
    diagnostics_server.start(
        concord::diagnostics::RegistrarSingleton::getInstance());

    // Clients

    std::shared_ptr<TimePusher> timePusher;
    if (concord::time::IsTimeServiceEnabled(config)) {
      timePusher.reset(new TimePusher(config, nodeConfig));
    }

    std::vector<KVBClient *> clients;

    milliseconds clientTimeout(
        nodeConfig.getValue<uint64_t>("bft_client_timeout_ms"));
    for (uint16_t i = 0;
         i < config.getValue<uint16_t>("client_proxies_per_replica"); ++i) {
      ClientConfig clientConfig;
      CommConfig clientCommConfig;
      bool success = InitializeSbftConfiguration(
          config, nodeConfig, &clientCommConfig, &clientConfig, i, nullptr);
      assert(success);

      bft::communication::ICommunication *comm = nullptr;
      if (commConfig.commType == "tls") {
        bft::communication::TlsTcpConfig config(
            clientCommConfig.listenIp, clientCommConfig.listenPort,
            clientCommConfig.bufferLength, clientCommConfig.nodes,
            clientCommConfig.maxServerId, clientCommConfig.selfId,
            clientCommConfig.certificatesRootPath, clientCommConfig.cipherSuite,
            clientCommConfig.statusCallback);
        comm = bft::communication::CommFactory::create(config);
      } else if (commConfig.commType == "udp") {
        bft::communication::PlainUdpConfig config(
            clientCommConfig.listenIp, clientCommConfig.listenPort,
            clientCommConfig.bufferLength, clientCommConfig.nodes,
            clientCommConfig.selfId, clientCommConfig.statusCallback);
        comm = bft::communication::CommFactory::create(config);
      } else {
        throw std::invalid_argument("Unknown communication module type" +
                                    commConfig.commType);
      }

      IClient *client = concord::kvbc::createClient(clientConfig, comm);
      client->start();
      client->setMetricsAggregator(prometheus_for_concordbft->getAggregator());
      KVBClient *kvbClient = new KVBClient(client, timePusher);
      clients.push_back(kvbClient);
    }
    KVBClientPool pool(clients, clientTimeout, timePusher, prometheus_registry);

    if (timePusher) {
      timePusher->Start(&pool);
    }

    signal(SIGINT, signalHandler);
    signal(SIGABRT, signalHandler);
    signal(SIGTERM, signalHandler);

    // API server

    if (daml_enabled) {
      std::string daml_addr{
          nodeConfig.getValue<std::string>("daml_service_addr")};

      // Limit the amount of gRPC threads
      int max_num_threads = nodeConfig.getValue<int>("daml_service_threads");

      // Spawn a thread in order to start management API server as well
      std::thread(RunDamlGrpcServer, daml_addr, std::ref(pool), config,
                  &replica, std::ref(subscriber_list), max_num_threads,
                  prometheus_registry)
          .detach();
    } else if (hlf_enabled) {
      // Get listening address for services
      std::string key_value_service_addr =
          nodeConfig.getValue<std::string>("hlf_kv_service_address");
      std::string chaincode_service_addr =
          nodeConfig.getValue<std::string>("hlf_chaincode_service_address");

      // Create Hlf Kvb Storage instance for Hlf key value service
      // key value service could put updates to cache, but it is not allowed to
      // write block
      const ILocalKeyValueStorageReadOnly &storage = replica;
      IdleBlockAppender block_appender(&replica);
      HlfKvbStorage kvb_storage = HlfKvbStorage(storage, &block_appender);

      // Start HLF gRPC services
      std::thread(RunHlfGrpcServer, std::ref(kvb_storage), std::ref(pool),
                  key_value_service_addr, chaincode_service_addr)
          .detach();
    } else if (tee_enabled) {
      std::string tee_addr{
          nodeConfig.getValue<std::string>("tee_service_addr")};

      int max_num_threads = nodeConfig.getValue<int>("tee_service_threads");

      std::thread(RunTeeGrpcServer, tee_addr, std::ref(pool), &replica,
                  std::ref(subscriber_list), max_num_threads)
          .detach();
    } else if (perf_enabled) {
      std::string perf_addr = {
          nodeConfig.getValue<std::string>("perf_service_addr")};

      int max_num_threads = nodeConfig.getValue<int>("perf_service_threads");

      std::thread(RunPerfGrpcServer, perf_addr, std::ref(pool), &replica,
                  std::ref(subscriber_list), max_num_threads)
          .detach();
    }

    // Start the Management API service. If Ethereum is enabled we also expose
    // Ethereum API through this service.
    std::string ip = nodeConfig.getValue<std::string>("service_host");
    short port = nodeConfig.getValue<short>("service_port");

    api_service = new io_service();
    tcp::endpoint endpoint(address::from_string(ip), port);
    uint64_t gasLimit = config.getValue<uint64_t>("gas_limit");
    ApiAcceptor acceptor(*api_service, endpoint, pool, sag, gasLimit, chainID,
                         eth_enabled, nodeConfig);
    LOG_INFO(logger, "API Listening on " << endpoint);

    start_worker_threads(nodeConfig.getValue<int>("api_worker_pool_size") - 1);

    // Wait for api_service->run() to return
    api_service->run();
    worker_pool.join_all();

    if (timePusher) {
      timePusher->Stop();
    }

    if (bft_metrics_server) {
      bft_metrics_server->Stop();
    }
    diagnostics_server.stop();
    replica.stop();
  } catch (std::exception &ex) {
    LOG_FATAL(logger, ex.what());
    return -1;
  }

  return 0;
}

int main(int argc, char **argv) {
  bool tracingInitialized = false;
  int result = 0;
  Logger mainLogger = Logger::getInstance("com.vmware.concord.main");
  try {
    ConcordConfiguration config;

    // Note that this must be the very first statement
    // in main function before doing any operations on config
    // parameters or 'argc/argv'. Never directly operate on
    // config parameters or command line parameters directly
    // always use po::variables_map interface for that.
    variables_map opts;
    if (!initialize_config(argc, argv, config, opts)) {
      return -1;
    }

    if (opts.count("help")) return result;

    if (opts.count("debug")) std::this_thread::sleep_for(chrono::seconds(20));

    // Get a reference to the node instance-specific configuration for the
    // current running Concord node because that is needed frequently and we do
    // not want to have to determine the current node every time.
    size_t nodeIndex = detectLocalNode(config);
    ConcordConfiguration &nodeConfig = config.subscope("node", nodeIndex);

    // Initialize logger
    LOG_CONFIGURE_AND_WATCH(nodeConfig.getValue<std::string>("logger_config"),
                            nodeConfig.getValue<int>("logger_reconfig_time"));

    // re-initialize logger after configuration
    mainLogger = Logger::getInstance("com.vmware.concord.main");
    // say hello
    LOG_INFO(mainLogger, "VMware Project concord starting");

    initialize_tracing(nodeConfig, mainLogger);
    tracingInitialized = true;
    // actually run the service - when this call returns, the
    // service has shutdown
    result = run_service(config, nodeConfig, mainLogger);

    LOG_INFO(mainLogger, "VMware Project concord halting");
  } catch (const error &ex) {
    LOG_FATAL(mainLogger, ex.what());
    result = -1;
  }

  if (tracingInitialized) {
    opentracing::Tracer::Global()->Close();
  }

  LOG_INFO(mainLogger, "Shutting down");

  return result;
}
