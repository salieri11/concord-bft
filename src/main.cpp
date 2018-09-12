// Copyright 2018 VMware, all rights reserved
//
// Athena node startup.

#include <iostream>
#include <csignal>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include "common/utils.hpp"
#include "common/athena_eth_sign.hpp"
#include "api_acceptor.hpp"
#include "athena_evm.hpp"
#include "athena_kvb.hpp"
#include "athena_exception.hpp"
#include "configuration_manager.hpp"
#include "evm_init_params.hpp"
#include "kvb/DatabaseInterface.h"
#include "kvb/BlockchainDBAdapter.h"
#include "kvb/ReplicaImp.h"
#include "kvb/Comparators.h"
#include "kvb/InMemoryDBClient.h"
#include <thread>
#include <string>
#include "status_aggregator.hpp"
#include "kvb/bft_configuration.hpp"

#ifdef USE_ROCKSDB
#include "kvb/RocksDBClient.h"
#endif

using namespace boost::program_options;
using boost::asio::ip::tcp;
using boost::asio::ip::address;
using boost::asio::io_service;
using log4cplus::Logger;

using namespace com::vmware::athena;
using namespace std;
using namespace Blockchain;

// the Boost service hosting our Helen connections
static io_service *api_service;
static boost::thread_group worker_pool;

void signalHandler(int signum) {
   try {
      Logger logger = Logger::getInstance("com.vmware.athena.main");
      LOG4CPLUS_INFO(logger, "Signal received (" << signum <<
                     "), stopping service");

      api_service->stop();
   } catch (exception &e) {
      cout << "Exception in signal handler: " << e.what() << endl;
   }
}

Blockchain::IDBClient* open_database(variables_map &opts, Logger logger)
{
   if (opts.count("blockchain_db_impl") < 1) {
      LOG4CPLUS_FATAL(logger, "Missing blockchain_db_impl config");
      throw EVMException("Missing blockchain_db_impl config");
   }

   string db_impl_name = opts["blockchain_db_impl"].as<std::string>();
   if (db_impl_name == "memory") {
      LOG4CPLUS_INFO(logger, "Using memory blockchain database");
      return new Blockchain::InMemoryDBClient(
         (Blockchain::IDBClient::KeyComparator)&Blockchain::InMemKeyComp);
#ifdef USE_ROCKSDB
   } else if (db_impl_name == "rocksdb") {
      LOG4CPLUS_INFO(logger, "Using rocksdb blockchain database");
      string rocks_path = opts["blockchain_db_path"].as<std::string>();
      return new Blockchain::RocksDBClient(
         rocks_path,
         new Blockchain::RocksKeyComparator());
#endif
   } else {
      LOG4CPLUS_FATAL(logger, "Unknown blockchain_db_impl " << db_impl_name);
      throw EVMException("Unknown blockchain_db_impl");
   }
}

/**
 * IdleBlockAppender is a shim to wrap IReplica::addBlocktoIdleReplica in an
 * IBlocksAppender interface, so that it can be rewrapped in a KVBStorage
 * object, thus allowing the create_genesis_block function to use the same
 * functions as athena_evm to put data in the genesis block.
 */
class IdleBlockAppender : public Blockchain::IBlocksAppender {
private:
   Blockchain::IReplica *replica_;

public:
   IdleBlockAppender(Blockchain::IReplica *replica)
      : replica_(replica) { }

   virtual Blockchain::Status addBlock(
      const Blockchain::SetOfKeyValuePairs &updates,
      Blockchain::BlockId& outBlockId) override
   {
      outBlockId = 0; // genesis only!
      return replica_->addBlockToIdleReplica(updates);
   }
};

/**
 * Create the initial transactions and a genesis block based on the
 * genesis file.
 */
Blockchain::Status create_genesis_block(Blockchain::IReplica *replica,
                                        EVMInitParams params,
                                        Logger logger)
{
   const Blockchain::ILocalKeyValueStorageReadOnly &storage =
      replica->getReadOnlyStorage();
   IdleBlockAppender blockAppender(replica);
   KVBStorage kvbStorage(storage, &blockAppender);

   if (storage.getLastBlock() > 0) {
      LOG4CPLUS_INFO(logger, "Blocks already loaded, skipping genesis");
      return Blockchain::Status::OK();
   }

   std::map<evm_address, uint64_t> genesis_acts = params.get_initial_accounts();
   uint64_t nonce = 0;
   for (std::map<evm_address,uint64_t>::iterator it = genesis_acts.begin();
	it != genesis_acts.end();
	++it) {

      // store a transaction for each initial balance in the genesis block
      // defintition
      EthTransaction tx = {
         nonce,                  // nonce
         zero_hash,              // block_hash: will be set in write_block
         0,                      // block_number
         zero_address,           // from
         it->first,              // to
         zero_address,           // contract_address
         std::vector<uint8_t>(), // input
         EVM_SUCCESS,            // status
         it->second,             // value
         0,                      // gas_price
         0,                      // gas_limit
         zero_hash,              // sig_r (no signature for genesis)
         zero_hash,              // sig_s (no signature for genesis)
         0                       // sig_v TODO: chain ID?
      };
      evm_uint256be txhash = tx.hash();
      LOG4CPLUS_INFO(logger, "Created genesis transaction " << txhash <<
                     " to address " << it->first <<
                     " with value = " << tx.value);
      kvbStorage.add_transaction(tx);

      // also set the balance record
      kvbStorage.set_balance(it->first, it->second);
      nonce++;
   }
   kvbStorage.set_nonce(zero_address, nonce);

   uint64_t timestamp = params.get_timestamp();

   // Genesis is always proposed and accepted at the same time.
   return kvbStorage.write_block(timestamp);
}


/*
 * Starts a set of worker threads which will call api_service.run() method.
 * This will allow us to have multiple threads accepting tcp connections
 * and passing the requests to KVBClient.
 */
void
start_worker_threads(int number) {
   Logger logger = Logger::getInstance("com.vmware.athena.main");
   LOG4CPLUS_INFO(logger, "Starting " << number << " new API worker threads");
   for (int i = 0; i < number; i++) {
      boost::thread *t = new boost::thread(
         boost::bind(&boost::asio::io_service::run, api_service));
      worker_pool.add_thread(t);
   }
}


/*
 * Start the service that listens for connections from Helen.
 */
int
run_service(variables_map &opts, Logger logger)
{
   EVMInitParams params;

   try {
      // If genesis block option was provided then read that so
      // it can be passed during EVM creation
      if (opts.count("genesis_block")) {
         string genesis_file_path = opts["genesis_block"].as<std::string>();
         LOG4CPLUS_INFO(logger, "Reading genesis block from " <<
                        genesis_file_path);
         params = EVMInitParams(genesis_file_path);
      } else {
         LOG4CPLUS_WARN(logger, "No genesis block provided");
      }

      Blockchain::IDBClient *dbclient = open_database(opts, logger);
      Blockchain::BlockchainDBAdapter db(dbclient);

      /// replica and comm config init
      Blockchain::CommConfig commConfig;
      Blockchain::ReplicaConsensusConfig replicaConsensusConfig;
      ///TODO(IG): check return value and shutdown athena if false
      parse_plain_config_file(opts["SBFT.replica"].as<std::string>(),
                              opts["SBFT.public"].as<std::string>(),
                              &commConfig,
                              nullptr,
                              &replicaConsensusConfig);

      /* init replica
       * TODO(IG): since ReplicaImpl is used as an implementation of few
       * intefaces, this object will be used for constructing KVBCommandsHandler
       * and thus we cant use IReplica here. Need to restructure the code, to
       * split interfaces implementation and to construct objects in more
       * clear way
       */
      StatusAggregator sag;
      Blockchain::ReplicaImp *replica = dynamic_cast<Blockchain::ReplicaImp*>(
              Blockchain::createReplica(commConfig,
                                                    replicaConsensusConfig,
                                                    dbclient));

      // throws an exception if it fails
      EVM athevm(params);
      EthSign verifier;
      KVBCommandsHandler athkvb(
              athevm,
              verifier,
              opts,
              replica,
              replica);
      replica->set_command_handler(&athkvb);

      // Genesis must be added before the replica is started.
      Blockchain::Status genesis_status =
              create_genesis_block(replica, params, logger);
      if (!genesis_status.ok()) {
         LOG4CPLUS_FATAL(logger, "Unable to load genesis block: " <<
                                 genesis_status.ToString());
         throw EVMException("Unable to load genesis block");
      }

      /// start replica
      replica->start();

      /// init and start clients pool
      std::vector<KVBClient*> clients;
      std::vector<std::string> clientConfigs =
         opts["SBFT.client"].as<std::vector<std::string>>();


      for (auto it = clientConfigs.begin(); it != clientConfigs.end(); it++) {
         Blockchain::ClientConsensusConfig clientConsensusConfig;
         ///TODO(IG): check return value and shutdown athena if false
         CommConfig clientCommConfig;
         parse_plain_config_file(*it,
                                 opts["SBFT.public"].as<std::string>(),
                                 &clientCommConfig,
                                 &clientConsensusConfig,
                                 nullptr);
         Blockchain::IClient *client =
            Blockchain::createClient(clientCommConfig, clientConsensusConfig);
         client->start();
         KVBClient *kvbClient = new KVBClient(client);
         clients.push_back(kvbClient);
      }

      KVBClientPool pool(clients);

      FilterManager filterManager;

      std::string ip = opts["ip"].as<std::string>();
      short port = opts["port"].as<short>();

      api_service = new io_service();
      tcp::endpoint endpoint(address::from_string(ip), port);
      api_acceptor acceptor(*api_service,
                            endpoint,
                            filterManager,
                            pool,
                            sag);

      signal(SIGINT, signalHandler);

      LOG4CPLUS_INFO(logger, "Listening on " << endpoint);
      // start worker thread pool first before calling api_service->run()
      // consider 1 main thread
      start_worker_threads(opts["api_worker_pool_size"].as<int>() - 1);
      // Wait for api_service->run() to return
      api_service->run();
      // wait for all threads to join
      worker_pool.join_all();

      // If we return from `run`, the service was stopped and we are shutting
      // down.

      /// replica
      replica->stop();
      replica->wait();

      Blockchain::release(replica);
      ////////////////////
   } catch (std::exception &ex) {
      LOG4CPLUS_FATAL(logger, ex.what());
      return -1;
   }

   return 0;
}

int
main(int argc, char** argv)
{
   bool loggerInitialized = false;
   int result = 0;

   try {
      // Note that this must be the very first statement
      // in main function before doing any operations on config
      // parameters or 'argc/argv'. Never directly operate on
      // config parameters or command line parameters directly
      // always use po::variables_map interface for that.
      variables_map opts = initialize_config(argc, argv);

      if (opts.count("help"))
         return result;

      if (opts.count("debug"))
         std::this_thread::sleep_for(chrono::seconds(20));

      // Initialize logger
      log4cplus::initialize();
      log4cplus::ConfigureAndWatchThread
         configureThread(opts["logger_config"].as<string>(),
                         opts["logger_reconfig_time"].as<int>());
      loggerInitialized = true;

      // say hello
      Logger mainLogger = Logger::getInstance("com.vmware.athena.main");
      LOG4CPLUS_INFO(mainLogger, "VMware Project Athena starting");


      // actually run the service - when this call returns, the
      // service has shutdown
      result = run_service(opts, mainLogger);

      LOG4CPLUS_INFO(mainLogger, "VMware Project Athena halting");
   } catch (const error &ex) {
      if (loggerInitialized) {
         Logger mainLogger = Logger::getInstance("com.vmware.athena.main");
         LOG4CPLUS_FATAL(mainLogger, ex.what());
      } else {
         std::cerr << ex.what() << std::endl;
      }
      result = -1;
   }

   if (loggerInitialized) {
      Logger mainLogger = Logger::getInstance("com.vmware.athena.main");
      LOG4CPLUS_INFO(mainLogger, "Shutting down");
   }

   // cleanup required for properties-watching thread
   log4cplus::Logger::shutdown();

   return result;
}
