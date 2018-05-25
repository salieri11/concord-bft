// Copyright 2018 VMware, all rights reserved
//
// Athena node startup.

#include <iostream>
#include <csignal>
#include <boost/program_options.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include "common/utils.hpp"
#include "api_acceptor.hpp"
#include "athena_evm.hpp"
#include "athena_kvb.hpp"
#include "athena_exception.hpp"
#include "configuration_manager.hpp"
#include "evm_init_params.hpp"
#include "kvb/DatabaseInterface.h"
#include "kvb/BlockchainDBAdapter.h"
#include "kvb/Comparators.h"
#include "kvb/InMemoryDBClient.h"
#include "kvb/ReplicaImp.h"
#include "kvb/ClientImp.h"
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
      throw new EVMException("Missing blockchain_db_impl config");
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
      throw new EVMException("Unknown blockchain_db_impl");
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
void create_genesis_block(Blockchain::IReplica *replica,
                          EVMInitParams params,
                          Logger logger)
{
   const Blockchain::ILocalKeyValueStorageReadOnly &storage =
      replica->getReadOnlyStorage();
   IdleBlockAppender blockAppender(replica);
   KVBStorage kvbStorage(storage, &blockAppender);

   if (storage.getLastBlock() > 0) {
      LOG4CPLUS_INFO(logger, "Blocks already loaded, skipping genesis");
      return;
   }

   std::map<evm_address, uint64_t> genesis_acts = params.get_initial_accounts();
   for (std::map<evm_address,uint64_t>::iterator it = genesis_acts.begin();
	it != genesis_acts.end();
	++it) {

      // store a transaction for each initial balance in the genesis block
      // defintition
      EthTransaction tx{
      nonce : 0,
            block_hash : zero_hash, // set to zero for now
            block_number : 0,
            from : zero_address,
            to : it->first,
            contract_address : zero_address,
            input : std::vector<uint8_t>(),
            status : EVM_SUCCESS,
            value : it->second
            };
      evm_uint256be txhash = tx.hash();
      LOG4CPLUS_INFO(logger, "Created genesis transaction " << txhash <<
                     " to address " << it->first <<
                     " with value = " << tx.value);
      kvbStorage.add_transaction(tx);

      // also set the balance record
      kvbStorage.set_balance(it->first, it->second);
   }

   kvbStorage.write_block();
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

      // throws an exception if it fails
      EVM athevm(params);
      KVBCommandsHandler athkvb(athevm);

      // TODO(BWF): This works because this thread is going to be the same one
      // that calls the replica (athena is single-threaded).
      Blockchain::ReplicaConsensusConfig replicaConsensusConfig;
      replicaConsensusConfig.byzConfig = "TODO(BWF):actualconfig";
      replicaConsensusConfig.byzPrivateConfig = "TODO(BWF):actualconfig";
      Blockchain::IReplica *replica =
         Blockchain::createReplica(replicaConsensusConfig, &athkvb, dbclient);
      create_genesis_block(replica, params, logger);
      replica->start();

      Blockchain::ClientConsensusConfig clientConsensusConfig;
      clientConsensusConfig.byzConfig = "TODO(BWF):actualconfig";
      clientConsensusConfig.byzPrivateConfig = "TODO(BWF):actualconfig";
      Blockchain::IClient *client =
         Blockchain::createClient(clientConsensusConfig);
      client->start();
      KVBClient kvbClient(client);

      FilterManager filterManager;

      std::string ip = opts["ip"].as<std::string>();
      short port = opts["port"].as<short>();

      api_service = new io_service();
      tcp::endpoint endpoint(address::from_string(ip), port);
      api_acceptor acceptor(*api_service,
                            endpoint,
                            filterManager,
                            kvbClient);

      signal(SIGINT, signalHandler);

      LOG4CPLUS_INFO(logger, "Listening on " << endpoint);
      api_service->run();

      // If we return from `run`, the service was stopped and we are shutting
      // down.

      client->stop();
      Blockchain::release(client);

      replica->stop();
      replica->wait();
      Blockchain::release(replica);

      //TODO: Maybe combine all these catches as we are not doing any
      //specific error handling for either
   } catch (EVMInitParamException &ex) {
      LOG4CPLUS_FATAL(logger, ex.what());
      return -1;
   } catch (EVMException &ex) {
      LOG4CPLUS_FATAL(logger, ex.what());
      return -1;
   } catch (ReplicaInitException &ex) {
      LOG4CPLUS_FATAL(logger, ex.what());
      return -1;
   }

   return 0;
}

int
main(int argc, char** argv)
{
   bool loggerInitialized = true;
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
