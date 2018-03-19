// Copyright 2018 VMware, all rights reserved
//
// Athena node startup.

#include <iostream>
#include <csignal>
#include <boost/program_options.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>

#include "api_acceptor.hpp"
#include "athena_evm.hpp"
#include "configuration_manager.hpp"

using namespace boost::program_options;
using boost::asio::ip::tcp;
using boost::asio::ip::address;
using boost::asio::io_service;
using log4cplus::Logger;

using namespace com::vmware::athena;
using namespace std;

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

/*
 * Start the service that listens for connections from Helen.
 */
void
start_service(variables_map &opts, Logger logger)
{
   std::string ip = opts["ip"].as<std::string>();
   short port = opts["port"].as<short>();

   api_service = new io_service();
   tcp::endpoint endpoint(address::from_string(ip), port);
   api_acceptor acceptor(*api_service, endpoint);

   signal(SIGINT, signalHandler);

   LOG4CPLUS_INFO(logger, "Listening on " << endpoint);
   api_service->run();
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

      // throws an exception if it fails
      evm::init_evm();

      // actually run the service - when this call returns, the
      // service has shutdown
      start_service(opts, mainLogger);

      LOG4CPLUS_INFO(mainLogger, "VMware Project Athena halting");
   } catch (const error &ex) {
      if (loggerInitialized) {
         Logger mainLogger = Logger::getInstance("com.vmware.athena.main");
         LOG4CPLUS_FATAL(mainLogger, ex.what());
      } else {
         std::cerr << ex.what() << std::endl;
      }
      result = -1;
   } catch (evm::EVMException &ex) {
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
   com::vmware::athena::evm::stop_evm();
   log4cplus::Logger::shutdown();

   return result;
}
