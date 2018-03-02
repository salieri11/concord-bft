// Copyright 2018 VMware, all rights reserved
//
// Athena node startup.

#include <iostream>
#include <boost/program_options.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>

#include "api_acceptor.hpp"

using namespace boost::program_options;
using boost::asio::ip::tcp;
using boost::asio::ip::address;
using boost::asio::io_service;

using namespace com::vmware::athena;
using namespace std;

// default IP on which to listen for client connections
static const string default_listen_ip = "0.0.0.0";

// default port on which to listen for client connections
static const short default_listen_port = 5458;

// default location of logging properties file
static const string default_log_props = "./resources/log4cplus.properties";

// default period to check for logging properties changes (milliseconds)
static const int default_log_props_time_ms = 60000; // 60sec

/*
 * Start the service that listens for connections from Helen.
 */
void
start_service(variables_map &opts, log4cplus::Logger logger)
{
   std::string ip = opts["ip"].as<std::string>();
   short port = opts["port"].as<short>();

   io_service io_service;
   tcp::endpoint endpoint(address::from_string(ip), port);
   api_acceptor acceptor(io_service, endpoint);

   LOG4CPLUS_INFO(logger, LOG4CPLUS_TEXT("Listening on ") << endpoint);
   io_service.run();
}

int
main(int argc, char** argv)
{
   bool loggerInitialized = true;

   try {
      variables_map opts;
      options_description desc{"Options"};
      desc.add_options()
         ("help,h", "Print this help message")
         ("ip",
          value<std::string>()->default_value(default_listen_ip),
          "IP on which to expose the service")
         ("port,p",
          value<short>()->default_value(default_listen_port),
          "Port on which to expose the service")
         ("logger_config",
          value<string>()->default_value(default_log_props),
          "Path to logging properties file")
         ("logger_reconfig_time",
          value<int>()->default_value(default_log_props_time_ms),
          "Interval (in ms) to check for updates to logging properties file");

      store(parse_command_line(argc, argv, desc), opts);

      if (opts.count("help")) {
         std::cout << "VMware Project Athena" << std::endl;
         std::cout << desc << std::endl;
         return 0;
      }

      // call notify after checking "help", so that required
      // parameters are not required to get help (this call throws an
      // exception to exit the program if any parameters are invalid)
      notify(opts);

      // Initialize logger
      log4cplus::initialize();
      log4cplus::ConfigureAndWatchThread
         configureThread(opts["logger_config"].as<string>(),
                         opts["logger_reconfig_time"].as<int>());
      loggerInitialized = true;

      // say hello
      log4cplus::Logger mainLogger =
         log4cplus::Logger::getInstance("com.vmware.athena.main");
      LOG4CPLUS_INFO(mainLogger, "VMware Project Athena starting");

      // actually run the service - when this call returns, the
      // service has shutdown
      start_service(opts, mainLogger);

      LOG4CPLUS_INFO(mainLogger, "VMware Project Athena halting");
   } catch (const error &ex) {
      if (loggerInitialized) {
         log4cplus::Logger mainLogger =
            log4cplus::Logger::getInstance("com.vmware.athena.main");
         LOG4CPLUS_FATAL(mainLogger, ex.what());
      } else {
         std::cerr << ex.what() << std::endl;
      }

      return -1;
   }

   // cleanup required for properties-watching thread
   log4cplus::Logger::shutdown();

   return 0;
}
