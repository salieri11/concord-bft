/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
//#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

#include "athena_testing/include/ethereum_node.h"
#include "athena_testing/include/node_base.h"
#include "athena_testing/include/vmware_node.h"

#include "hermes/include/lib/system_calls.h"

// log4cplus header files
#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include <cstddef>

#include <boost/program_options.hpp>
namespace po = boost::program_options;


using namespace std;

/**
 * Each test will be in a subdirectory.
 * Process:
 * - Read the subdirectories here in main().
 * - Pass each one to makeCall().
 * - makeCall() will create and execute the curl command, and
 *   return a result.
 * - Here in main(), accept the result and add it to a JSON object.
 * - When all tests are done, write the JSON to a file and exit.
 * - Either the human or the higher level test framework will evaluate the JSON.
 **/


static const string default_config_path = "./resources/log4cplus.properties";
static int default_reconfig_time = 60 * 100; // 60 seconds - default time period
// after which logger will recheck config file

int main(int argc, char **argv)
{
   try {
      po::options_description desc("Command line parameters");
      desc.add_options()
         ("help", "Print this help message")
         ("logger_config",
          po::value<string>()->default_value(default_config_path),
          "Complete path of configuration file for log4c+")
         ("logger_reconfig_time",
          po::value<int>()->default_value(default_reconfig_time),
          "Interval time (in milli seconds) after which logger should check"\
          " for changes in configuration file");

      po::variables_map opts;
      po::store(po::parse_command_line(argc, argv, desc), opts);

      if (opts.count("help")) {
         cout << desc << std::endl;
         return 0;
      }

      // Calling notify after displaying help so that required options are not
      // needed for showing help
      po::notify(opts);

      // Initializer logger
      log4cplus::initialize();
      log4cplus::ConfigureAndWatchThread configureThread(
                                       opts["logger_config"].as<string>(),
                                       opts["logger_reconfig_time"].as<int>());
      log4cplus::Logger athena_test_logger =
         log4cplus::Logger::getInstance("athena.test.log");

      // Use EthereumNode to generate expected results or to verify that the test
      // suite is internally consistent.
      // Use VMwareNode to test the product.
      EthereumNode eNode;
      VMwareNode vNode;
      NodeBase* n = &eNode;
      NodeBase* v = &vNode;
      n->makeCall();
      v->makeCall();

      eNode.makeCall();
      vNode.makeCall();

      LOG4CPLUS_INFO(athena_test_logger, "Done");

      // Placeholder.  This will be an Ethereum RPC call.
      string command = "curl http://build-squid.eng.vmware.com/build/mts/"
                       "release/bora-7802939/publish/MD5SUM.txt 2>&1";

      try{
         LOG4CPLUS_INFO(athena_test_logger, "Running command '" + command + "'");
         string result = makeExternalCall(command);
         LOG4CPLUS_INFO(athena_test_logger, result);
         cout << result << endl;
      }catch(string e){
         LOG4CPLUS_WARN(athena_test_logger, e);
      }

      // Important to shutdown the logger while exiting, ConfigureAndWatchThread
      // is still running and if we exit from main without killing that thread it
      // might result in unexpected behaviour
      log4cplus::Logger::shutdown();

   } catch (exception& er) {
      cerr << "error:" << er.what() << endl;
      return 1;
   }

   return 0;
}
