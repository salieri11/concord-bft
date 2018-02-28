//#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

#include "athena/athena_testing/include/ethereum_node.h"
#include "athena/athena_testing/include/node_base.h"
#include "athena/athena_testing/include/vmware_node.h"

#include "hermes/include/system_calls.h"

// log4cplus header files
#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include <cstddef>


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


static const string logger_config_file = "resources/log4cplus.properties";
int reconfig_time_ms = 5 * 1000; // Number of milli seconds after which re-read config file

int main()
{

  // Initializer logger
  log4cplus::initialize();
  log4cplus::ConfigureAndWatchThread configureThread(
						     logger_config_file,
						     reconfig_time_ms);
  log4cplus::Logger athena_test_logger = log4cplus::Logger::getInstance("athena.test.logger");

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
  string command = "curl http://build-squid.eng.vmware.com/build/mts/release/bora-7802939/publish/MD5SUM.txt 2>&1";

  try{
    LOG4CPLUS_INFO(athena_test_logger, "Running command '" + command + "'");
    string result = makeExternalCall(command);
    LOG4CPLUS_INFO(athena_test_logger, result);
  }catch(string e){
    LOG4CPLUS_WARN(athena_test_logger, e);
  }

  // Important to shutdown the logger while exiting, ConfigureAndWatchThread is still
  // running and if we exit from main without killing that thread it might result in
  // unexpected behaviour
  log4cplus::Logger::shutdown();
  return 0;
}
