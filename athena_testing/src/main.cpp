//#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

#include "athena_testing/include/ethereum_node.h"
#include "athena_testing/include/node_base.h"
#include "athena_testing/include/vmware_node.h"

#include "hermes/include/system_calls.h"

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
int main()
{
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

  cout << "Done" << endl;

  // Placeholder.  This will be an Ethereum RPC call.
  string command = "curl http://build-squid.eng.vmware.com/build/mts/release/bora-7802939/publish/MD5SUM.txt 2>&1";

  try{
    cout << "Running command '" << command << "'" << endl;
    string result = makeExternalCall(command);
    cout << result << endl;
  }catch(string e){
    cerr << e << endl;
  }
  return 0;
}
