#include <iostream>
#include <string>

#include "athena/athena_testing/include/vmware_node.h"

using namespace std;

/**
 * Create a data directory with the test's name.
 * Initialize with users and ether.
 **/
VMwareNode::VMwareNode(){
  cout << "VMwareNode constructor" << endl;
}


VMwareNode::~VMwareNode(){
  cout << "VmwareNode destructor" << endl;
}


/**
 * Create and execute the curl command.
 **/
string VMwareNode::makeCall(){
  cout << "VMwareNode makeCall()" << endl;
  return "";
}


