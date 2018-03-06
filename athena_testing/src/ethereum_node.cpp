/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <iostream>
#include <string>

#include "athena_testing/include/ethereum_node.h"

using namespace std;

/**
 * Create a data directory with the test's name.
 * Initialize with users and ether.
 **/
EthereumNode::EthereumNode(){
  cout << "EthereumNode constructor" << endl;
}


EthereumNode::~EthereumNode(){
  cout << "EtherumNode destructor" << endl;
}


string EthereumNode::makeCall(){
  cout << "EthereumNode makeCall()" << endl;
  return "";
}


