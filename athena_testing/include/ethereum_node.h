/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#pragma once

#include <string>
#include "node_base.h"

using namespace std;

/**
 * Represents an official Ethereum node.
 **/
class EthereumNode: public NodeBase{
public:
   EthereumNode();
   ~EthereumNode();
   string makeCall();
};
