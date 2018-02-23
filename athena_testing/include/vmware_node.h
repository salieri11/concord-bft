#pragma once

#include "node_base.h"

using namespace std;

/**
 * Represents a VMware Ethereum node.
 **/
class VMwareNode: public NodeBase{
public:
  VMwareNode();
  ~VMwareNode();
  string makeCall();
};

