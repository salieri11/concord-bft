// Copyright 2018 VMware, all rights reserved
//
// Tests for KVBlockchain.

#ifndef BASICRANDOMTESTS_H
#define BASICRANDOMTESTS_H

#include "BlockchainInterfaces.h"

using namespace Blockchain;

namespace BasicRandomTests
{
   void run(IClient* client, const size_t numOfOperations);

   ICommandsHandler* commandsHandler();
}

#endif
