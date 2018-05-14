// Copyright 2018 VMware, all rights reserved
//
// Wrappers of SBFT's environment initialization functions.

#include "libbyz.h"

namespace Blockchain {

void initEnv()
{
#if defined(_WIN32)
   initWinSock();
#endif
   initEnvironment();
}

void freeEnv()
{
   freeEnvironment();
}

}
