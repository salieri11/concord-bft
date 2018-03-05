/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#pragma once

#include <string>

#include "test_suite.h"

using namespace std;

class CoreVMTests: public TestSuite{
public:
   string getName();
   string run();
};
