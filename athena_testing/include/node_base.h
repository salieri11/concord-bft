/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#pragma once

#include <string>
#include <iostream>

using namespace std;

class NodeBase{
public:
   string s = "";
   virtual string makeCall() = 0;
};

