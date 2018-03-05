/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <string>

using namespace std;

string constructExternalCallError(string command, int popenErrNo, int exitCode);
string makeExternalCall(string command);
