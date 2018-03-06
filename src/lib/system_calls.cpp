/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <stdio.h>
#include <string>
#include <cstring>

#include "hermes/include/lib/system_calls.h"

using namespace std;

/**
 * Given a command, such as curl, runs it.
 * Throws an exception on failure.
 **/
string makeExternalCall(string command){
   int bufferSize = 256;
   char buffer[bufferSize];
   string output = "";

   bool launched = false;
   int popenErrNo = 0;
   int exitCode = 0;
   string exitCodeStr = "";

   errno = 0;
   FILE* pipe = popen(command.c_str(), "r");

   if (pipe){
      launched = true;

      while(fgets(buffer, bufferSize, pipe) != NULL){
         output.append(buffer);
      }

      exitCode = pclose(pipe)/256;
   }else{
      popenErrNo = errno;
   }

   if (exitCode != 0 ||
       popenErrNo != 0 ||
       !launched){
      string error = constructExternalCallError(command, popenErrNo, exitCode);
      throw(error);
   }

   return output;
}


/**
 * Given the command, errno, and exit code, return an informative error string.
 **/
string constructExternalCallError(string command, int popenErrNo, int exitCode){
   string error = "Failed to run '";
   error.append(command);
   error.append("'.");

   if (popenErrNo != 0){
      error.append(" Error: '");
      error.append(strerror(popenErrNo));
      error.append("'");
   }

   if (exitCode != 0){
      error.append(" Exit code: '");
      error.append(to_string(exitCode)); // Compile with at least --std=c++11.
      error.append("'");
   }

   return error;
}
