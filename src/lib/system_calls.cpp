/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <iostream>
#include <stdio.h>
#include <string>
#include <cstring>
#include <wordexp.h>

#include "hermes/include/lib/system_calls.h"

using namespace std;

/**
 * Given a command, such as curl, runs it and returns the output as a string.
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


/**
 * Given a string, return a new string with the shell variables expanded.
 * This will help with the fact that build locations may be provided as
 * ~/vmwathena/...
 * Raises an exception on error, except the error about an illegal character.
 * Error strings are copied from the man page.
 **/
string expandShellVariables(string expandMe){
   wordexp_t resultStruct;

   int resultCode =  wordexp(expandMe.c_str(), &resultStruct, WRDE_SHOWERR);

   try{
      switch(resultCode){
      case WRDE_BADCHAR:
         throw(string("Illegal occurrence of newline or one of |, &, ;, <, >, "
                      "(, ), {, }."));
         break;
      case WRDE_BADVAL:
         throw(string("An undefined shell variable was referenced, and the "
                      "WRDE_UNDEF flag told us to consider this an error."));
         break;
      case WRDE_CMDSUB:
         throw(string("Command substitution requested, but the WRDE_NOCMD flag "
                      "told us to consider this an error."));
         break;
      case WRDE_NOSPACE:
         throw(string("Out of memory."));
         break;
      case WRDE_SYNTAX:
         throw(string("Shell syntax error, such as unbalanced parentheses or "
                      "unmatched quotes."));
         break;
      default:
         int retLength = strlen(resultStruct.we_wordv[0]);
         char* expanded = new char[retLength + 1];
         string ret(resultStruct.we_wordv[0]);
         wordfree(&resultStruct);
         return ret;
      }
   }catch(string error){
      string msg("Error expanding shell parameter '");
      msg.append(expandMe).append("': '");
      msg.append(error);
      msg.append("'.");

      if (resultCode == WRDE_BADCHAR){
         // We use a few of of these; it's fine.
         msg.append(" Illegal character occurrences can usually be ignored.");
         cout << msg << endl; // Maybe pass in a logger for core library stuff.
         return expandMe;
      }else{
         msg.append(" This is critical and an execption is being raised.");
         cout << msg << endl; // Maybe pass in a logger for core library stuff.
         throw(msg);
      }
   }
}
