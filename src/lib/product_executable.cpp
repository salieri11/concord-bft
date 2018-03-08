/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <cstring>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <json/json.h>
#include <sys/wait.h>
#include <unistd.h>

#include "hermes/include/lib/product_executable.h"
#include "hermes/include/lib/system_calls.h"

#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>

using namespace std;

const string ProductExecutable::CMD_KEY = "launchCommand";
const string ProductExecutable::PARAMS_KEY = "parameters";

/**
 * Creates a product executable object from a JSON object.
 **/
ProductExecutable::ProductExecutable(Json::Value config){
   m_command = config[ProductExecutable::CMD_KEY].asString();
   m_command = expandShellVariables(m_command);

   Json::Value jsonParams = config[ProductExecutable::PARAMS_KEY];
   // +1 because the parameters must start with the command.
   // +1 because the last parameter must be a null pointer.
   int numParameters = jsonParams.size() + 2;
   m_parameters = new char*[numParameters];

   m_parameters[0] = new char[m_command.length() + 1];
   memset(m_parameters[0], 0, m_command.length() + 1);
   memcpy(m_parameters[0], m_command.c_str(), m_command.length());

   for (int i = 0; i < jsonParams.size(); i++){
      string param = jsonParams[i].asString();
      param = expandShellVariables(param);

      int paramLength = param.size();
      m_parameters[i + 1] = new char[paramLength + 1];
      memset(m_parameters[i + 1], 0, paramLength + 1);
      memcpy(m_parameters[i + 1], param.c_str(), paramLength);
   }

   m_parameters[numParameters - 1] = 0;
   //m_logFile = Upcoming checkin.
}

/**
 * Launches the executable and saves the process id.
 **/
void ProductExecutable::launch(){
   m_procId = fork();

   if(m_procId == 0) {
      // An upcoming checkin will address having a log directory
      // for product logs.  For now, all output gets spewed onto
      // the user's console, and also leaves the console needing
      // a reset.  Yuck.  But this code seems to fix that.
      // int logFile = open(m_logFile, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
      // dup2(logFile, 1);
      // dup2(logFile, 2);
      // close(logFile);

      execv(m_command.c_str(), (char* const*)m_parameters);
   }
}

/**
 * Kills the executable via its process id.
 * We use SIGTERM.  Since we're testing software under development,
 * we might switch to SIGKILL if it misbehaves.
 **/
void ProductExecutable::stop(){
   int status;
   pid_t checkedProcId = waitpid(m_procId, &status, WNOHANG);

   if (checkedProcId == 0){
      kill(m_procId, SIGTERM);
   }
}
