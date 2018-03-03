#include <cstring>
#include <json/json.h>
#include <sys/wait.h>
#include <unistd.h>

#include "hermes/include/lib/product_executable.h"

using namespace std;

const string ProductExecutable::CMD_KEY = "launchCommand";
const string ProductExecutable::PARAMS_KEY = "parameters";

/**
 * Creates a product executable object from a JSON object.
 **/
ProductExecutable::ProductExecutable(Json::Value config){
  m_command = config[ProductExecutable::CMD_KEY].asString();

  Json::Value jsonParams = config[ProductExecutable::PARAMS_KEY];
  // +1 because the parameters must start with the command.
  // +1 because the last parameter must be a null pointer.
  m_numParameters = jsonParams.size() + 2;
  m_parameters = new char*[m_numParameters];
  
  m_parameters[0] = new char[m_command.length() + 1];
  memset(m_parameters[0], 0, m_command.length() + 1);
  memcpy(m_parameters[0], m_command.c_str(), m_command.length());

  for (int i = 0; i < jsonParams.size(); i++){
    int paramLength = jsonParams[i].asString().size();
    m_parameters[i + 1] = new char[paramLength + 1];
    memset(m_parameters[i + 1], 0, paramLength + 1);
    memcpy(m_parameters[i + 1], jsonParams[i].asString().c_str(), paramLength);
  }

  m_parameters[m_numParameters - 1] = 0;
}

/**
 * Launches the executable and saves the process id.
 * Throws an exception if the executable could not be launched.
 **/
void ProductExecutable::launch(){
  m_procId = fork();

  if(m_procId == 0) {
    execv(m_command.c_str(), (char* const*)m_parameters);
    int execError = errno;
    string error = "Error launching ";
    string cmd(m_command);
    
    error.append("'").append(cmd).append("'");

    if (m_parameters){
      error.append(" with parameters ");
      
      for(int i = 0; i < m_numParameters; i++){
	string param(m_parameters[i]);
	error.append("'").append(param).append("'");
      }
    }

    error.append(", Error: ").append(strerror(execError));
    throw(error);
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
