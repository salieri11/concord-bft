#pragma once

#include <json/json.h>

using namespace std;

/**
 * Handles the processes for parts of the product that have
 * to be launched.
 * e.g. P2_Blockchain replicas, P2_Blockchain clients, and helen.
 **/
class ProductExecutable{
public:
  static const string CMD_KEY;
  static const string DIR_KEY;
  static const string PARAMS_KEY;

  ProductExecutable();
  ProductExecutable(int i);
  ProductExecutable(Json::Value config);
  void launch();
  void stop();

private:
  string m_command;
  char** m_parameters;
  int m_numParameters;
  pid_t m_procId;
};

