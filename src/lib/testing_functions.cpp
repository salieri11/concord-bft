/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

#include "hermes/include/lib/product_executable.h"
#include "hermes/include/lib/testing_functions.h"

#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>

using namespace std;

/**
 * Given the path to a JSON config file defining the locations and parameters
 * of the product executables, launches the product and returns a vector of
 * ProductExecutable objects.
 *
 * Note that the current replica and client implementation is prototype code, so
 * we're just going to go with the default configuration that is already set up
 * in P2_Blockchain and run by runReplicasLocally and runClientsLocally. That
 * means four replicas and three clients.
 *
 * Returns a pointer to a vector of pointers to ProductExecutable objects.
 **/
vector <ProductExecutable*>* launchProduct(string launchConfigFile){
   vector <ProductExecutable*>* processes = new vector <ProductExecutable*>();
   Json::Value launchConfigs;
   ifstream launchConfigStream((const string)launchConfigFile);
   launchConfigStream >> launchConfigs;
   Json::Value::iterator it;

   for(it = launchConfigs.begin(); it != launchConfigs.end(); it++){
      string name = it.key().asString();
      Json::Value config = launchConfigs[name];
      ProductExecutable* exec = new ProductExecutable(config);
      exec->launch();
      processes->push_back(exec);

      // Hard coding a time is a temporary measure while working on
      // launching code.  We must test that things acutally start.
      // e.g. Maybe accept a function pointer, since this can launch
      // different types of things.
      // We get errors in the client logs if just launching everything
      // ASAP.
      cout << "Waiting for executable to start." << endl;
      this_thread::sleep_for(chrono::milliseconds(1000));
   }

   return processes;
}

/**
 * Stops all of the ProductExecutables launched and frees the memory used to
 * store them.
 **/
void stopProduct(vector <ProductExecutable*>* processes){
   for (int i = 0; i < processes->size(); i++){
      ProductExecutable* p = (*processes)[i];
      p->stop();
      free(p);
   }

   free(processes);
}
