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

   int numReplicas = 4;
   for (int i = 0; i < numReplicas; i++){
      string name("replica");
      name.append(to_string(i));
      ProductExecutable* replica = launchExecutable(launchConfigs, name);
      processes->push_back(replica);
   }

   int numClients = 3;
   for (int i = 0; i < numClients; i++){
      string name("client");
      name.append(to_string(i));
      ProductExecutable* client = launchExecutable(launchConfigs, name);
      processes->push_back(client);
   }

   ProductExecutable* apiServer = launchExecutable(launchConfigs, "apiServer");
   processes->push_back(apiServer);
   return processes;
}

/**
 * Launches a single ProductExecutable, given a JSON object and key.
 * Returns a pointer to the ProductExecutable object.
 **/
ProductExecutable* launchExecutable(Json::Value launchConfigs, string key){
   if (!launchConfigs.isMember(key)){
      string error = "There is no launch configuration for '";
      error.append(key).append("'.  Available members: ");
      vector<string> members = launchConfigs.getMemberNames();
      for(int i = 0; i < members.size(); i++){
         error.append("\n   ").append(members[i]);
      }
      cout << error << endl; // Logger
      throw(error);
   }

   Json::Value config = launchConfigs[key];
   ProductExecutable* exec = new ProductExecutable(config);
   exec->launch();

   // Hard coding a time is a temporary measure while working on
   // launching code.  We must test that things acutally start.
   // e.g. Maybe accept a function pointer, since this can launch
   // different types of things.
   // We get errors in the client logs if just launching everythin
   // ASAP.
   cout << "Waiting for executable to start." << endl;
   this_thread::sleep_for(chrono::milliseconds(1000));

   return exec;
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
