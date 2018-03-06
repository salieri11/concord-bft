/* **********************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
 * **********************************************************/
#include <iostream>
#include <string>
#include <unistd.h>
#include <vector>

#include "hermes/include/test_suite.h"
#include "hermes/include/core_vm_tests.h"

using namespace std;

TestSuite* createTestSuite(string name);
void publishResults(vector<string> results);

int main(int argc, char** argv)
{
   vector<TestSuite*> suites;
   vector<string> results; // One JSON result per suite.
   char argName;
  
   while ((argName = getopt (argc, argv, "t:")) != -1){
      switch(argName){
      case 't':
         string suite = optarg;
         suites.push_back(createTestSuite(suite));
      }
   }

   cout << "Test suites to run:" << endl;
   for (int i = 0; i < suites.size(); i++){
      cout << "  " << suites[i]->getName() << endl;
   }

   for (int i = 0; i < suites.size(); i++){
      cout << "Running: " << suites[i]->getName() << endl;
      results.push_back(suites[i]->run());
   }

   cout << "Tests finished.  Results:" << endl;
   for (int i = 0; i < results.size(); i++){
      cout << results[i] << endl;
   }

   publishResults(results);
}


/**
 * Given the name of a test suite, instantiate one and return a
 * pointer to it.
 **/
TestSuite* createTestSuite(string name){
   // TODO: Use a map of name -> fn instead?
   if (name == "CoreVMTests"){
      CoreVMTests* suite = new CoreVMTests();
      return (TestSuite*) suite;
   }
}


/**
 * Given the results, create a web page, upload
 * to a server, etc...
 **/
void publishResults(vector<string> results){
   cout << "Publishing results." << endl;
}
