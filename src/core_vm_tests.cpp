#include <iostream>
#include <string>

#include "hermes/include/core_vm_tests.h"

using namespace std;

string CoreVMTests::getName(){
  return "CoreVMTests";
}

string CoreVMTests::run(){
  cout << "Hello from the CoreVM tests." << endl;

  string testDir = "../athena/athena_testing";
  string command = string("make -C ").append(testDir).append(" build run_core_vm_tests");
  string resultFile = testDir.append("results.json");
  string results;

  int bufferSize = 256;
  char buffer[bufferSize];
  cout << "Running command: '" << command << "'" << endl;
  FILE* pipe = popen(command.c_str(), "r");

  if (pipe){
    while(fgets(buffer, bufferSize, pipe) != NULL){
      cout << buffer;
    }
    
    cout << endl;
    pclose(pipe);
  }else{
    string error = string("Unable to create external process for ").append(command);
    throw(error);
  }
  
  results = "foo"; //Read resultFile from remote test execution.
  return results;
}
