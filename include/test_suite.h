#pragma once

#include <string>

using namespace std;

/*******************************************************************************
 * A test suite:
 *   - Defines and runs a set of related tests.
 *   - Is launched by the testing framework calling its run() method.
 *   - Should be located in the source code project which contains the code
 *     which is being tested.
 *
 * The run() method should return the results in JSON with the following format:
 * {
 *   "suite-name": {
 *     "result": "pass" or "fail",
 *     "tests": {
 *       "test1-name": {
 *         "result": "pass" or "fail",
 *         "info": String of text containing error messages, stack traces, etc...
 *       },
 *       ...
 *     }
 *   }
 * }
 * 
 * Test suites may use third party suites, such as unit testing frameworks,
 * etc...
 *
 * Test suites handle their own exceptions, critical failures, etc...
 **/
class TestSuite{
public:
  virtual string getName() = 0;
  virtual string run() = 0;
};

