#pragma once

#include <string>

#include "test_suite.h"

using namespace std;

class CoreVMTests: public TestSuite{
public:
  string getName();
  string run();
};
