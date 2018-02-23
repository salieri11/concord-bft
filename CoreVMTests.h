#pragma once

#include <string>

#include "TestSuite.h"

using namespace std;

class CoreVMTests: public TestSuite{
public:
  string getName();
  string run();
};
