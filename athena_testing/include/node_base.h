#pragma once

#include <string>
#include <iostream>

using namespace std;

class NodeBase{
public:
  string s = "";
  virtual string makeCall() = 0;
};

