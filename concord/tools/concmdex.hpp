// Copyright 2018 VMware, all rights reserved
//
// Exceptions for command line tools.

#ifndef CONCMDEX_HPP
#define CONCMDEX_HPP

class ConcCmdException : public std::exception {
 public:
  explicit ConcCmdException(const std::string& what) : msg(what){};

  virtual const char* what() const noexcept override { return msg.c_str(); }

 private:
  std::string msg;
};

#endif
