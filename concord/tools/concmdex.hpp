// Copyright 2018 VMware, all rights reserved
//
// Exceptions for command line tools.

#ifndef CONCMDEX_HPP
#define CONCMDEX_HPP

class AthCmdException: public std::exception {
public:
   explicit AthCmdException(const std::string &what): msg(what) {};

   virtual const char* what() const noexcept override
   {
      return msg.c_str();
   }

private:
   std::string msg;
};

#endif
