// Copyright 2019 VMware, all rights reserved
//
// Exception thrown by time library.

#ifndef TIME_TIME_EXCEPTION_HPP
#define TIME_TIME_EXCEPTION_HPP

#include <exception>
#include <string>

namespace concord {
namespace time {

class TimeException : public std::exception {
 public:
  explicit TimeException(const std::string& what) : msg_(what) {}

  const char* what() const noexcept override { return msg_.c_str(); }

 private:
  std::string msg_;
};

}  // namespace time
}  // namespace concord

#endif  // TIME_TIME_EXCEPTION_HPP
