// Copyright 2020 VMware, all rights reserved

#pragma once

#include <exception>
#include <string>

#include "status.hpp"

namespace concord {
namespace external_client {

// A base class for external client exceptions.
class ClientExceptionBase : public std::exception {
 protected:
  ClientExceptionBase(const std::string& msg) : msg_{msg} {}

 public:
  // Returns an explanatory string. Overridden from std::exception .
  const char* what() const noexcept override { return msg_.c_str(); }

  // Returns an explanatory string.
  const std::string& Message() const noexcept { return msg_; }

 private:
  std::string msg_;
};

// An exception thrown on errors when sending client requests. Contains message
// and status fields.
class ClientRequestException : public ClientExceptionBase {
 public:
  ClientRequestException(const concordUtils::Status& status,
                         const std::string& msg)
      : ClientExceptionBase(msg), status_{status} {
    status_msg_ = Message() + ": status: " + status_.toString();
  }

  // Returns a status describing the error that has occurred.
  const concordUtils::Status& Status() const noexcept { return status_; }

  // Returns an explanatory string. Overridden from std::exception .
  const char* what() const noexcept override { return status_msg_.c_str(); }

 private:
  concordUtils::Status status_;
  std::string status_msg_;
};

}  // namespace external_client
}  // namespace concord
