// Copyright 2018 VMware, all rights reserved

/**
 * Status stores the result of an operation.
 */

#ifndef ATHENA_KVB_STATUS_HPP
#define ATHENA_KVB_STATUS_HPP

#include <string>

namespace Blockchain {
class Status {
 public:
  static Status OK() { return Status(ok, ""); }
  static Status NotFound(std::string msg) { return Status(notFound, msg); }
  static Status InvalidArgument(std::string msg) { return Status(invalidArgument, msg); }
  static Status IllegalOperation(std::string msg) { return Status(illegalOperation, msg); }
  static Status GeneralError(std::string msg) { return Status(generalError, msg); }

  bool isOK() const { return type == ok; }
  bool isNotFound() const { return type == notFound; }
  bool isInvalidArgument() const { return type == invalidArgument; }
  bool isIllegalOperation() const { return type == illegalOperation; }
  bool isGeneralError() const { return type == generalError; }

  std::ostream& operator<<(std::ostream& s) const {
    s << messagePrefix();
    if (!isOK()) {
      s << message;
    }
    return s;
  };

 private:
  enum statusType {
    ok,
    notFound,
    invalidArgument,
    illegalOperation,
    generalError
  };

  statusType type;
  std::string message;

  Status(statusType t, std::string msg): type(t), message(msg) { }

  std::string messagePrefix() const {
    switch (type) {
    case ok: return "OK";
    case notFound: return "Not Found: ";
    case invalidArgument: return "Invalid Argument: ";
    case illegalOperation: return "Illegal Operation: ";
    case generalError: return "General Error: ";
    default: return "Unknown Error Type: ";
    }
  }
};

std::ostream& operator<<(std::ostream& s, Status const& status);
}  // end namespace Blockchain

#endif  // end ifndef ATHENA_KVB_STATUS_HPP
