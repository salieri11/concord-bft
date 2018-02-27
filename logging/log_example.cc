#include "athenalogger.h"

log4cplus::Logger lg = get_athena_logger("test.log_1");

int main() {
  LOG4CPLUS_DEBUG(lg, "debug");
  LOG4CPLUS_INFO(lg, "info");
  LOG4CPLUS_WARN(lg, "warn");
  LOG4CPLUS_ERROR(lg, "error");
  LOG4CPLUS_FATAL(lg, "fatal");
  return 0;
}

