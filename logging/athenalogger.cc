#include "athenalogger.h"

bool initialized = false; // has the logger been already initialized
std::string athena_logger_config_file_path = "log4cplus.properties";
int reconfig_time_ms = 5 * 1000; // Number of milli seconds after which re-read config file

log4cplus::Logger get_athena_logger(std::string logger_name) {
  if (!initialized) {
    log4cplus::initialize();
    log4cplus::ConfigureAndWatchThread configureThread(
						       athena_logger_config_file_path,
						       reconfig_time_ms);
    initialized = true;
  }
  return log4cplus::Logger::getInstance(logger_name);
}
