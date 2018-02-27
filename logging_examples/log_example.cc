#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include <cstddef>

int main() {
  std::string athena_logger_config_file_path = "log4cplus.properties";
  int reconfig_time_ms = 5 * 1000; // Number of milli seconds after which re-read config file
  log4cplus::initialize();
  log4cplus::ConfigureAndWatchThread configureThread(
						     athena_logger_config_file_path,
						     reconfig_time_ms);

  log4cplus::Logger lg = log4cplus::Logger::getInstance("test.log_1");

  std::string data = "some more data";
  LOG4CPLUS_DEBUG(lg, "debug " + data + " !");
  LOG4CPLUS_INFO(lg, "info");
  LOG4CPLUS_WARN(lg, "warn");
  LOG4CPLUS_ERROR(lg, "error");
  LOG4CPLUS_FATAL(lg, "fatal");
  return 0;
}

