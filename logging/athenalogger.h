#include <log4cplus/logger.h>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>
#include <cstddef>

log4cplus::Logger get_athena_logger(std::string logger_name);
