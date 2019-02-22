// Copyright 2018 VMware, all rights reserved
//

#ifndef CONFIGURATION_MANAGER_HPP
#define CONFIGURATION_MANAGER_HPP

#include <log4cplus/configurator.h>
#include <log4cplus/loggingmacros.h>
#include <boost/program_options.hpp>
#include <csignal>
#include <fstream>
#include <iostream>

boost::program_options::variables_map initialize_config(int argc, char **argv);

#endif
