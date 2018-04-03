// Copyright 2018 VMware, all rights reserved
//

#ifndef CONFIGURATION_MANAGER_HPP
#define CONFIGURATION_MANAGER_HPP

#include <iostream>
#include <fstream>
#include <csignal>
#include <boost/program_options.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>

boost::program_options::variables_map initialize_config(int argc, char **argv);

#endif
