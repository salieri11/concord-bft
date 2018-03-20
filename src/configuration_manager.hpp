// Copyright 2018 VMware, all rights reserved
//

#include <iostream>
#include <fstream>
#include <csignal>
#include <boost/program_options.hpp>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/configurator.h>

boost::program_options::variables_map initialize_config(int argc, char **argv);
