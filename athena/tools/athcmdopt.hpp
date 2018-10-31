// Copyright 2018 VMware, all rights reserved
//
// Options for command line tools.

#ifndef ATHCMDOPT_HPP
#define ATHCMDOPT_HPP

#include <boost/program_options.hpp>

static const std::string DEFAULT_ATHENA_IP = "127.0.0.1";
static const std::string DEFAULT_ATHENA_PORT = "5458";

#define OPT_HELP "help"
#define OPT_ADDRESS "address"
#define OPT_PORT "port"

/**
 * Tool-specific options adding function. When passed to the parse_options
 * function, it will be called with an options_description, to which it should
 * add its specific options.
 */
typedef void (*options_adder)(boost::program_options::options_description&);

bool parse_options(int argc, char **argv, options_adder adder,
                   boost::program_options::variables_map &opts);

#endif
