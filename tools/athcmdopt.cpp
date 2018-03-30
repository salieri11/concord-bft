// Copyright 2018 VMware, all rights reserved
//
// Options for command line tools.

#include <iostream>
#include "athcmdopt.hpp"

using boost::program_options::value;

bool parse_options(int argc, char **argv, options_adder adder,
                   boost::program_options::variables_map &opts)
{
   boost::program_options::options_description desc{"Options"};

   desc.add_options()
      (OPT_HELP",h", "Print this help message")
      (OPT_ADDRESS",a",
       value<std::string>()->default_value(DEFAULT_ATHENA_IP),
       "IP address of athena node")
      (OPT_PORT",p",
       value<std::string>()->default_value(DEFAULT_ATHENA_PORT),
       "Port of athena node");

   (*adder)(desc);

   store(parse_command_line(argc, argv, desc), opts);

   if (opts.count(OPT_HELP)) {
      std::cout << desc << std::endl;
      return false;
   }

   // After help-check, so that required params are not required for help.
   notify(opts);
   return true;
}
