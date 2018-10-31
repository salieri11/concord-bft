// Copyright 2018 VMware, all rights reserved
//
// Athena connection for command line tools.

#ifndef ATHCMDCONN_HPP
#define ATHCMDCONN_HPP

#include <boost/program_options.hpp>
#include "athena.pb.h"

bool call_athena(boost::program_options::variables_map &opts,
                 com::vmware::athena::AthenaRequest &request,
                 com::vmware::athena::AthenaResponse &response /* out */);

#endif
