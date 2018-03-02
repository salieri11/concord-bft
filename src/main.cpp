// Copyright 2018 VMware, all rights reserved
//
// Athena node startup.

#include <iostream>
#include <boost/program_options.hpp>

#include "api_acceptor.hpp"

using namespace boost::program_options;
using boost::asio::ip::tcp;
using boost::asio::ip::address;
using boost::asio::io_service;

using namespace com::vmware::athena;

/*
 * Start the service that listens for connections from Helen.
 */
void
start_service(variables_map &opts)
{
   std::string ip = opts["ip"].as<std::string>();
   short port = opts["port"].as<short>();

   io_service io_service;
   tcp::endpoint endpoint(address::from_string(ip), port);
   api_acceptor acceptor(io_service, endpoint);

   std::cout << "Listening on " << endpoint << std::endl;
   io_service.run();
}

int
main(int argc, char** argv)
{
   variables_map opts;

   std::cout << "VMware Project Athena" << std::endl;

   try {
      options_description desc{"Options"};
      desc.add_options()
         ("help,h", "Print this help message")
         ("ip",
          value<std::string>()->default_value("0.0.0.0"),
          "IP on which to expose the service")
         ("port,p",
          value<short>()->default_value(5458),
          "Port on which to expose the service");

      store(parse_command_line(argc, argv, desc), opts);

      if (opts.count("help")) {
         std::cout << desc << std::endl;
         return 0;
      }

      // call notify after checking "help", so that required parameters are not
      // required to get help
      notify(opts);

      start_service(opts);
   } catch (const error &ex) {
      std::cerr << ex.what() << std::endl;
      return -1;
   }

   return 0;
}
