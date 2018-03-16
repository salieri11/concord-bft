// Copyright 2018 VMware, all rights reserved
//
// Athena node startup.

#include <iostream>
#include <inttypes.h>
#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <google/protobuf/text_format.h>

#include "athena.pb.h"

using namespace boost::program_options;
using boost::asio::ip::tcp;
using boost::asio::ip::address;
using boost::asio::io_service;
using namespace com::vmware::athena;

static const std::string DEFAULT_ATHENA_IP = "127.0.0.1";
static const std::string DEFAULT_ATHENA_PORT = "5458";

int main(int argc, char** argv)
{
   try {
      variables_map opts;
      options_description desc{"Options"};

      desc.add_options()
         ("help,h", "Print this help message")
         ("address,a",
          value<std::string>()->default_value(DEFAULT_ATHENA_IP),
          "IP address of athena node")
         ("port,p",
          value<std::string>()->default_value(DEFAULT_ATHENA_PORT),
          "Port of athena node")
         ("from,f",
          value<std::string>(),
          "Address to send the TX from")
         ("to,t",
          value<std::string>(),
          "Address to send the TX to")
         ("value,v",
          value<int>(),
          "Amount to pass as value")
         ("data,d",
          value<std::string>(),
          "Hex-encoded string to pass as data");

      store(parse_command_line(argc, argv, desc), opts);

      if (opts.count("help")) {
         std::cout << "Athena eth_sendTransaction wrapper" << std::endl;
         std::cout << desc << std::endl;
         return 0;
      }

      // After help-check, so that required params are not required for help.
      notify(opts);

      /*** Open connection ***/

      io_service io_service;
      tcp::socket s(io_service);
      tcp::resolver resolver(io_service);
      boost::asio::connect(
         s, resolver.resolve(
            {opts["address"].as<std::string>(),
             opts["port"].as<std::string>()}));

      std::cout << "Connected" << std::endl;

      /*** Create request ***/

      AthenaRequest athReq;
      EthRequest *ethReq = athReq.add_eth_request();

      if (opts.count("from") > 0) {
         // TODO: dehex?
         ethReq->mutable_addr_from()->assign(opts["from"].as<std::string>());
      }
      if (opts.count("to") > 0) {
         // TODO: dehex?
         ethReq->mutable_addr_to()->assign(opts["to"].as<std::string>());
      }
      if (opts.count("value") > 0) {
         // TODO: hex value?
         std::cout << "Warning: not supporting value yet" << std::endl;
      }
      if (opts.count("data") > 0) {
         // TODO: dehex?
         ethReq->mutable_data()->assign(opts["data"].as<std::string>());
      }

      std::cout << "Message Prepared" << std::endl;

      /*** Send request ***/

      std::string pb;
      athReq.SerializeToString(&pb);
      size_t msglen = athReq.ByteSize();
      // only sixteen bits available
      assert(msglen < 0x10000);
      // little-endian!
      char *prefix = (char*)&msglen;

      std::cout << __LINE__ << std::endl;
      boost::asio::write(s, boost::asio::buffer(prefix, 2));
      std::cout << __LINE__ << std::endl;
      boost::asio::write(s, boost::asio::buffer(pb, msglen));
      std::cout << __LINE__ << std::endl;

      std::cout << "Message Sent" << std::endl;

      /*** Receive response ***/

      size_t reply_length = boost::asio::read(
         s, boost::asio::buffer(prefix, 2));
      if (reply_length != 2) {
         std::cerr << "Did not read full prefix, reply_length = "
                   << reply_length << std::endl;
      } else {
         // little-endian!
         msglen = (size_t)*prefix;
         char *reply = new char[msglen];
         reply_length = boost::asio::read(
            s, boost::asio::buffer(reply, msglen));
         if (reply_length != msglen) {
            std::cerr << "Did not read full reply, expected " <<
               msglen << " bytes, but got " << reply_length << std::endl;
         } else {
            AthenaResponse athResp;
            athResp.ParseFromString(reply);

            std::string pbtext;
            google::protobuf::TextFormat::PrintToString(athResp, &pbtext);

            std::cout << "Received response: " << pbtext << std::endl;
         }
      }

      /*** Close Connection ***/
      s.close();
   } catch (std::exception &e) {
      std::cerr << "Exception: " << e.what() << std::endl;
      return -1;
   }

   return 0;
}
