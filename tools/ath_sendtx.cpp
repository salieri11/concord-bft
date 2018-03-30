// Copyright 2018 VMware, all rights reserved
//
// Send a transaction to Athena directly.

#include <iostream>
#include <inttypes.h>
#include <boost/program_options.hpp>
#include <google/protobuf/text_format.h>

#include "athcmdex.hpp"
#include "athcmdfmt.hpp"
#include "athcmdopt.hpp"
#include "athcmdconn.hpp"
#include "athena.pb.h"

using namespace boost::program_options;
using namespace com::vmware::athena;

#define OPT_FROM "from"
#define OPT_TO "to"
#define OPT_VALUE "value"
#define OPT_DATA "data"

void add_options(options_description &desc) {
   desc.add_options()
      (OPT_FROM",f",
       value<std::string>(),
       "Address to send the TX from")
      (OPT_TO",t",
       value<std::string>(),
       "Address to send the TX to")
      (OPT_VALUE",v",
       value<int>(),
       "Amount to pass as value")
      (OPT_DATA",d",
       value<std::string>(),
       "Hex-encoded string to pass as data");
}

int main(int argc, char** argv)
{
   try {
      variables_map opts;
      if (!parse_options(argc, argv, &add_options, opts)) {
         return 0;
      }

      /*** Create request ***/

      AthenaRequest athReq;
      EthRequest *ethReq = athReq.add_eth_request();
      std::string from;
      std::string to;
      std::string data;

      if (opts.count(OPT_FROM) > 0) {
         dehex0x(opts[OPT_FROM].as<std::string>(), from);
	 ethReq->set_addr_from(from);
      }
      if (opts.count(OPT_TO) > 0) {
         dehex0x(opts[OPT_TO].as<std::string>(), to);
	 ethReq->set_addr_to(to);
      }
      if (opts.count(OPT_VALUE) > 0) {
         // TODO: hex value?
         std::cout << "Warning: not supporting value yet" << std::endl;
      }
      if (opts.count(OPT_DATA) > 0) {
         dehex0x(opts[OPT_DATA].as<std::string>(), data);
	 ethReq->set_data(data);
      }

      std::string pbtext;
      google::protobuf::TextFormat::PrintToString(athReq, &pbtext);
      std::cout << "Message Prepared: " << pbtext << std::endl;

      /*** Send & Receive ***/

      AthenaResponse athResp;
      if (call_athena(opts, athReq, athResp)) {
         google::protobuf::TextFormat::PrintToString(athResp, &pbtext);
         std::cout << "Received response: " << pbtext << std::endl;

         /*** Handle Response ***/

         if (athResp.eth_response_size() == 1) {
            EthResponse ethResp = athResp.eth_response(0);
            if (ethResp.has_data()) {
               std::string result;
               hex0x(ethResp.data(), result);
               std::cout << "Transaction Receipt: " << result << std::endl;
            } else {
               std::cerr << "EthResponse has no data" << std::endl;
               return -1;
            }
         } else {
            std::cerr << "Wrong number of eth_responses: "
                      << athResp.eth_response_size()
                      << " (expected 1)" << std::endl;
            return -1;
         }
      } else {
         return -1;
      }
   } catch (std::exception &e) {
      std::cerr << "Exception: " << e.what() << std::endl;
      return -1;
   }

   return 0;
}
