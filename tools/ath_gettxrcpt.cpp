// Copyright 2018 VMware, all rights reserved
//
// Get a transaction receipt from Athena directly.

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

#define OPT_RECEIPT "receipt"

void add_options(options_description &desc) {
   desc.add_options()
      (OPT_RECEIPT",r",
       value<std::string>(),
       "The transaction hash returned from eth_sendTransaction");
}

std::string status_to_string(int32_t status) {
   switch (status) {
   case 0:
      return "(failure)";
   case 1:
      return "(success)";
   default:
      return "(error: unknown status value)";
   }
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
      TransactionRequest *tReq = athReq.mutable_transaction_request();
      std::string rcpthash;


      if (opts.count(OPT_RECEIPT) > 0) {
         dehex0x(opts[OPT_RECEIPT].as<std::string>(), rcpthash);
	 tReq->set_hash(rcpthash);
      } else {
         std::cerr << "Please provide a transaction receipt." << std::endl;
         return -1;
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

         if (athResp.has_transaction_response()) {
            TransactionResponse tResp = athResp.transaction_response();
            if (tResp.has_status()) {
               uint32_t status = tResp.status();
               std::cout << "Transaction status: " << status
                         << " " << status_to_string(status) << std::endl;
            } else {
               std::cerr << "EthResponse has no status" << std::endl;
               return -1;
            }

            if (tResp.has_contract_address()) {
               std::string result;
               hex0x(tResp.contract_address(), result);
               std::cout << "Contract address: " << result << std::endl;
            }
         } else {
            std::cerr << "transaction response not found: " << std::endl;
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
