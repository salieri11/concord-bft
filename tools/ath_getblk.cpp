// Copyright 2018 VMware, all rights reserved
//
// Get a block or block list from athena.

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

#define OPT_LIST "list"
#define OPT_NUMBER "number"
#define OPT_COUNT "count"
#define OPT_HASH "hash"

void add_options(options_description &desc) {
   desc.add_options()
      (OPT_LIST",l",
       "List transactions from index to index-count")
      (OPT_NUMBER",n",
       value<std::uint64_t>(),
       "Number of block to get (latest if listing)")
      (OPT_COUNT",c",
       value<std::uint64_t>(),
       "Number of blocks to list")
      (OPT_HASH",s",
       value<std::string>(),
       "Hash of block to get");
}

void prepare_block_list_request(variables_map &opts, AthenaRequest &athReq)
{
   BlockListRequest *blkReq = athReq.mutable_block_list_request();

   if (opts.count(OPT_NUMBER)) {
      blkReq->set_latest(opts[OPT_NUMBER].as<std::uint64_t>());
   }
   if (opts.count(OPT_COUNT)) {
      blkReq->set_count(opts[OPT_COUNT].as<std::uint64_t>());
   }
}

void prepare_block_request(variables_map &opts, AthenaRequest &athReq)
{
   BlockRequest *blkReq = athReq.mutable_block_request();

   if (opts.count(OPT_NUMBER)) {
      blkReq->set_number(opts[OPT_NUMBER].as<std::uint64_t>());
   } else {
      std::string blkhash;
      dehex0x(opts[OPT_HASH].as<std::string>(), blkhash);
      blkReq->set_hash(blkhash);
   }
}

void handle_block_list_response(AthenaResponse athResp) {
   if (!athResp.has_block_list_response()) {
      std::cerr << "No block list response found." << std::endl;
      if (athResp.error_response_size() == 1) {
         std::cerr << "Error response: '"
                   << athResp.error_response(0).description()
                   << std::endl;
      }
   }

   BlockListResponse blkResp = athResp.block_list_response();

   std::cout << "Blocks: (" << blkResp.block_size() << ")" << std::endl;

   for (int i = 0; i < blkResp.block_size(); i++) {
      BlockBrief bb = blkResp.block(i);
      std::string hash;
      hex0x(bb.hash(), hash);
      std::cout << bb.number() << " == " << hash << std::endl;
   }
}

void handle_block_response(AthenaResponse athResp) {
   if (!athResp.has_block_response()) {
      std::cerr << "No block response found." << std::endl;
      if (athResp.error_response_size() == 1) {
         std::cerr << "Error response: '"
                   << athResp.error_response(0).description()
                   << std::endl;
      }
   }

   BlockResponse blkResp = athResp.block_response();

   std::string hash, parent;
   hex0x(blkResp.hash(), hash);
   hex0x(blkResp.parent_hash(), parent);
   std::cout << "Number: " << blkResp.number() << std::endl
             << "  Hash: " << hash << std::endl
             << "Parent: " << parent << std::endl
             << "Transactions:" << std::endl;

   for (int i = 0; i < blkResp.transaction_size(); i++) {
      std::string tx;
      hex0x(blkResp.transaction(i).hash(), tx);
      std::cout << "   " << tx << std::endl;
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

      if (opts.count(OPT_LIST) == 0) {
         // list not requested
         if (opts.count(OPT_COUNT)) {
            std::cerr <<
               "The count parameter is not valid if a list is not requested."
                      << std::endl;
            return -1;
         }
         if (!opts.count(OPT_NUMBER) && !opts.count(OPT_HASH)) {
            std::cerr << "Please provide either a number or a hash."
                      << std::endl;
            return -1;
         }
         if (opts.count(OPT_NUMBER) && opts.count(OPT_HASH)) {
            std::cerr << "Please provide only one of number or hash."
                      << std::endl;
            return -1;
         }
      } else {
         // we are listing
         if (opts.count(OPT_HASH)) {
            std::cerr <<
               "The hash parameter is not valid if a list is requested."
                      << std::endl;
            return -1;
         }
      }

      AthenaRequest athReq;
      if (opts.count(OPT_LIST)) {
         prepare_block_list_request(opts, athReq);
      } else {
         prepare_block_request(opts, athReq);
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

         if (opts.count(OPT_LIST)) {
            handle_block_list_response(athResp);
         } else {
            handle_block_response(athResp);
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
