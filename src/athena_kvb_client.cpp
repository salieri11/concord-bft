// Copyright 2018 VMware, all rights reserved
//
// Layer between api_connection and Blockchain::IClient

#include <log4cplus/loggingmacros.h>

#include "kvb/BlockchainInterfaces.h"
#include "athena_kvb_client.hpp"

using namespace com::vmware::athena;

/**
 * Send a request to the replicas. Returns true if the response contains
 * something to forward (either a response message or an appropriate error
 * message). Returns false if the response is empty (for example, if parsing
 * failed).
 */
bool com::vmware::athena::KVBClient::send_request_sync(AthenaRequest &req,
                                                       bool isReadOnly,
                                                       AthenaResponse &resp)
{
   std::string command;
   req.SerializeToString(&command);
   Blockchain::Slice cmdslice(command);
   Blockchain::Slice replyslice;

   Blockchain::Status status = client_->invokeCommandSynch(
      cmdslice, isReadOnly, replyslice);

   if (status.ok()) {
      return resp.ParseFromArray(replyslice.data(), replyslice.size());
   } else {
      LOG4CPLUS_ERROR(logger_, "Error invoking read-only command: " <<
                      status.ToString());
      ErrorResponse *err = resp.add_error_response();
      err->set_description("Internal Athena Error");
      return true;
   }
}
