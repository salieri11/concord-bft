// Copyright 2020 VMware, all rights reserved
#include <google/protobuf/util/time_util.h>
#include <log4cplus/loggingmacros.h>

#include "concord.pb.h"
#include "tee.pb.h"
#include "tee_commands_handler.hpp"
#include "time/time_contract.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::TeeRequest;
using com::vmware::concord::TeeResponse;
using concord::time::TimeContract;
using concordUtils::BlockId;
using concordUtils::SetOfKeyValuePairs;
using std::string;

namespace concord {
namespace tee {

bool TeeCommandsHandler::Execute(const ConcordRequest& concord_request,
                                 uint8_t flags, TimeContract* time_contract,
                                 opentracing::Span& parent_span,
                                 ConcordResponse& concord_response) {
  if (!concord_request.has_tee_request()) {
    // we have to ignore this, to allow time-only updates
    return true;
  }

  com::vmware::concord::TeeResponse* tee_response =
      concord_response.mutable_tee_response();

  const TeeRequest& tee_request = concord_request.tee_request();

  if (tee_request.has_tee_input()) {
    tee_response->set_tee_output("Test Execution Handler received input '" +
                                 tee_request.tee_input() + "'");
  } else {
    tee_response->set_tee_output("TeeCommandsHandler received no input");
  }

  return true;
}

void TeeCommandsHandler::WriteEmptyBlock(TimeContract* time_contract) {
  BlockId currentBlockId = storage_.getLastBlock();
  SetOfKeyValuePairs empty_updates;
  BlockId newBlockId = 0;
  assert(addBlock(empty_updates, newBlockId).isOK());
  assert(newBlockId == currentBlockId + 1);
}

}  // namespace tee
}  // namespace concord
