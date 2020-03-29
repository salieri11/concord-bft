// Copyright 2020 VMware, all rights reserved
#include "tee_commands_handler.hpp"

#include <google/protobuf/util/time_util.h>
#include <log4cplus/loggingmacros.h>
#include <log4cplus/mdc.h>
#include <map>
#include <string>

#include "concord_storage.pb.h"
#include "storage/kvb_key_types.h"
#include "time/time_contract.hpp"

using std::map;
using std::string;
using std::vector;

using concord::kvbc::BlockId;
using concord::kvbc::IBlocksAppender;
using concord::kvbc::ILocalKeyValueStorageReadOnly;
using concord::kvbc::Key;
using concord::kvbc::KeyValuePair;
using concord::kvbc::SetOfKeyValuePairs;
using concord::kvbc::Value;
using concord::time::TimeContract;
using concordUtils::Sliver;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::SkvbcRequest;
using com::vmware::concord::SkvbcResponse;
using com::vmware::concord::TeeRequest;
using com::vmware::concord::TeeResponse;
using com::vmware::concord::kvb::ValueWithTrids;

using google::protobuf::util::TimeUtil;

namespace concord {
namespace tee {

Sliver CreateSliver(const char* content, size_t size) {
  char* val = new char[size];
  memcpy(val, content, size);
  return Sliver(val, size);
}

Sliver CreateSliver(const string& content) {
  return CreateSliver(content.c_str(), content.size());
}

Sliver CreateTeeKvbKey(const string& content) {
  string full_key(content);
  // filter system is currently hardcoded for kKvbKeyDaml storage type
  full_key.insert(0, &concord::storage::kKvbKeyDaml,
                  sizeof(concord::storage::kKvbKeyDaml));
  return CreateSliver(full_key.c_str(), full_key.size());
}

Sliver CreateTeeKvbValue(const string& value, vector<string> trid_list) {
  ValueWithTrids proto;
  proto.set_value(value);
  for (const auto& trid : trid_list) {
    proto.add_trid(trid);
  }

  size_t size = proto.ByteSizeLong();
  char* data = new char[size];
  proto.SerializeWithCachedSizesToArray(reinterpret_cast<unsigned char*>(data));

  return Sliver(data, size);
}

bool TeeCommandsHandler::Execute(const ConcordRequest& concord_request,
                                 uint8_t flags, TimeContract* time_contract,
                                 opentracing::Span& parent_span,
                                 ConcordResponse& concord_response) {
  if (!concord_request.has_tee_request()) {
    // we have to ignore this, to allow time-only updates
    return true;
  }

  const TeeRequest& tee_request = concord_request.tee_request();
  com::vmware::concord::TeeResponse* tee_response =
      concord_response.mutable_tee_response();

  if (tee_request.has_skvbc_request() &&
      tee_request.skvbc_request().has_request_content()) {
    return ExecuteSkvbcRequest(tee_request, flags, tee_response);
  }

  if (!tee_request.has_tee_input()) {
    tee_response->set_tee_output("TeeCommandsHandler received no input");
  } else if (tee_request.tee_input() != "PrivacySanityTest") {
    // for backward compatibily
    tee_response->set_tee_output("Test Execution Handler received input '" +
                                 tee_request.tee_input() + "'");
  } else {  // PrivacySanityTest
    /*
      Mock transaction, on receipt of "PrivacySanityTest"
      {
        [
          {
            trids: ["client_id_1", "client_id_2", "client_id_3"],
            k: key-123
            v: value-123
          },
          {
            trids: ["client_id_1"],
            k: key-1
            v: value-1
          },
          {
            trids: ["client_id_2"],
            k: key-2
            v: value-2
          },
          {
            trids: ["client_id_1", "client_id_2"],
            k: key-12
            v: value-12
          },
          {
            trids: [],
            k: key-qll
            v: value-all
          }
        ]
      }
    */
    SetOfKeyValuePairs updates;
    vector<string> trid1, trid2, trid3;
    trid1.push_back("client_id_1");
    trid2.push_back("client_id_2");
    trid3.push_back("client_id_3");
    vector<string> trid12{trid1.at(0), trid2.at(0)};
    vector<string> trid123(begin(trid12), end(trid12));
    trid123.push_back(trid3.at(0));

    updates.insert(KeyValuePair(CreateTeeKvbKey("key-123"),
                                CreateTeeKvbValue("value-123", trid123)));
    updates.insert(KeyValuePair(CreateTeeKvbKey("key-1"),
                                CreateTeeKvbValue("value-1", trid1)));
    updates.insert(KeyValuePair(CreateTeeKvbKey("key-2"),
                                CreateTeeKvbValue("value-2", trid2)));
    updates.insert(KeyValuePair(CreateTeeKvbKey("key-12"),
                                CreateTeeKvbValue("value-12", trid12)));
    updates.insert(KeyValuePair(CreateTeeKvbKey("key-all"),
                                CreateTeeKvbValue("value-all", {})));

    RecordTransaction(updates, tee_response);
  }

  return true;
}

bool TeeCommandsHandler::ExecuteSkvbcRequest(const TeeRequest& tee_request,
                                             uint8_t flags,
                                             TeeResponse* tee_response) {
  LOG4CPLUS_DEBUG(logger_, "Processing SKVBC request...");
  const std::string& request_content =
      tee_request.skvbc_request().request_content();

  const auto max_response_size = request_context_->max_response_size;

  uint32_t reply_size = 0;
  char reply_buffer[max_response_size];
  memset(reply_buffer, 0, max_response_size);

  int result = skvbc_commands_handler_.execute(
      request_context_->client_id, request_context_->sequence_num, flags,
      request_content.size(), request_content.c_str(), max_response_size,
      reply_buffer, reply_size);

  if (result != 0) {
    LOG4CPLUS_ERROR(logger_, "Failed to process SKVBC request.");
    return false;
  }

  com::vmware::concord::SkvbcResponse skvbc_response;
  skvbc_response.mutable_response_content()->assign(reply_buffer, reply_size);

  tee_response->mutable_skvbc_response()->MergeFrom(skvbc_response);

  LOG4CPLUS_DEBUG(logger_, "Successfully processed SKVBC request.");
  return true;
}

void TeeCommandsHandler::RecordTransaction(const SetOfKeyValuePairs& updates,
                                           TeeResponse* tee_response) {
  BlockId new_block_id = 0;
  concordUtils::Status res = addBlock(updates, new_block_id);
  assert(res.isOK());
  write_ops_.Increment();
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
