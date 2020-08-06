// Copyright 2020 VMware, all rights reserved

#include <opentracing/span.h>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <thread>
#include "Logger.hpp"
#include "Logging4cplus.hpp"
#include "concord.pb.h"
#include "consensus/concord_commands_handler.hpp"
#include "kv_types.hpp"
#include "kvstream.h"
#include "perf_init.hpp"
#include "performance.pb.h"
#include "status.hpp"
#include "thin_replica/subscription_buffer.hpp"

using namespace std;
using com::vmware::concord::PerfRequest;
using com::vmware::concord::PerfResponse;
using com::vmware::concord::performance::PerfCleanRequest;
using com::vmware::concord::performance::PerfCleanResponse;
using com::vmware::concord::performance::PerfInitRequest;
using com::vmware::concord::performance::PerfInitResponse;
using com::vmware::concord::performance::PerfWriteExternal;
using com::vmware::concord::performance::PerfWriteFromInit;
using com::vmware::concord::performance::PerfWriteRequest;
using com::vmware::concord::performance::PerfWriteResponse;

namespace concord {
namespace performance {

class PerformanceCommandsHandler
    : public concord::consensus::ConcordCommandsHandler {
 public:
  PerformanceCommandsHandler(
      const concord::config::ConcordConfiguration& config,
      const concord::config::ConcordConfiguration& node_config,
      concord::kvbc::ILocalKeyValueStorageReadOnly& ros,
      concord::kvbc::IBlocksAppender& ba,
      concord::thin_replica::SubBufferList& subscriber_list,
      shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
      : ConcordCommandsHandler{config, node_config,     ros,
                               ba,     subscriber_list, prometheus_registry},
        logger_{logging::getLogger("concord.perf.handler")} {}

  void WriteEmptyBlock(concord::time::TimeContract* time_contract,
                       const opentracing::Span&) override {}

 private:
  logging::Logger logger_;
  PerfInitData init_data_;

  void ExecuteInitRequest(const PerfInitRequest& request,
                          PerfInitResponse& outResponse) {
    shared_ptr<MultiBlockData> data;
    LOG_DEBUG(logger_, "ExecuteInitRequest start, block_count: "
                           << request.block_count()
                           << ", kv_count: " << request.kv_count()
                           << ", key_size: " << request.key_size()
                           << ", value_size: " << request.value_size());
    string res =
        init_data_.CreateData(request.block_count(), request.kv_count(),
                              request.key_size(), request.value_size(), data);
    outResponse.set_id(data ? res : "Error");
    outResponse.set_workset_info(init_data_.GetInfo());
    LOG_DEBUG(logger_,
              "ExecuteInitRequest end , id: "
                  << res
                  << ", blocks count: " << (data ? data->blocks.size() : 0)
                  << ", total size: " << (data ? data->total_size : 0));
  }

  void ExecuteCleanRequest(const PerfCleanRequest& request,
                           PerfCleanResponse& outResponse) {
    init_data_.Clean(request.init_id());
    outResponse.set_message("Cleaned. New info : " + init_data_.GetInfo());
    LOG_DEBUG(logger_, "ExecuteCleanRequest, id: " << request.init_id()
                                                   << ", new init data size: "
                                                   << init_data_.GetInfo());
  }

  void ExecuteWriteRequest(const PerfWriteRequest& request,
                           PerfWriteResponse& outResponse, uint8_t flags) {
    using namespace concord::kvbc;
    using namespace concordUtils;

    string id;
    shared_ptr<MultiBlockData> data = nullptr;
    BlockId blockId = 0;
    if (request.has_from_init()) {
      const auto& fromInit = request.from_init();
      id = fromInit.init_id();
      blockId = fromInit.block_id();
      init_data_.GetBlocksData(id, data);
    } else {
      const auto& external = request.external();
      auto keyPrefix = external.key_prefix();
      auto valPrefix = external.val_prefix();
      auto kv_count = external.kv_count();
      auto ksize = external.key_size();
      auto vsize = external.value_size();
      auto start = chrono::steady_clock::now();
      id = init_data_.CreateData(1, kv_count, ksize, vsize, data, keyPrefix,
                                 valPrefix);
      auto end = chrono::steady_clock::now();
      auto time =
          chrono::duration_cast<chrono::milliseconds>(end - start).count();
      if (external.max_exec_time_milli() > 0 &&
          time < external.max_exec_time_milli()) {
        this_thread::sleep_for(
            chrono::milliseconds(external.max_exec_time_milli() - time));
      }
    }

    bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
    bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
    if (pre_execute && !has_pre_executed) {
      LOG_DEBUG(
          logger_,
          "ExecuteWriteRequest, pre-execute: skip block addition, id: " << id);
      outResponse.set_message(Status::OK().toString());
      return;
    }

    const auto payload = request.payload().size();
    if (data && data->blocks.size() > blockId) {
      const auto kvCount = data->blocks[blockId].size();
      LOG_DEBUG(logger_, "ExecuteWriteRequest, execution start"
                             << KVLOG(id, has_pre_executed, payload, kvCount));
      BlockId newBlockId;
      Status status = addBlock(data->blocks[blockId], newBlockId);
      if (!status.isOK()) {
        const string statusStr = status.toString();
        LOG_ERROR(logger_, "ExecuteWriteRequest, addBlock fail"
                               << KVLOG(id, payload, statusStr));
        outResponse.set_message("addBlock error, " + statusStr);
      }

      outResponse.set_message(status.toString());
      outResponse.set_new_block_id(newBlockId);
      LOG_DEBUG(logger_, "ExecuteWriteRequest, execution done"
                             << KVLOG(id, newBlockId, payload, kvCount));
    } else {
      LOG_ERROR(logger_,
                "ExecuteWriteRequest, execution done" << KVLOG(id, payload));
      outResponse.set_message("internal error: " + id);
    }
  }

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               const concord::consensus::ConcordRequestContext& request_context,
               uint8_t flags, concord::time::TimeContract* time_contract,
               opentracing::Span& parent_span,
               com::vmware::concord::ConcordResponse& response) override {
    if (!request.has_perf_request() && !request.has_pre_execution_result()) {
      // We have to ignore this, to allow time-only updates
      return true;
    }

    bool has_pre_executed = flags & bftEngine::MsgFlag::HAS_PRE_PROCESSED_FLAG;
    bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;
    PerfRequest perf_req;
    if (has_pre_executed && request.has_pre_execution_result()) {
      perf_req.ParseFromString(request.pre_execution_result().output());
    } else
      perf_req = request.perf_request();

    if (time_contract) {
      auto time = time_contract->GetTime();
    }

    auto type = perf_req.type();
    LOG_DEBUG(logger_, KVLOG(type, pre_execute, has_pre_executed));
    PerfResponse* perf_resp = response.mutable_perf_response();
    if (type == ::com::vmware::concord::PerfRequest_PerfRequestType::
                    PerfRequest_PerfRequestType_Init) {
      PerfInitRequest init_req;
      init_req.ParseFromString(perf_req.request_content());
      PerfInitResponse init_resp;
      ExecuteInitRequest(init_req, init_resp);
      string s;
      init_resp.SerializeToString(&s);
      perf_resp->set_response_content(s);
    } else if (type == ::com::vmware::concord::PerfRequest_PerfRequestType::
                           PerfRequest_PerfRequestType_Clean) {
      PerfCleanRequest clean_req;
      clean_req.ParseFromString(perf_req.request_content());
      PerfCleanResponse clean_resp;
      ExecuteCleanRequest(clean_req, clean_resp);
      string s;
      clean_resp.SerializeToString(&s);
      perf_resp->set_response_content(s);
    } else {
      PerfWriteRequest write_req;
      write_req.ParseFromString(perf_req.request_content());
      PerfWriteResponse write_resp;
      string s;
      if (has_pre_executed) {
        // Reset max_exec_time, as the command has been already executed
        write_req.mutable_external()->set_max_exec_time_milli(0);
      }
      // For request to be pre-executed ExecuteWriteRequest should be called
      // to support long-running requests.
      ExecuteWriteRequest(write_req, write_resp, flags);
      if (pre_execute && !has_pre_executed) {
        // For request to be pre-executed include the request in the response
        perf_req.SerializeToString(&s);
        auto* preExecutionResult = response.mutable_pre_execution_result();
        preExecutionResult->set_output(s);
        return true;
      }
      write_resp.SerializeToString(&s);
      perf_resp->set_response_content(s);
    }
    return true;
  }

  std::shared_ptr<bftEngine::ControlHandlers> getControlHandlers() override {
    return nullptr;
  }
  void setControlStateManager(std::shared_ptr<bftEngine::ControlStateManager>
                                  controlStateManager) override {}
};

}  // namespace performance
}  // namespace concord
