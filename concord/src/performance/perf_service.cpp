// Copyright 2020 VMware, all rights reserved

#include "perf_service.hpp"
#include <log4cplus/mdc.h>
#include <opentracing/tracer.h>
#include <string>
#include "kv_types.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::PerfRequest;
using com::vmware::concord::PerfResponse;
using com::vmware::concord::performance::PerfCleanRequest;
using com::vmware::concord::performance::PerfCleanResponse;
using com::vmware::concord::performance::PerfInitRequest;
using com::vmware::concord::performance::PerfInitResponse;
using com::vmware::concord::performance::PerfWriteRequest;
using com::vmware::concord::performance::PerfWriteResponse;

using namespace std;

namespace concord {
namespace performance {

grpc::Status PerformanceServiceImp::PerfInit(grpc::ServerContext* context,
                                             const PerfInitRequest* request,
                                             PerfInitResponse* response) {
  string inData;
  request->SerializeToString(&inData);
  string outData;
  auto res = Send(::com::vmware::concord::PerfRequest_PerfRequestType::
                      PerfRequest_PerfRequestType_Init,
                  inData, outData);
  response->ParseFromString(outData);
  return res;
}

grpc::Status PerformanceServiceImp::PerfClear(grpc::ServerContext* context,
                                              const PerfCleanRequest* request,
                                              PerfCleanResponse* response) {
  string inData;
  request->SerializeToString(&inData);
  string outData;
  auto res = Send(::com::vmware::concord::PerfRequest_PerfRequestType::
                      PerfRequest_PerfRequestType_Clean,
                  inData, outData);
  response->ParseFromString(outData);
  return res;
}

grpc::Status PerformanceServiceImp::PerfWrite(grpc::ServerContext* context,
                                              const PerfWriteRequest* request,
                                              PerfWriteResponse* response) {
  string inData;
  request->SerializeToString(&inData);
  string outData;
  auto res = Send(::com::vmware::concord::PerfRequest_PerfRequestType::
                      PerfRequest_PerfRequestType_Write,
                  inData, outData);
  response->ParseFromString(outData);
  return res;
}

grpc::Status PerformanceServiceImp::Send(
    ::com::vmware::concord::PerfRequest_PerfRequestType type, string& inData,
    string& outData) {
  LOG_DEBUG(logger_, "Send, type: " << to_string(type));
  auto span = opentracing::Tracer::Global()->StartSpan(
      "PerformanceServiceImp::Send_type_" + to_string(type));
  ConcordRequest conc_request;
  ConcordResponse conc_response;
  PerfRequest* perf_req = conc_request.mutable_perf_request();

  perf_req->set_request_content(inData);
  perf_req->set_type(type);

  if (!pool_.send_request_sync(conc_request, false, *span.get(),
                               conc_response)) {
    LOG_ERROR(logger_,
              "PerformanceServiceImp::Send failed, type:" << to_string(type));
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  PerfResponse resp = conc_response.perf_response();
  outData = resp.response_content();
  LOG_DEBUG(logger_, "Send done, type: " << to_string(type));
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

}  // namespace performance
}  // namespace concord