// Copyright 2020 VMware, all rights reserved

#include "grpc_services.hpp"
#include <opentracing/tracer.h>
#include <string>
#include "Logger.hpp"

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::SkvbcRequest;
using com::vmware::concord::SkvbcResponse;
using com::vmware::concord::TeeRequest;
using com::vmware::concord::TeeResponse;
using com::vmware::concord::WriteBlockRequest;
using com::vmware::concord::tee::KVData;
using com::vmware::concord::tee::TestInput;
using com::vmware::concord::tee::TestOutput;
using grpc::ServerContext;
using grpc::ServerWriter;

using namespace std;

namespace concord {
namespace tee {

grpc::Status TeeServiceImpl::RunTest(grpc::ServerContext* context,
                                     const TestInput* test_input,
                                     TestOutput* test_output) {
  auto span = opentracing::Tracer::Global()->StartSpan("RunTest");

  ConcordRequest conc_request;
  ConcordResponse conc_response;
  TeeRequest* tee_request = conc_request.mutable_tee_request();

  tee_request->set_tee_input(test_input->test_input());

  if (!pool_.send_request_sync(conc_request, test_input->flags(), *span.get(),
                               conc_response)) {
    LOG_ERROR(logger_, "RunTest transaction failed");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  TeeResponse tee_response = conc_response.tee_response();
  test_output->set_test_output(tee_response.tee_output());
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

grpc::Status TeeServiceImpl::WriteBlock(::grpc::ServerContext* context,
                                        const KVData* kvdata,
                                        TestOutput* testout) {
  auto span = opentracing::Tracer::Global()->StartSpan("WriteBlock");

  ConcordRequest conc_request;
  ConcordResponse conc_response;

  WriteBlockRequest wbr;
  wbr.set_kvdata_content(kvdata->SerializeAsString());
  conc_request.mutable_tee_request()->mutable_wb_request()->CopyFrom(wbr);

  if (!pool_.send_request_sync(conc_request, false, *span.get(),
                               conc_response)) {
    LOG_ERROR(logger_, "WriteBlock transaction failed");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  testout->set_test_output(conc_response.tee_response().tee_output());
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

grpc::Status TeeServiceImpl::SkvbcRead(
    ::grpc::ServerContext* context,
    const ::com::vmware::concord::tee::RawSkvbcRequest* request,
    ::com::vmware::concord::tee::RawSkvbcResponse* response) {
  auto span = opentracing::Tracer::Global()->StartSpan("SkvbcRead");

  ConcordRequest conc_request;
  ConcordResponse conc_response;
  SkvbcRequest* skvbc_request =
      conc_request.mutable_tee_request()->mutable_skvbc_request();
  skvbc_request->set_request_content(request->content());

  if (!pool_.send_request_sync(conc_request, bftEngine::READ_ONLY_REQ,
                               *span.get(), conc_response)) {
    LOG_ERROR(logger_, "SKVBC read failed");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  TeeResponse tee_response = conc_response.tee_response();
  response->set_content(tee_response.skvbc_response().response_content());
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

grpc::Status TeeServiceImpl::SkvbcWrite(
    ::grpc::ServerContext* context,
    const ::com::vmware::concord::tee::RawSkvbcRequest* request,
    ::com::vmware::concord::tee::RawSkvbcResponse* response) {
  auto span = opentracing::Tracer::Global()->StartSpan("SkvbcWrite");

  ConcordRequest conc_request;
  ConcordResponse conc_response;
  SkvbcRequest* skvbc_request =
      conc_request.mutable_tee_request()->mutable_skvbc_request();
  skvbc_request->set_request_content(request->content());

  if (!pool_.send_request_sync(conc_request, request->flags(), *span.get(),
                               conc_response)) {
    LOG_ERROR(logger_, "SKVBC write failed");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  TeeResponse tee_response = conc_response.tee_response();
  response->set_content(tee_response.skvbc_response().response_content());
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

}  // namespace tee
}  // namespace concord
