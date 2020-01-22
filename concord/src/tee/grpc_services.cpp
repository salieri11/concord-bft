// Copyright 2020 VMware, all rights reserved

#include "grpc_services.hpp"
#include <log4cplus/mdc.h>
#include <opentracing/tracer.h>
#include <string>

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::TeeRequest;
using com::vmware::concord::TeeResponse;
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

  if (!pool_.send_request_sync(conc_request, true, *span.get(),
                               conc_response)) {
    LOG4CPLUS_ERROR(logger_, "RunTest transaction failed");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  TeeResponse tee_response = conc_response.tee_response();
  test_output->set_test_output(tee_response.tee_output());
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

}  // namespace tee
}  // namespace concord
