// Copyright 2020 VMware, all rights reserved
//
// concord open tracing utilities

#ifndef OPEN_TRACING_UTILS_HPP
#define OPEN_TRACING_UTILS_HPP

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>
#include <opentracing/span.h>
#include <opentracing/tracer.h>
#include <unordered_map>

#include "sliver.hpp"
#include "storage/kvb_key_types.h"

using SpanPtr = std::unique_ptr<opentracing::Span>;

namespace concord::utils {

const std::string kCorrelationIdTag = "cid";
const std::string kRequestSeqNumTag = "req_seq_num";
const std::string kClientIdTag = "client_id";
const std::string kRequestFlagsTag = "req_flags";
const std::string kRequestSizeTag = "req_size";
const std::string kExecResultTag = "exec_result";
const std::string kExecRespSizeTag = "exec_resp_size";
const std::string kReplicaIdTag = "rid";

using OpenTracingKeyValMap =
    std::unordered_map<concordUtils::Sliver, concordUtils::Sliver>;

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    log4cplus::Logger logger);

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    log4cplus::Logger logger, const std::string correlation_id,
                    bool create_span_on_failure = true);

SpanPtr ExtractSpan(OpenTracingKeyValMap& kv, const std::string& child_name,
                    log4cplus::Logger logger);

SpanPtr ExtractSpan(OpenTracingKeyValMap& kv, const std::string& child_name,
                    log4cplus::Logger logger, const std::string& correlation_id,
                    bool create_span_on_failure = true);

void InjectSpan(const SpanPtr& span, OpenTracingKeyValMap& kv);

void InjectSpanAsMetadata(const opentracing::Span& span,
                          grpc::ClientContext* context);

}  // namespace concord::utils

#endif
