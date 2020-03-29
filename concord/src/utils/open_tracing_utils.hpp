// Copyright 2020 VMware, all rights reserved
//
// concord open tracing utilities

#ifndef OPEN_TRACING_UTILS_HPP
#define OPEN_TRACING_UTILS_HPP

#include <log4cplus/loggingmacros.h>
#include <opentracing/tracer.h>
#include "sliver.hpp"
#include "storage/kvb_key_types.h"

#include <unordered_map>

using SpanPtr = std::unique_ptr<opentracing::Span>;

namespace concord::utils {

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

}  // namespace concord::utils

#endif
