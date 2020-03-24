// Copyright 2020 VMware, all rights reserved
//
// concord open tracing utilities

#ifndef OPEN_TRACING_UTILS_HPP
#define OPEN_TRACING_UTILS_HPP

#include <log4cplus/loggingmacros.h>
#include <opentracing/tracer.h>
#include "kv_types.hpp"
#include "storage/kvb_key_types.h"

using SpanPtr = std::unique_ptr<opentracing::Span>;

namespace concord::utils {

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    log4cplus::Logger logger);

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    log4cplus::Logger logger, const std::string correlation_id,
                    bool create_span_on_failure = true);

SpanPtr ExtractSpan(concordUtils::SetOfKeyValuePairs& kv,
                    const std::string& child_name, log4cplus::Logger logger);

SpanPtr ExtractSpan(concordUtils::SetOfKeyValuePairs& kv,
                    const std::string& child_name, log4cplus::Logger logger,
                    const std::string& correlation_id,
                    bool create_span_on_failure = true);

void InjectSpan(const SpanPtr& span, concordUtils::SetOfKeyValuePairs& kv);

}  // namespace concord::utils

#endif
