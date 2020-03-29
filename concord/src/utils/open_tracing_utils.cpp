// Copyright 2020 VMware, all rights reserved
//
// concord open tracing utilities

#include "open_tracing_utils.hpp"

#include <sstream>
#include <stdexcept>
#include <strstream>
#include "storage/kvb_key_types.h"

namespace {
// Note: they key is not stored in the final KVB
const ::concordUtils::Sliver kSpanContextKey =
    ::concordUtils::Sliver(new char[2]{0x00, 0x01}, 2);

const std::string kCorrelationIdTag = "cid";
}  // namespace

namespace concord::utils {

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    log4cplus::Logger logger) {
  return ExtractSpan(context, child_name, logger, "", false);
}

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    log4cplus::Logger logger, const std::string correlation_id,
                    bool create_span_on_failure) {
  std::istringstream context_stream{context};
  auto parent_span_context =
      opentracing::Tracer::Global()->Extract(context_stream);
  if (!parent_span_context) {
    if (create_span_on_failure) {
      LOG4CPLUS_WARN(logger,
                     "Failed to extract span, creating a new one, error:"
                         << parent_span_context.error());
      return opentracing::Tracer::Global()->StartSpan(
          child_name, {opentracing::SetTag{kCorrelationIdTag, correlation_id}});
    } else {
      std::ostringstream stream;
      stream << "Failed to extract span, error:" << parent_span_context.error();
      throw std::runtime_error(stream.str());
    }
  }
  return opentracing::Tracer::Global()->StartSpan(
      child_name, {opentracing::ChildOf(parent_span_context->get())});
}

SpanPtr ExtractSpan(const OpenTracingKeyValMap& kv,
                    const std::string& child_name, log4cplus::Logger logger,
                    const std::string& correlation_id) {
  auto res = kv.find(kSpanContextKey);
  if (res == kv.end()) {
    LOG4CPLUS_WARN(logger,
                   "Update does not contain a span, creating a new one");
    return opentracing::Tracer::Global()->StartSpan(
        child_name, {opentracing::SetTag{kCorrelationIdTag, correlation_id}});
  }
  return ExtractSpan(res->second.toString(), child_name, logger,
                     correlation_id);
}

SpanPtr ExtractSpan(OpenTracingKeyValMap& kv, const std::string& child_name,
                    log4cplus::Logger logger) {
  return ExtractSpan(kv, child_name, logger, "", false);
}

SpanPtr ExtractSpan(OpenTracingKeyValMap& kv, const std::string& child_name,
                    log4cplus::Logger logger, const std::string& correlation_id,
                    bool create_span_on_failure) {
  auto res = kv.find(kSpanContextKey);
  if (res == kv.end()) {
    if (create_span_on_failure) {
      LOG4CPLUS_WARN(logger,
                     "Update does not contain a span, creating a new one");
      return opentracing::Tracer::Global()->StartSpan(
          child_name, {opentracing::SetTag{kCorrelationIdTag, correlation_id}});
    } else {
      throw std::runtime_error(
          "Update does not contain a span, creating a new one");
    }
  }
  auto span = ExtractSpan(res->second.toString(), child_name, logger,
                          correlation_id, create_span_on_failure);
  kv.erase(res);
  return span;
}

void InjectSpan(const SpanPtr& span, OpenTracingKeyValMap& kv) {
  std::ostringstream context;
  span->tracer().Inject(span->context(), context);
  kv[kSpanContextKey] = context.str();
}

}  // namespace concord::utils
