// Copyright 2020 VMware, all rights reserved
//
// concord open tracing utilities

#include "open_tracing_utils.hpp"

#include <sstream>
#include <stdexcept>
#include "storage/kvb_key_types.h"

using opentracing::expected;
using opentracing::string_view;

namespace {
// Note: they key is not stored in the final KVB
const ::concordUtils::Sliver kSpanContextKey =
    ::concordUtils::Sliver(new char[2]{0x00, 0x01}, 2);
}  // namespace

namespace concord::utils {

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    logging::Logger logger) {
  return ExtractSpan(context, child_name, logger, "", false);
}

SpanPtr ExtractSpan(const std::string& context, const std::string& child_name,
                    logging::Logger logger, const std::string correlation_id,
                    bool create_span_on_failure) {
  std::istringstream context_stream{context};
  auto parent_span_context =
      opentracing::Tracer::Global()->Extract(context_stream);
  if (!parent_span_context) {
    if (create_span_on_failure) {
      LOG_WARN(logger, "Failed to extract span, creating a new one, error:"
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
                    const std::string& child_name, logging::Logger logger,
                    const std::string& correlation_id) {
  auto res = kv.find(kSpanContextKey);
  if (res == kv.end()) {
    LOG_WARN(logger, "Update does not contain a span, creating a new one");
    return opentracing::Tracer::Global()->StartSpan(
        child_name, {opentracing::SetTag{kCorrelationIdTag, correlation_id}});
  }
  return ExtractSpan(res->second.toString(), child_name, logger,
                     correlation_id);
}

SpanPtr ExtractSpan(OpenTracingKeyValMap& kv, const std::string& child_name,
                    logging::Logger logger) {
  return ExtractSpan(kv, child_name, logger, "", false);
}

SpanPtr ExtractSpan(OpenTracingKeyValMap& kv, const std::string& child_name,
                    logging::Logger logger, const std::string& correlation_id,
                    bool create_span_on_failure) {
  auto res = kv.find(kSpanContextKey);
  if (res == kv.end()) {
    if (create_span_on_failure) {
      LOG_DEBUG(logger, "Update does not contain a span, creating a new one");
      return opentracing::Tracer::Global()->StartSpan(
          child_name, {opentracing::SetTag{kCorrelationIdTag, correlation_id}});
    } else {
      throw std::runtime_error("Failed to find span, error: Not in updates");
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

struct GrpcMetadataCarrier : opentracing::TextMapWriter {
  GrpcMetadataCarrier(grpc::ClientContext& client_context)
      : client_context_(client_context) {}

  expected<void> Set(string_view key, string_view value) const override {
    client_context_.AddMetadata(key, value);
    return {};
  }

  grpc::ClientContext& client_context_;
};

void InjectSpanAsMetadata(const opentracing::Span& span,
                          grpc::ClientContext* context) {
  GrpcMetadataCarrier metadata_carrier(*context);
  span.tracer().Inject(span.context(), metadata_carrier);
}

}  // namespace concord::utils
