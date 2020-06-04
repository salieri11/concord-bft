package com.digitalasset.daml.on.vmware.tracing

import io.opentracing.Tracer
import io.opentracing.contrib.grpc.TracingServerInterceptor

object TracingHelper {
  def createTracingInterceptor(tracer: Tracer): TracingServerInterceptor =
    TracingServerInterceptor.newBuilder
      .withTracer(tracer)
      .withVerbosity
      .build
}
