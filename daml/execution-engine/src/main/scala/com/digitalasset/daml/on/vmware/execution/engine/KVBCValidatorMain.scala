// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.daml.grpc.adapter.{ExecutionSequencerFactory, SingleThreadExecutionSequencerPool}
import com.daml.ledger.api.health.HealthChecks
import com.daml.lf.engine.{Engine, EngineConfig}
import com.daml.metrics.Metrics
import com.daml.platform.server.api.services.grpc.GrpcHealthService
import com.digitalasset.daml.on.vmware.common.{
  KVBCHttpServer,
  KVBCMetricsRegistry,
  KVBCPrometheusMetricsEndpoint
}
import com.digitalasset.daml.on.vmware.execution.engine.metrics.KVBCGraphMetricsEndpoint
import com.digitalasset.daml.on.vmware.tracing.TracingHelper
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{Server, ServerBuilder}
import io.opentracing.Tracer
import io.opentracing.util.GlobalTracer
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

object KVBCValidatorMain extends App {
  val server = new KVBCValidatorServer(GlobalTracer.get())
  server.start()
  server.blockUntilShutdown()
}

class KVBCValidatorServer(tracer: Tracer) {
  private[this] var server: Server = _

  private val GrpcPort = 55000
  private val MaxInboundMessageSize: Int = 50 * 1024 * 1024 // 50 MiBytes

  private val metricsRegistry = new KVBCMetricsRegistry("kvutils")
  private val httpServer = new KVBCHttpServer()
  KVBCPrometheusMetricsEndpoint.createEndpoint(metricsRegistry.registry, httpServer.context)
  private val _ =
    new KVBCGraphMetricsEndpoint(metricsRegistry.registry, httpServer.context)
  private val logger = LoggerFactory.getLogger(this.getClass)

  // Use the global execution context. This uses threads proportional to
  // available processors.
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private implicit val system: ActorSystem = ActorSystem("validator")
  private implicit val mat: Materializer = Materializer(system)
  private implicit val esf: ExecutionSequencerFactory =
    new SingleThreadExecutionSequencerPool("validator-health", 1)

  private val engine = new Engine(EngineConfig.Stable)
  private val validator = new ValidationServiceImpl(engine, new Metrics(metricsRegistry.registry))
  private val healthChecks = new HealthChecks("validator" -> validator)
  private val apiHealthService = new GrpcHealthService(healthChecks)
  private val apiReflectionService = ProtoReflectionService.newInstance()

  def start(): Unit = {
    httpServer.start()
    val tracingInterceptor = TracingHelper.createTracingInterceptor(tracer)
    server = ServerBuilder
      .forPort(GrpcPort)
      .addService(tracingInterceptor.intercept(validator))
      .addService(apiHealthService)
      .addService(apiReflectionService)
      .maxInboundMessageSize(MaxInboundMessageSize)
      .build
      .start
    logger.info("Server started, port=" + GrpcPort)
    sys.addShutdownHook {
      System.err.println("Shutting down...")
      stop()
    }
  }

  def stop(): Unit = {
    if (server != null) {
      server.shutdown()
    }
  }

  def blockUntilShutdown(): Unit = {
    if (server != null) {
      server.awaitTermination()
      httpServer.stop()
    }
  }
}
