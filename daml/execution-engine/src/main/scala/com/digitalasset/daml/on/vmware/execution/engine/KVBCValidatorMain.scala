// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.daml.grpc.adapter.{ExecutionSequencerFactory, SingleThreadExecutionSequencerPool}
import com.daml.ledger.api.health.HealthChecks
import com.daml.metrics.Metrics
import com.daml.platform.server.api.services.grpc.GrpcHealthService
import com.digitalasset.daml.on.vmware.common.{
  KVBCHttpServer,
  KVBCMetricsRegistry,
  KVBCPrometheusMetricsEndpoint
}
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{Server, ServerBuilder}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

object KVBCValidatorMain extends App {

  val server = new KVBCValidatorServer()
  server.start()
  server.blockUntilShutdown()

}

class KVBCValidatorServer() {
  private[this] var server: Server = null
  private[this] val port = 55000

  private val metricsRegistry = new KVBCMetricsRegistry("kvutils")
  private val httpServer = new KVBCHttpServer()
  KVBCPrometheusMetricsEndpoint.createEndpoint(metricsRegistry.registry, httpServer.context)
  private val graphEndpoint =
    new KVBCGraphMetricsEndpoint(metricsRegistry.registry, httpServer.context)
  private val logger = LoggerFactory.getLogger(this.getClass)
  private val ledgerInboundMessageSizeMax: Int = 50 * 1024 * 1024 // 50 MiBytes

  // Use the global execution context. This uses threads proportional to
  // available processors.
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private implicit val system: ActorSystem = ActorSystem("validator")
  private implicit val mat: Materializer = ActorMaterializer()
  private implicit val esf: ExecutionSequencerFactory =
    new SingleThreadExecutionSequencerPool("validator-health", 1)

  private val validator = new KVBCValidator(new Metrics(metricsRegistry.registry))
  private val healthChecks = new HealthChecks("validator" -> validator)
  private val apiHealthService = new GrpcHealthService(healthChecks)
  private val apiReflectionService = ProtoReflectionService.newInstance()

  def start(): Unit = {
    httpServer.start()
    server = ServerBuilder
      .forPort(port)
      .addService(validator)
      .addService(apiHealthService)
      .addService(apiReflectionService)
      .maxInboundMessageSize(ledgerInboundMessageSizeMax)
      .build
      .start
    logger.info("Server started, port=" + port)
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
