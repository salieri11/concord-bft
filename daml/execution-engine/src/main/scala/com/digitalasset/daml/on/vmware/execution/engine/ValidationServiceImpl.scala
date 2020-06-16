// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import java.util.concurrent.Executors

import akka.stream.Materializer
import com.codahale.metrics.{InstrumentedExecutorService, MetricRegistry}
import com.daml.ledger.api.health.{HealthStatus, Healthy, ReportsHealth}
import com.daml.ledger.participant.state.kvutils.KeyValueCommitting
import com.daml.ledger.validator.batch.{
  BatchedSubmissionValidator,
  BatchedSubmissionValidatorParameters,
  ConflictDetection
}
import com.daml.lf.engine.Engine
import com.daml.metrics.{MetricName, Metrics}
import com.digitalasset.kvbc.daml_validator._
import io.grpc.stub.StreamObserver
import io.grpc.{BindableService, ServerServiceDefinition}

import scala.concurrent.ExecutionContext

class ValidationServiceImpl(engine: Engine, metrics: Metrics)(implicit materializer: Materializer)
    extends ValidationServiceGrpc.ValidationService
    with ReportsHealth
    with BindableService {
  implicit val executionContext: ExecutionContext =
    ValidationServiceImpl.createInstrumentedExecutionContext(metrics.registry)

  private val readerCommitterFactoryFunction =
    PipelinedValidator.createReaderCommitter(() => StateCaches.createDefault(metrics.registry)) _

  private val batchValidator =
    BatchedSubmissionValidator[Unit](
      BatchedSubmissionValidatorParameters.default,
      new KeyValueCommitting(engine, metrics),
      new ConflictDetection(metrics),
      metrics,
      engine,
    )

  private val pipelinedValidator = new PipelinedValidator(
    batchValidator,
    readerCommitterFactoryFunction,
    new ConcordLedgerStateOperations.Metrics(metrics.registry)
  )

  override def validate(
      responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] =
    pipelinedValidator.validateSubmissions(responseObserver)

  override def currentHealth(): HealthStatus = Healthy

  override def bindService(): ServerServiceDefinition =
    ValidationServiceGrpc.bindService(this, materializer.executionContext)

}

private[engine] object ValidationServiceImpl {
  def createInstrumentedExecutionContext(metricRegistry: MetricRegistry): ExecutionContext =
    ExecutionContext.fromExecutorService(
      new InstrumentedExecutorService(
        Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors()),
        metricRegistry,
        MetricName.DAML :+ "validator" :+ "parallel_validation" :+ "threadpool"
      ))
}
