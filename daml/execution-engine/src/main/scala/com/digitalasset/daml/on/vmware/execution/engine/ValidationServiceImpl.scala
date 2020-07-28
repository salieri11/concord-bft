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
import com.daml.ledger.validator.preexecution.PreExecutingSubmissionValidator
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList
import com.daml.lf.engine.Engine
import com.daml.metrics.{MetricName, Metrics}
import com.digitalasset.daml.on.vmware.execution.engine.caching.{
  PreExecutionStateCaches,
  StateCaches
}
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.daml.on.vmware.execution.engine.preexecution.PreExecutingValidator
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

  private val keyValueCommitting = new KeyValueCommitting(engine, metrics)

  private val batchSubmissionValidator =
    BatchedSubmissionValidator[Unit](
      BatchedSubmissionValidatorParameters.reasonableDefault,
      keyValueCommitting,
      new ConflictDetection(metrics),
      metrics,
    )

  private val preExecutingReaderFactoryFunction =
    PreExecutingValidator.getOrCreateReader(
      () => PreExecutionStateCaches.createDefault(metrics.registry),
      SharedKeySerializationStrategy) _

  private val preExecutingSubmissionValidator =
    new PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList](
      keyValueCommitting,
      metrics,
      SharedKeySerializationStrategy,
      LogFragmentsPreExecutingCommitStrategy(SharedKeySerializationStrategy)
    )

  private val concordLedgerStateOperationsMetrics =
    new ConcordLedgerStateOperationsMetrics(metrics.registry)

  private val pipelinedValidator = new PipelinedValidator(
    batchSubmissionValidator,
    readerCommitterFactoryFunction,
    concordLedgerStateOperationsMetrics
  )

  private val preExecutingValidator = new PreExecutingValidator(
    preExecutingSubmissionValidator,
    preExecutingReaderFactoryFunction,
    concordLedgerStateOperationsMetrics
  )

  override def validate(
      responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] =
    pipelinedValidator.validateSubmissions(responseObserver)

  override def preexecute(responseObserver: StreamObserver[PreprocessorFromEngine])
    : StreamObserver[PreprocessorToEngine] =
    preExecutingValidator.preExecuteSubmission(responseObserver)

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
