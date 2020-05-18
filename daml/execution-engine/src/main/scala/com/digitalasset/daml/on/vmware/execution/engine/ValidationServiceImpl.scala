package com.digitalasset.daml.on.vmware.execution.engine

import akka.stream.Materializer
import com.daml.ledger.api.health.{HealthStatus, Healthy, ReportsHealth}
import com.daml.ledger.participant.state.kvutils.KeyValueCommitting
import com.daml.ledger.validator.batch.{BatchValidator, BatchValidatorParameters, ConflictDetection}
import com.daml.lf.engine.Engine
import com.daml.metrics.Metrics
import com.digitalasset.kvbc.daml_validator._
import io.grpc.stub.StreamObserver
import io.grpc.{BindableService, ServerServiceDefinition}

import scala.concurrent.{ExecutionContext, Future}

class ValidationServiceImpl(engine: Engine, metrics: Metrics)(implicit materializer: Materializer)
    extends ValidationServiceGrpc.ValidationService
    with ReportsHealth
    with BindableService {
  implicit val executionContext: ExecutionContext = materializer.executionContext

  private val readerCommitterFactoryFunction =
    PipelinedValidator.createReaderCommitter(() => StateCaches.createDefault(metrics.registry)) _

  private val batchValidator =
    BatchValidator[Unit](
      BatchValidatorParameters.default,
      new KeyValueCommitting(engine, metrics),
      new ConflictDetection(metrics),
      metrics,
      engine,
    )

  private val pipelinedValidator = new PipelinedValidator(
    batchValidator,
    readerCommitterFactoryFunction
  )(materializer = materializer, executionContext = ExecutionContext.global)

  override def validate(
      responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] =
    pipelinedValidator.validateSubmissions(responseObserver)

  override def validateSubmission(request: ValidateRequest): Future[ValidateResponse] =
    Future.failed(new UnsupportedOperationException)

  override def validatePendingSubmission(
      request: ValidatePendingSubmissionRequest): Future[ValidatePendingSubmissionResponse] =
    Future.failed(new UnsupportedOperationException)

  override def currentHealth(): HealthStatus = Healthy

  override def bindService(): ServerServiceDefinition =
    ValidationServiceGrpc.bindService(this, executionContext)

}
