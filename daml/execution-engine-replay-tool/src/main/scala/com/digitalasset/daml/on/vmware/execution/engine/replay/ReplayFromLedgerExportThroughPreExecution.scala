// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.replay

import com.daml.ledger.participant.state.kvutils.DamlKvutils.DamlSubmission
import com.daml.ledger.participant.state.kvutils.export.{SubmissionInfo, WriteSet}
import com.daml.ledger.participant.state.kvutils.{Bytes, Envelope}
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.ValidationFailed
import com.digitalasset.kvbc.daml_validator.PreExecuteRequest.{PreExecutionRequest, ReadResult}
import com.digitalasset.kvbc.daml_validator.PreExecuteResponse.FromEngine.{
  PreexecutionResult,
  ReadRequest
}
import com.digitalasset.kvbc.daml_validator.ValidationServiceGrpc.ValidationServiceStub
import com.digitalasset.kvbc.daml_validator.{
  KeyValueFingerprintTriple,
  PreExecuteRequest,
  PreExecuteResponse,
  PreExecutionOutput
}
import com.google.protobuf.ByteString
import com.vmware.concord.concord.PreExecutionResult
import io.opentracing.{SpanContext, Tracer}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

final class ReplayFromLedgerExportThroughPreExecution(
    override val validationService: ValidationServiceStub,
    override val tracer: Tracer,
)(implicit executionContext: ExecutionContext)
    extends ReplayFromLedgerExport {

  override protected def runValidation(
      validationService: ValidationServiceStub,
      ignoredSpanContext: SpanContext, // Will be removed from the pre-execution protocol
      info: SubmissionInfo,
      store: InMemoryStore): Seq[WriteSet] = {
    val result = List.newBuilder[WriteSet]
    for (submission <- extractSubmissionsFromEnvelope(info.submissionEnvelope)) {
      result +=
        Await.result(
          validateThroughPreExecution(
            submission,
            info.participantId,
            ReplicaId,
            info.correlationId,
            store),
          10.seconds
        )
    }
    result.result()
  }

  private def validateThroughPreExecution(
      submission: DamlSubmission,
      participantId: ParticipantId,
      replicaId: Long,
      correlationId: String,
      store: InMemoryStore): Future[WriteSet] = {

    println(s"Processing submission $correlationId")

    val envelope = Envelope.enclose(submission)

    val preExecutionRequest = PreExecutionRequest.of(
      envelope,
      participantId,
      replicaId,
      correlationId,
      ByteString.EMPTY
    )

    def handleReadRequest(readRequest: PreExecuteResponse.ReadRequest): Future[WriteSet] = {
      println(s"Received read request of ${readRequest.keys.size} keys for $correlationId")
      go(Some(ReadResult.of(readFromStore(store, readRequest.keys).map {
        case (key, maybeValue) =>
          val value = maybeValue.getOrElse(ByteString.EMPTY)
          KeyValueFingerprintTriple
            .of(key, value, value)
      })))
    }

    def handlePreExecutionResult(
        preExecutionResult: PreExecutionResult): Future[Seq[(Bytes, Bytes)]] = {

      val preExecutionOutput = PreExecutionOutput
        .parseFrom(preExecutionResult.output.get.toByteArray)

      println(s"Received pre-execution result of $correlationId: " +
        s"success write set size ${preExecutionOutput.successWriteSet.size}, " +
        s"out-of-time-bounds write set size ${preExecutionOutput.outOfTimeBoundsWriteSet.size}, " +
        s"minRecordTime ${preExecutionOutput.minRecordTime} " +
        s"maxRecordTime ${preExecutionOutput.maxRecordTime}")

      Future {
        preExecutionOutput.successWriteSet match {
          case Some(writeSet) =>
            writeToStore(store, writeSet.writes.map(write => (write.key, write.value)))
          case None =>
            throw new IllegalArgumentException(
              "Received empty write set in successful pre-execution result set")
        }
      }
    }

    def go(maybeReadResult: Option[ReadResult] = None): Future[WriteSet] =
      for {
        preExecutionResponse <- validationService
          .preExecuteMultiStep(
            PreExecuteRequest.of(
              Some(preExecutionRequest),
              maybeReadResult
            )
          )
        result <- preExecutionResponse.fromEngine match {
          case PreexecutionResult(preExecutionResult) =>
            handlePreExecutionResult(preExecutionResult)
          case ReadRequest(readRequest) =>
            handleReadRequest(readRequest)
          case other =>
            throw new IllegalArgumentException(
              s"Received unsupported reply from execution engine: $other")
        }
      } yield result

    go()
  }

  override protected def validateResults(
      expectedResults: Seq[WriteSet],
      actualResults: Seq[WriteSet]): Unit =
    println("Validation replay was OK but write sets checking is currently not implemented")

  private def extractSubmissionsFromEnvelope(envelope: Bytes): Seq[DamlSubmission] =
    Envelope.open(envelope) match {
      case Right(Envelope.SubmissionMessage(submission)) =>
        List(submission)

      case Right(Envelope.SubmissionBatchMessage(batch)) =>
        batch.getSubmissionsList.asScala.flatMap { correlatedSubmission =>
          extractSubmissionsFromEnvelope(correlatedSubmission.getSubmission)
        }

      case Right(other) =>
        throw ValidationFailed.ValidationError(
          s"Unexpected message in envelope: ${other.getClass.getSimpleName}")

      case Left(error) =>
        throw ValidationFailed.ValidationError(s"Cannot open envelope: $error")
    }
}
