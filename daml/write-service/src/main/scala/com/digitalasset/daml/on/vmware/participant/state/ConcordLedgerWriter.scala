// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.participant.state

import com.daml.ledger.api.health.{HealthStatus, Healthy}
import com.daml.ledger.participant.state.kvutils.api.{CommitMetadata, LedgerWriter}
import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.digitalasset.daml.on.vmware.write.service.kvbc.KvbcWriteClient
import com.digitalasset.kvbc.daml_commit.CommitRequest
import com.google.protobuf.ByteString
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class ConcordLedgerWriter(
    override val participantId: ParticipantId,
    commitTransaction: (CommitRequest, CommitMetadata) => Future[SubmissionResult],
    fetchCurrentHealth: () => HealthStatus = () => Healthy)(
    implicit executionContext: ExecutionContext)
    extends LedgerWriter {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def commit(
      correlationId: String,
      envelope: ByteString,
      metadata: CommitMetadata): Future[SubmissionResult] = {

    def resultHandler(submissionResult: SubmissionResult): SubmissionResult = {
      submissionResult match {
        case SubmissionResult.Acknowledged =>
          logger.info(s"Submission succeeded, correlationId=$correlationId")
        case SubmissionResult.Overloaded =>
          logger.error(s"Submission failed due to overload, correlationId=$correlationId ")
        case SubmissionResult.InternalError(reason) =>
          logger.error(
            s"Submission failed due to internal error, correlationId=$correlationId, reason='$reason'")
        case SubmissionResult.NotSupported => // This should never happen; if it happens, then there's a bug.
          val msg = s"Unsupported submission, correlationId=$correlationId"
          logger.error(msg)
          throw new IllegalStateException(msg)
      }
      submissionResult
    }

    def exceptionHandler(throwable: Throwable): Throwable = // This exception handler should never be triggered; if it is, then there's a bug.
      throwable match {
        case NonFatal(exception) =>
          logger.error(
            s"Submission failed with an exception, correlationId=$correlationId exception='$exception'")
          new IllegalStateException(exception.getMessage)
        case exception =>
          exception
      }

    logger.info(
      s"Sending commit request, correlationId=$correlationId envelopeSize=${envelope.size}")
    val commitRequest = CommitRequest(
      envelope,
      participantId,
      correlationId
    )
    commitTransaction(commitRequest, metadata)
      .transform(resultHandler, exceptionHandler)
  }

  override def currentHealth(): HealthStatus = fetchCurrentHealth()
}

object ConcordLedgerWriter {
  def create(participantId: ParticipantId, client: KvbcWriteClient)(
      implicit executionContext: ExecutionContext): ConcordLedgerWriter =
    new ConcordLedgerWriter(
      participantId,
      (commitRequest, commitMetadata) =>
        client.commitTransaction(commitRequest, commitMetadata)(executionContext),
      () => client.currentHealth)
}
