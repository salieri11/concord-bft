package com.digitalasset.daml.on.vmware.participant.state

import com.daml.ledger.participant.state.kvutils.api.LedgerWriter
import com.daml.ledger.participant.state.v1.{
  LedgerId,
  ParticipantId,
  SubmissionResult
}
import com.digitalasset.daml.on.vmware.write.service.KVBCClient
import com.digitalasset.kvbc.daml_commit.{CommitRequest, CommitResponse}
import com.daml.ledger.api.health.{HealthStatus, Healthy}
import com.google.protobuf.ByteString
import io.grpc.{Status, StatusRuntimeException}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

class ConcordLedgerWriter(
    ledgerId: LedgerId,
    override val participantId: ParticipantId,
    commitTransaction: CommitRequest => Future[CommitResponse],
    fetchCurrentHealth: () => HealthStatus = () => Healthy)(
    implicit executionContext: ExecutionContext)
    extends LedgerWriter {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  override def commit(correlationId: String,
                      envelope: ByteString): Future[SubmissionResult] = {
    logger.info(s"Sending commit request, correlationId=$correlationId envelopeSize=${envelope.size}")
    val commitRequest = CommitRequest(
      envelope,
      participantId.toString,
      correlationId
    )
    commitTransaction(commitRequest)
      .map {
        case response if response.status == CommitResponse.CommitStatus.OK =>
          logger.info(s"Submission succeeded, correlationId=$correlationId")
          SubmissionResult.Acknowledged
        case response =>
          val responseStatus = response.status.toString
          logger.error(
            s"Submission failed with an error, correlationId=$correlationId " +
              s"error='$responseStatus'")
          SubmissionResult.InternalError(responseStatus)
      }
      .recover {
        case grpc: StatusRuntimeException
            if grpc.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED =>
          logger.error(
            s"Submission failed with an exception, correlationId=$correlationId exception='$grpc'")
          SubmissionResult.Overloaded
        case exception @ _ =>
          logger.error(
            s"Submission failed with an exception, correlationId=$correlationId exception='$exception'")
          SubmissionResult.InternalError(exception.toString)
      }
  }

  override def currentHealth(): HealthStatus = fetchCurrentHealth()
}

object ConcordLedgerWriter {
  def create(ledgerId: LedgerId,
             participantId: ParticipantId,
             client: KVBCClient)(
      implicit executionContext: ExecutionContext): ConcordLedgerWriter =
    new ConcordLedgerWriter(ledgerId,
                            participantId,
                            client.commitTransaction,
                            () => client.currentHealth)
}
