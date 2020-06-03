package com.digitalasset.daml.on.vmware.execution.engine

import java.time.Instant

import akka.stream.Materializer
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.batch.BatchedSubmissionValidator
import com.daml.ledger.validator.caching.ImmutablesOnlyCacheUpdatePolicy
import com.daml.ledger.validator.privacy.{
  LedgerStateOperationsWithAccessControl,
  PrivacyAwareSubmissionValidator
}
import com.daml.ledger.validator.{CommitStrategy, DamlLedgerStateReader, QueryableReadSet}
import com.daml.lf.data.Time
import com.digitalasset.daml.on.vmware.execution.engine.StateCaches.StateCache
import com.digitalasset.kvbc.daml_validator.{EventFromValidator, EventToValidator}
import io.grpc.stub.StreamObserver
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.control.NonFatal

/**
  * Orchestrates validation using pipelined state access.
  * Works only for a single replica; in case more than one replica IDs are seen in the requests an
  * error is thrown.
  *
  * @param validator  validator instance to use
  * @param readerCommitterFactory defines how we create ephemeral reader/committer instances per replica ID
  * @param materializer   materializer to be used during parallel validation
  */
class PipelinedValidator(
    validator: BatchedSubmissionValidator[Unit],
    readerCommitterFactory: (Long, LedgerStateOperationsWithAccessControl) => (
        DamlLedgerStateReader with QueryableReadSet,
        CommitStrategy[Unit]),
    metricsForLedgerStateOperations: ConcordLedgerStateOperations.Metrics)(
    implicit materializer: Materializer,
    val executionContext: ExecutionContext) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def validateSubmissions(
      responseObserver: StreamObserver[EventFromValidator]): StreamObserver[EventToValidator] = {
    val ledgerStateOperations =
      new ConcordLedgerStateOperations(responseObserver.onNext, metricsForLedgerStateOperations)
    new StreamObserver[EventToValidator] {
      override def onNext(value: EventToValidator): Unit = {
        value.toValidator match {
          case EventToValidator.ToValidator.ValidateRequest(request) =>
            logger.trace(
              s"Validation request received, replicaId=${request.replicaId} correlationId=${request.correlationId} participantId=${request.participantId}")
            ledgerStateOperations.updateCorrelationId(Some(request.correlationId))
            val (recordingLedgerStateReader, commitStrategy) =
              readerCommitterFactory(request.replicaId, ledgerStateOperations)
            val recordTime = parseTimestamp(request.recordTime.get).toInstant
            validator
              .validateAndCommit(
                request.submission,
                request.correlationId,
                recordTime,
                ParticipantId.assertFromString(request.participantId),
                recordingLedgerStateReader,
                commitStrategy
              )
              .andThen {
                case Failure(NonFatal(exception)) =>
                  responseObserver.onError(exception)
                  logger.info(
                    s"Batch validation failed, correlationId=${request.correlationId} " +
                      s"participantId=${request.participantId} recordTime=${recordTime.toString} " +
                      s"exception=${exception.getLocalizedMessage}")
              }
              .foreach { _ =>
                logger.debug(s"Submission with correlationId=${request.correlationId} validated")
                val sortedReadSet =
                  recordingLedgerStateReader.getReadSet.toSeq.sorted
                responseObserver.onNext(
                  EventFromValidator().withDone(EventFromValidator.Done(sortedReadSet)))
                responseObserver.onCompleted()
                logger.info(
                  s"Batch validation completed, correlationId=${request.correlationId} " +
                    s"participantId=${request.participantId} recordTime=${recordTime.toString} " +
                    s"readSetSize=${sortedReadSet.size}")
              }
          case EventToValidator.ToValidator.ReadResult(result) =>
            ledgerStateOperations.handleReadResult(result)

          case EventToValidator.ToValidator.Empty =>
            sys.error("Message EventToValidator is empty!")
        }
      }

      override def onError(t: Throwable): Unit =
        logger.error(s"validateSubmissions() aborted due to an error: $t")

      override def onCompleted(): Unit =
        logger.trace("validateSubmissions() completed")
    }
  }

  private def parseTimestamp(ts: com.google.protobuf.timestamp.Timestamp): Time.Timestamp =
    Time.Timestamp.assertFromInstant(Instant.ofEpochSecond(ts.seconds, ts.nanos))
}

object PipelinedValidator {
  private val cachePerReplica = collection.concurrent.TrieMap[Long, StateCache]()

  /**
    * Factory function to be used for creating ephemeral reader/committer instances.
    * Required for caching to work per replica ID.
    * @param cacheFactory   factory function for creating a state cache instance
    *                       (to be called per replica)
    * @param replicaId      ID of replica for which this instance is created for
    * @param ledgerStateOperations  defines how we access ledger state
    * @return
    */
  private[engine] def createReaderCommitter(cacheFactory: () => StateCache)(
      replicaId: Long,
      ledgerStateOperations: LedgerStateOperationsWithAccessControl)(
      implicit executionContext: ExecutionContext)
    : (DamlLedgerStateReader with QueryableReadSet, CommitStrategy[Unit]) = {
    val cache = cachePerReplica.getOrElseUpdate(replicaId, cacheFactory())
    PrivacyAwareSubmissionValidator.cachingReaderAndCommitStrategyFrom(
      ledgerStateOperations,
      cache,
      ImmutablesOnlyCacheUpdatePolicy)
  }
}