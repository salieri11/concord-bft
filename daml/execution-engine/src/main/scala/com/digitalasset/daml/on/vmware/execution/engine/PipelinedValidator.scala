// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import java.time.Instant

import akka.stream.Materializer
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.batch.BatchedSubmissionValidator
import com.daml.ledger.validator.caching.{ImmutablesOnlyCacheUpdatePolicy, QueryableReadSet}
import com.daml.ledger.validator.privacy.{
  LedgerStateOperationsWithAccessControl,
  PrivacyAwareSubmissionValidator
}
import com.daml.ledger.validator.{CommitStrategy, DamlLedgerStateReader, LedgerStateOperations}
import com.daml.lf.data.Time
import com.digitalasset.daml.on.vmware.execution.engine.caching.StateCaches.StateCache
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.{EventFromValidator, EventToValidator}
import com.google.protobuf.ByteString
import io.grpc.stub.StreamObserver
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

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
    metricsForLedgerStateOperations: ConcordLedgerStateOperationsMetrics)(
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
              .transformWith {
                case Failure(NonFatal(exception)) =>
                  responseObserver.onError(exception)
                  logger.info(
                    s"Batch validation failed, correlationId=${request.correlationId} " +
                      s"participantId=${request.participantId} recordTime=${recordTime.toString} " +
                      s"exception=${exception.getLocalizedMessage}")
                  Future.failed(exception)
                case Success(_) =>
                  logger.debug(s"Submission with correlationId=${request.correlationId} validated")
                  val sortedReadSet =
                    recordingLedgerStateReader.getReadSet.toSeq.sorted
                  val writeSet = ledgerStateOperations.getAndClearWriteSet()
                  if (writeSet.isEmpty) {
                    logger.error(
                      s"Empty write-set generated, replicaId=${request.replicaId} correlationId=${request.correlationId}")
                  }
                  val doneEvent =
                    EventFromValidator.Done(readSet = sortedReadSet, writeSet = writeSet)
                  responseObserver.onNext(EventFromValidator().withDone(doneEvent))
                  responseObserver.onCompleted()
                  logReadSet(sortedReadSet, ledgerStateOperations, request.correlationId)
                  logger.info(
                    s"Batch validation completed, correlationId=${request.correlationId} " +
                      s"participantId=${request.participantId} recordTime=${recordTime.toString} " +
                      s"readSetSize=${sortedReadSet.size} writeSetSize=${writeSet.size}")
                  Future.unit
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

  private def logReadSet(
      keys: Seq[LedgerStateOperations.Key],
      operations: ConcordLedgerStateOperations,
      correlationId: String): Unit =
    if (logger.isTraceEnabled) {
      val readKeysHash = Digests.hexDigestOfBytes(keys)
      val completedReadHashes = operations.getCompletedReadHashes
      val readValuesHash =
        Digests.hexDigestOfBytes(keys.flatMap(completedReadHashes.get).map(ByteString.copyFrom))
      logger.trace(
        s"Produced read-set, size=${keys.size} readKeysHash=$readKeysHash readValuesHash=$readValuesHash correlationId=$correlationId")
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
      ImmutablesOnlyCacheUpdatePolicy,
      SharedKeySerializationStrategy)
  }
}
