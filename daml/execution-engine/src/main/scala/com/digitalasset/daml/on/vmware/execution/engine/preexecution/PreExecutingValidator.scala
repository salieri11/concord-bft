// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.daml.api.util.TimestampConversion
import com.daml.ledger.participant.state.pkvutils.KeySerializationStrategy
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReaderWithFingerprints.StateCacheWithFingerprints
import com.daml.ledger.validator.caching.{
  CachingDamlLedgerStateReaderWithFingerprints,
  ImmutablesOnlyCacheUpdatePolicy
}
import com.daml.ledger.validator.preexecution
import com.daml.ledger.validator.preexecution._
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList
import com.digitalasset.daml.on.vmware.common.Conversions.toReplicaId
import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateOperations.accessControlListToThinReplicaIds
import com.digitalasset.daml.on.vmware.execution.engine.Digests
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.PreprocessorToEngine.PreExecutionRequest
import com.digitalasset.kvbc.daml_validator.{
  PreExecutionOutput,
  PreprocessorFromEngine,
  PreprocessorToEngine
}
import com.google.protobuf.ByteString
import com.vmware.concord.concord._
import com.vmware.concord.kvb.concord_storage.ValueWithTrids
import io.grpc.stub.StreamObserver
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Orchestrates validation using pre-execution.
  * Works only for a single replica; in case more than one replica IDs are seen in the requests an
  * error is thrown.
  *
  * @param validator  validator instance to use
  * @param damlReaderProvider defines how we obtain reader instances per replica ID
  * @param metricsForLedgerStateOperations  shared metrics instance with [[com.digitalasset.daml.on.vmware.execution.engine.PipelinedValidator]]
  */
class PreExecutingValidator(
    validator: PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList],
    damlReaderProvider: (
        Long,
        LedgerStateReaderWithFingerprints) => DamlLedgerStateReaderWithFingerprints,
    metricsForLedgerStateOperations: ConcordLedgerStateOperationsMetrics)(
    implicit val executionContext: ExecutionContext) {
  import PreExecutingValidator._

  private val logger = LoggerFactory.getLogger(this.getClass)

  def preExecuteSubmission(responseObserver: StreamObserver[PreprocessorFromEngine])
    : StreamObserver[PreprocessorToEngine] = {
    val ledgerStateReader =
      new ConcordPreExecutionLedgerStateReader(
        responseObserver.onNext,
        metricsForLedgerStateOperations)
    new StreamObserver[PreprocessorToEngine] {
      override def onNext(value: PreprocessorToEngine): Unit = {
        value.toEngine match {
          case PreprocessorToEngine.ToEngine.PreexecutionRequest(request) =>
            handlePreExecutionRequest(request, ledgerStateReader, responseObserver)

          case PreprocessorToEngine.ToEngine.ReadResult(result) =>
            ledgerStateReader.handleReadResult(result)

          case PreprocessorToEngine.ToEngine.Empty =>
            sys.error("Message PreprocessorToEngine is empty!")
        }
      }

      override def onError(t: Throwable): Unit =
        logger.error(s"preExecuteSubmission() aborted due to an error: $t")

      override def onCompleted(): Unit =
        logger.trace("preExecuteSubmission() completed")
    }
  }

  private def handlePreExecutionRequest(
      request: PreprocessorToEngine.PreExecutionRequest,
      ledgerStateReader: ConcordPreExecutionLedgerStateReader,
      responseObserver: StreamObserver[PreprocessorFromEngine]): Unit = {
    logger.trace(
      s"Pre-execution request received, replicaId=${request.replicaId} " +
        s"correlationId=${request.correlationId} " +
        s"participantId=${request.submittingParticipantId}")
    ledgerStateReader.updateCorrelationId(Some(request.correlationId))
    val damlLedgerStateReader =
      damlReaderProvider(request.replicaId, ledgerStateReader)
    validator
      .validate(
        request.submission,
        request.correlationId,
        ParticipantId.assertFromString(request.submittingParticipantId),
        damlLedgerStateReader
      )
      .transformWith {
        case Failure(NonFatal(exception)) =>
          responseObserver.onError(exception)
          logger.info(
            s"Pre-execution validation failed, correlationId=${request.correlationId} " +
              s"participantId=${request.submittingParticipantId} " +
              s"exception=${exception.getLocalizedMessage}")
          Future.failed(exception)
        case Success(preExecutionResult) =>
          logger.debug(
            s"Submission with correlationId=${request.correlationId} validated in pre-execution")
          completePreExecutionRequest(request, preExecutionResult, responseObserver)
          logReadSet(preExecutionResult.readSet, ledgerStateReader, request.correlationId)
          logger.info(
            s"Pre-execution validation completed, correlationId=${request.correlationId} " +
              s"participantId=${request.submittingParticipantId} " +
              s"readSetSize=${preExecutionResult.readSet.size} " +
              s"successWriteSetSize=${preExecutionResult.successWriteSet.size} " +
              s"outOfTimeBoundsWriteSetSize=${preExecutionResult.outOfTimeBoundsWriteSet.size} ")
          Future.unit
      }
  }

  private def completePreExecutionRequest(
      request: PreExecutionRequest,
      preExecutionResult: preexecution.PreExecutionOutput[KeyValuePairsWithAccessControlList],
      responseObserver: StreamObserver[PreprocessorFromEngine]): Unit = {
    val preExecutionOutput =
      preExecutionResultToOutput(preExecutionResult, request.submittingParticipantId)
    val doneEvent =
      PreExecutionResult.of(
        Some(ReadSet.of(preExecutionResult.readSet.map {
          case (key, fingerprint) => KeyAndFingerprint.of(key, fingerprint)
        })),
        Some(preExecutionOutput.toByteString),
        Some(request.correlationId),
      )
    responseObserver.onNext(PreprocessorFromEngine().withPreexecutionResult(doneEvent))
    responseObserver.onCompleted()
  }

  private def logReadSet(
      keysAndFingerprints: PreExecutionCommitResult.ReadSet,
      ledgerStateReader: ConcordPreExecutionLedgerStateReader,
      correlationId: String): Unit =
    if (logger.isTraceEnabled) {
      val keys = keysAndFingerprints.map(_._1)
      val readKeysHash = Digests.hexDigestOfBytes(keys)
      val fingerprints = keysAndFingerprints.map(_._2)
      val readFingerprintsHash = Digests.hexDigestOfBytes(fingerprints)
      val completedReadHashes = ledgerStateReader.getCompletedReadHashes
      val readValuesHash =
        Digests.hexDigestOfBytes(keys.flatMap(completedReadHashes.get).map(ByteString.copyFrom))
      logger.trace(
        s"Produced read-set, size=${keysAndFingerprints.size} " +
          s"readKeysHash=$readKeysHash " +
          s"readFingerprintsHash=$readFingerprintsHash " +
          s"readValuesHash=$readValuesHash " +
          s"correlationId=$correlationId")
    }
}

object PreExecutingValidator {

  private[this] val readerPerReplica =
    collection.concurrent.TrieMap[Long, DamlLedgerStateReaderWithFingerprints]()

  /**
    * Creates or retrieves a reader per replica ID.
    *
    * @param cacheFactory          factory function for creating a state cache instance
    *                              (to be called per replica)
    * @param replicaId             ID of replica for which this instance is created for
    * @param ledgerStateReader defines how we read ledger state
    */
  private[engine] def getOrCreateReader(
      cacheFactory: () => StateCacheWithFingerprints,
      keySerializationStrategy: KeySerializationStrategy)(
      replicaId: Long,
      ledgerStateReader: LedgerStateReaderWithFingerprints)(
      implicit executionContext: ExecutionContext): DamlLedgerStateReaderWithFingerprints =
    readerPerReplica.getOrElseUpdate(
      replicaId,
      CachingDamlLedgerStateReaderWithFingerprints(
        cacheFactory(),
        ImmutablesOnlyCacheUpdatePolicy,
        ledgerStateReader,
        keySerializationStrategy
      )
    )

  private[preexecution] def toWriteSet(
      preExecutionCommitResultWriteSet: LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList)
    : WriteSet = {
    // Pre-execution result write set must be sorted to avoid non-determinism caused
    // by multiple execution engines producing write sets in different order.
    val writeSetSortedByKeys = preExecutionCommitResultWriteSet.sortBy(_._1.asReadOnlyByteBuffer())
    WriteSet.of(writeSetSortedByKeys.map {
      case (key, value, accessControlList) =>
        val adaptedAccessControlList = accessControlListToThinReplicaIds(accessControlList)
          .map(ByteString.copyFromUtf8)
        val outputValue = ValueWithTrids.of(adaptedAccessControlList, Some(value)).toByteString
        KeyValuePair.of(key, outputValue)
    })
  }

  private[preexecution] def preExecutionResultToOutput(
      preExecutionResult: preexecution.PreExecutionOutput[KeyValuePairsWithAccessControlList],
      submittingParticipantId: String
  ): PreExecutionOutput =
    PreExecutionOutput.of(
      preExecutionResult.minRecordTime.map(TimestampConversion.fromInstant),
      preExecutionResult.maxRecordTime.map(TimestampConversion.fromInstant),
      Some(toWriteSet(preExecutionResult.successWriteSet)),
      Some(toWriteSet(preExecutionResult.outOfTimeBoundsWriteSet)),
      preExecutionResult.involvedParticipants.map(toReplicaId).toSeq,
      submittingParticipantId
    )

}
