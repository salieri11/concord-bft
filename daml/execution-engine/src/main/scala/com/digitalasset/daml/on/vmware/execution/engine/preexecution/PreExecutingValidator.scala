// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.daml.api.util.TimestampConversion
import com.daml.ledger.participant.state.pkvutils.KeySerializationStrategy
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.caching.{
  CachingDamlLedgerStateReaderWithFingerprints,
  ImmutablesOnlyCacheUpdatePolicy
}
import com.daml.ledger.validator.preexecution._
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList
import com.digitalasset.daml.on.vmware.common.Conversions.toReplicaId
import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateOperations.accessControlListToThinReplicaIds
import com.digitalasset.daml.on.vmware.execution.engine.Digests
import com.digitalasset.daml.on.vmware.execution.engine.caching.PreExecutionStateCaches.StateCacheWithFingerprints
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
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
  */
class PreExecutingValidator(
    validator: PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList],
    damlReaderProvider: (
        Long,
        LedgerStateReaderWithFingerprints) => DamlLedgerStateReaderWithFingerprints,
    metricsForLedgerStateOperations: ConcordLedgerStateOperationsMetrics)(
    implicit val executionContext: ExecutionContext) {
  import PreExecutingValidator.toWriteSet

  private val logger = LoggerFactory.getLogger(this.getClass)

  def preExecuteSubmission(responseObserver: StreamObserver[PreprocessorFromEngine])
    : StreamObserver[PreprocessorToEngine] = {
    val ledgerStateOperations =
      new ConcordPreExecutionLedgerStateReader(
        responseObserver.onNext,
        metricsForLedgerStateOperations)
    new StreamObserver[PreprocessorToEngine] {
      override def onNext(value: PreprocessorToEngine): Unit = {
        value.toEngine match {
          case PreprocessorToEngine.ToEngine.PreexecutionRequest(request) =>
            logger.trace(
              s"Pre-execution request received, replicaId=${request.replicaId} " +
                s"correlationId=${request.correlationId} " +
                s"participantId=${request.submittingParticipantId}")
            ledgerStateOperations.updateCorrelationId(Some(request.correlationId))
            val damlLedgerStateReader =
              damlReaderProvider(request.replicaId, ledgerStateOperations)
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
                  val preExecutionOutput = PreExecutionOutput.of(
                    preExecutionResult.minRecordTime.map(TimestampConversion.fromInstant),
                    preExecutionResult.maxRecordTime.map(TimestampConversion.fromInstant),
                    Some(toWriteSet(preExecutionResult.successWriteSet)),
                    Some(toWriteSet(preExecutionResult.outOfTimeBoundsWriteSet)),
                    preExecutionResult.involvedParticipants.map(toReplicaId).toSeq,
                    request.submittingParticipantId
                  )
                  val doneEvent =
                    PreExecutionResult.of(
                      Some(ReadSet.of(preExecutionResult.readSet.map {
                        case (key, fingerprint) => KeyAndFingerprint.of(key, fingerprint)
                      })),
                      Some(preExecutionOutput.toByteString),
                      Some(request.correlationId),
                    )
                  responseObserver.onNext(
                    PreprocessorFromEngine().withPreexecutionResult(doneEvent))
                  responseObserver.onCompleted()
                  logReadSet(
                    preExecutionResult.readSet,
                    ledgerStateOperations,
                    request.correlationId)
                  logger.info(
                    s"Pre-execution validation completed, correlationId=${request.correlationId} " +
                      s"participantId=${request.submittingParticipantId} " +
                      s"readSetSize=${preExecutionResult.readSet.size} " +
                      s"successWriteSetSize=${preExecutionResult.successWriteSet.size} " +
                      s"outOfTimeBoundsWriteSetSize=${preExecutionResult.outOfTimeBoundsWriteSet.size} ")
                  Future.unit
              }

          case PreprocessorToEngine.ToEngine.ReadResult(result) =>
            ledgerStateOperations.handleReadResult(result)

          case PreprocessorToEngine.ToEngine.Empty =>
            sys.error("Message PreprocessorToEngine is empty!")
        }
      }

      override def onError(t: Throwable): Unit =
        logger.error(s"validateSubmissions() aborted due to an error: $t")

      override def onCompleted(): Unit =
        logger.trace("validateSubmissions() completed")
    }
  }

  private def logReadSet(
      keysAndFingerprints: PreExecutionCommitResult.ReadSet,
      operations: ConcordPreExecutionLedgerStateReader,
      correlationId: String): Unit =
    if (logger.isTraceEnabled) {
      val keys = keysAndFingerprints.map(_._1)
      val readKeysHash = Digests.hexDigestOfBytes(keys)
      val fingerprints = keysAndFingerprints.map(_._2)
      val readFingerprintsHash = Digests.hexDigestOfBytes(fingerprints)
      val completedReadHashes = operations.getCompletedReadHashes
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
    * @param ledgerStateOperations defines how we access ledger state
    */
  private[engine] def getOrCreateReader(
      cacheFactory: () => StateCacheWithFingerprints,
      keySerializationStrategy: KeySerializationStrategy)(
      replicaId: Long,
      ledgerStateOperations: LedgerStateReaderWithFingerprints)(
      implicit executionContext: ExecutionContext): DamlLedgerStateReaderWithFingerprints =
    readerPerReplica.getOrElseUpdate(
      replicaId,
      // TODO Use SDK factory method once available
      new CachingDamlLedgerStateReaderWithFingerprints(
        cacheFactory(),
        ImmutablesOnlyCacheUpdatePolicy.shouldCacheOnRead,
        keySerializationStrategy,
        new RawToDamlLedgerStateReaderWithFingerprintsAdapterAccess(
          ledgerStateOperations,
          keySerializationStrategy)
      )
    )

  private[preexecution] def toWriteSet(
      preExecutionCommitResultWriteSet: LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList)
    : WriteSet =
    WriteSet.of(preExecutionCommitResultWriteSet.map {
      case (key, value, accessControlList) =>
        val adaptedAccessControlList = accessControlListToThinReplicaIds(accessControlList)
          .map(ByteString.copyFromUtf8)
        val outputValue = ValueWithTrids.of(adaptedAccessControlList, Some(value)).toByteString
        KeyValuePair.of(key, outputValue)
    })
}
