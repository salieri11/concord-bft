// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.daml.api.util.TimestampConversion
import com.daml.ledger.participant.state.pkvutils.KeySerializationStrategy
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReaderWithFingerprints.StateCacheWithFingerprints
import com.daml.ledger.validator.caching.{
  CachingDamlLedgerStateReaderWithFingerprints,
  ImmutablesOnlyCacheUpdatePolicy
}
import com.daml.ledger.validator.preexecution
import com.daml.ledger.validator.preexecution.{
  DamlLedgerStateReaderWithFingerprints,
  PreExecutingSubmissionValidator
}
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList
import com.daml.lf.data.Ref
import com.digitalasset.daml.on.vmware.common.Conversions.toReplicaId
import com.digitalasset.daml.on.vmware.execution.engine.preexecution.PreExecutingValidator.toWriteSet
import com.digitalasset.daml.on.vmware.execution.engine.preexecution.StaticLedgerStateReader.KeysUnavailableException
import com.digitalasset.kvbc.daml_validator.{
  PreExecuteRequest,
  PreExecuteResponse,
  PreExecutionOutput
}
import com.vmware.concord.concord.{KeyAndFingerprint, PreExecutionResult, ReadSet}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class PreExecutingMultiStepValidator(
    validator: PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList],
    cacheFactory: () => StateCacheWithFingerprints,
    keySerializationStrategy: KeySerializationStrategy)(
    implicit executionContext: ExecutionContext) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private[this] val cachePerReplica =
    collection.concurrent.TrieMap[Long, StateCacheWithFingerprints]()

  def preExecute(request: PreExecuteRequest): Future[PreExecuteResponse] = {
    val preExecutionRequest = request.preexecutionRequest.get
    val ledgerStateReader =
      createLedgerStateReader(preExecutionRequest.replicaId, request.readResult)
    validator
      .validate(
        preExecutionRequest.submission,
        preExecutionRequest.correlationId,
        Ref.ParticipantId.assertFromString(preExecutionRequest.submittingParticipantId),
        ledgerStateReader
      )
      .transformWith {
        case Success(preExecutionResult) =>
          logger.info(
            "Pre-execution validation completed, " +
              s"correlationId=${preExecutionRequest.correlationId} " +
              s"participantId=${preExecutionRequest.submittingParticipantId} " +
              s"readSetSize=${preExecutionResult.readSet.size} " +
              s"successWriteSetSize=${preExecutionResult.successWriteSet.size} " +
              s"outOfTimeBoundsWriteSetSize=${preExecutionResult.outOfTimeBoundsWriteSet.size} ")
          val result = successfulResult(preExecutionRequest, preExecutionResult)
          val response =
            PreExecuteResponse().withPreexecutionResult(result)
          Future.successful(response)
        case Failure(KeysUnavailableException(missingKeys)) =>
          logger.info(
            "Pre-execution validation needs more data, " +
              s"correlationId=${preExecutionRequest.correlationId} " +
              s"participantId=${preExecutionRequest.submittingParticipantId} " +
              s"missingKeysCount=${missingKeys.size}")
          val readRequest = PreExecuteResponse.ReadRequest.of(missingKeys)
          val response =
            PreExecuteResponse().withReadRequest(readRequest)
          Future.successful(response)
        case Failure(exception) =>
          logger.info(
            "Pre-execution validation failed, " +
              s"correlationId=${preExecutionRequest.correlationId} " +
              s"participantId=${preExecutionRequest.submittingParticipantId} " +
              s"exception=${exception.getLocalizedMessage}")
          Future.failed(exception)
      }
  }

  private[preexecution] def createLedgerStateReader(
      replicaId: Long,
      maybeReadResult: Option[PreExecuteRequest.ReadResult])
    : DamlLedgerStateReaderWithFingerprints = {
    val cache = cachePerReplica.getOrElseUpdate(replicaId, cacheFactory())
    val ledgerStateReader = StaticLedgerStateReader(
      maybeReadResult.getOrElse(PreExecuteRequest.ReadResult()))
    CachingDamlLedgerStateReaderWithFingerprints(
      cache,
      ImmutablesOnlyCacheUpdatePolicy,
      ledgerStateReader,
      keySerializationStrategy
    )
  }

  private def successfulResult(
      request: PreExecuteRequest.PreExecutionRequest,
      preExecutionResult: preexecution.PreExecutionOutput[KeyValuePairsWithAccessControlList])
    : PreExecutionResult = {
    val preExecutionOutput = PreExecutingValidator.preExecutionResultToOutput(
      preExecutionResult,
      request.submittingParticipantId)
    PreExecutionResult.of(
      Some(ReadSet.of(preExecutionResult.readSet.map {
        case (key, fingerprint) => KeyAndFingerprint.of(key, fingerprint)
      })),
      Some(preExecutionOutput.toByteString),
      Some(request.correlationId),
    )
  }
}
