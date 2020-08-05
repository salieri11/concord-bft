// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import java.time.Instant

import com.codahale.metrics.MetricRegistry
import com.daml.api.util.TimestampConversion
import com.daml.dec.DirectExecutionContext
import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.daml.ledger.participant.state.pkvutils.KeySerializationStrategy
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.DamlLedgerStateReader
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReaderWithFingerprints.StateCacheWithFingerprints
import com.daml.ledger.validator.caching.QueryableReadSet
import com.daml.ledger.validator.preexecution.{
  DamlLedgerStateReaderWithFingerprints,
  LedgerStateReaderWithFingerprints,
  PreExecutingSubmissionValidator,
  PreExecutionOutput
}
import com.daml.ledger.validator.privacy.LogFragmentsPreExecutingCommitStrategy.KeyValuePairsWithAccessControlList
import com.daml.ledger.validator.privacy.{PublicAccess, RestrictedAccess}
import com.digitalasset.daml.on.vmware.common.Conversions.toReplicaId
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.daml.on.vmware.execution.engine.preexecution.PreExecutingValidator.toWriteSet
import com.digitalasset.kvbc.daml_validator.PreprocessorToEngine.PreExecutionRequest
import com.digitalasset.kvbc.daml_validator.{
  PreprocessorFromEngine,
  PreprocessorToEngine,
  PreExecutionOutput => ProtoPreExecutionOutput
}
import com.google.protobuf.ByteString
import com.vmware.concord.concord._
import com.vmware.concord.kvb.concord_storage.ValueWithTrids
import io.grpc.stub.StreamObserver
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.{ExecutionContext, Future}

class PreExecutingValidatorSpec
    extends AsyncWordSpec
    with Matchers
    with MockitoSugar
    with AkkaBeforeAndAfterAll {
  private trait DamlLedgerStateReaderWithQueryableReadSet
      extends DamlLedgerStateReader
      with QueryableReadSet

  "validateSubmissions" should {
    "validate submission and return pre-execution result" in {
      val mockDamlLedgerStateReaderWithFingerprints =
        mock[DamlLedgerStateReaderWithFingerprints]
      val mockValidator = mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]]
      val submissionCaptor =
        ArgumentCaptor.forClass(classOf[ByteString]).asInstanceOf[ArgumentCaptor[ByteString]]
      when(
        mockValidator
          .validate(submissionCaptor.capture(), anyString(), any(), any())(any())
      ).thenReturn(Future.successful(aPreExecutionOutput))
      val mockStreamObserver = mock[StreamObserver[PreprocessorFromEngine]]

      implicit val executionContext: ExecutionContext = DirectExecutionContext
      val instance =
        new PreExecutingValidator(
          mockValidator,
          (_, _) => mockDamlLedgerStateReaderWithFingerprints,
          createMetrics())
      val inputStream = instance.preExecuteSubmission(mockStreamObserver)
      inputStream.onNext(aPreExecutionRequest())
      inputStream.onCompleted()

      val expectedProtoPreExcutionOutput =
        ProtoPreExecutionOutput.of(
          aPreExecutionOutput.minRecordTime.map(TimestampConversion.fromInstant),
          aPreExecutionOutput.maxRecordTime.map(TimestampConversion.fromInstant),
          Some(toWriteSet(aPreExecutionOutput.successWriteSet)),
          Some(toWriteSet(aPreExecutionOutput.outOfTimeBoundsWriteSet)),
          aPreExecutionOutput.involvedParticipants.map(toReplicaId).toSeq,
          aParticipantId
        )

      val expectedPreExecutionResultEvent =
        PreprocessorFromEngine().withPreexecutionResult(
          PreExecutionResult.of(
            Some(ReadSet.of(aPreExecutionOutput.readSet.map {
              case (key, fingerprint) => KeyAndFingerprint.of(key, fingerprint)
            })),
            Some(expectedProtoPreExcutionOutput.toByteString),
            Some(aCorrelationId),
          ))
      verify(mockStreamObserver, times(1)).onNext(expectedPreExecutionResultEvent)
      verify(mockStreamObserver, times(1)).onCompleted()
      submissionCaptor.getValue shouldBe aSubmission
    }

    "report validation error via onError() callback" in {
      val mockDamlLedgerStateReaderWithFingerprints =
        mock[DamlLedgerStateReaderWithFingerprints]
      val mockValidator = mock[PreExecutingSubmissionValidator[KeyValuePairsWithAccessControlList]]
      when(mockValidator.validate(any(), any(), any(), any())(any()))
        .thenReturn(Future.failed(new IllegalArgumentException("something is not right")))
      val mockStreamObserver = mock[StreamObserver[PreprocessorFromEngine]]
      implicit val executionContext: ExecutionContext = DirectExecutionContext
      val instance =
        new PreExecutingValidator(
          mockValidator,
          (_, _) => mockDamlLedgerStateReaderWithFingerprints,
          createMetrics())

      val inputStream = instance.preExecuteSubmission(mockStreamObserver)
      inputStream.onNext(aPreExecutionRequest())

      verify(mockStreamObserver, times(1)).onError(any())
      verify(mockValidator, times(1)).validate(any(), any(), any(), any())(any())
      succeed
    }
  }

  "getOrCreateReader" should {
    "create a new reader for an unseen replica ID" in {
      val mockCacheFactory = mock[CacheFactoryFunction]
      when(mockCacheFactory.create()).thenReturn(mock[StateCacheWithFingerprints])
      val mockLedgerStateReaderWithFingerprints =
        mock[LedgerStateReaderWithFingerprints]
      val mockKeySerializationStrategy = mock[KeySerializationStrategy]
      val validatorFactory =
        PreExecutingValidator.getOrCreateReader(
          mockCacheFactory.create,
          mockKeySerializationStrategy) _

      // Create 2 instances for IDs 0 and 1.
      validatorFactory(0, mockLedgerStateReaderWithFingerprints)
      validatorFactory(1, mockLedgerStateReaderWithFingerprints)
      validatorFactory(0, mockLedgerStateReaderWithFingerprints)

      // Expect same instance to be reused as for ID 0.
      verify(mockCacheFactory, times(2)).create()
      succeed
    }
  }

  "toWriteSet" should {
    "populate ValueWithTrids message for restricted ACL" in {
      val expectedThinReplicaId = "a participant"
      val expectedValue = ValueWithTrids
        .of(Seq(ByteString.copyFromUtf8(expectedThinReplicaId)), Some(aValue))
        .toByteString
      val input: KeyValuePairsWithAccessControlList = Seq(
        (aKey, aValue, RestrictedAccess(Set(ParticipantId.assertFromString(expectedThinReplicaId))))
      )
      PreExecutingValidator.toWriteSet(input) shouldBe WriteSet.of(
        Seq(
          KeyValuePair.of(aKey, expectedValue)
        ))
    }

    "populate ValueWithTrids message for public ACL" in {
      val expectedValue = ValueWithTrids.of(Seq.empty, Some(aValue)).toByteString
      val input: KeyValuePairsWithAccessControlList = Seq((aKey, aValue, PublicAccess))
      PreExecutingValidator.toWriteSet(input) shouldBe WriteSet.of(
        Seq(
          KeyValuePair.of(aKey, expectedValue)
        ))
    }
  }

  private def createMetrics(): ConcordLedgerStateOperationsMetrics =
    new ConcordLedgerStateOperationsMetrics(new MetricRegistry)

  private def aSubmission: ByteString = ByteString.copyFromUtf8("a submission")

  private val aCorrelationId = "aCorrelationId"
  private val aParticipantId = "aParticipantId"
  private val aSpanContext = ByteString.copyFromUtf8("aSpanContext")
  private val aKey = ByteString.copyFromUtf8("a key")
  private val aValue = ByteString.copyFromUtf8("a value")

  private def aPreExecutionRequest(): PreprocessorToEngine = {
    PreprocessorToEngine().withPreexecutionRequest(
      PreExecutionRequest()
        .withReplicaId(0)
        .withSubmission(aSubmission)
        .withSubmittingParticipantId(aParticipantId)
        .withCorrelationId(aCorrelationId)
        .withSpanContext(aSpanContext))
  }

  private val anInstant = Instant.now()
  private val anotherInstant = Instant.now()

  private val aReadSet = Seq(
    ByteString.copyFromUtf8("aKey") -> ByteString.copyFromUtf8("fingerprint1")
  )

  private val aSuccessWriteSet = Seq(
    (
      ByteString.copyFromUtf8("aKey"),
      ByteString.copyFromUtf8("value2"),
      PublicAccess
    ))

  private val aTimeOutOfBoundsWriteSet = Seq(
    (
      ByteString.copyFromUtf8("aKey"),
      ByteString.copyFromUtf8("value3"),
      PublicAccess
    ))

  private def aPreExecutionOutput =
    new PreExecutionOutput[KeyValuePairsWithAccessControlList](
      Some(anInstant),
      Some(anotherInstant),
      aSuccessWriteSet,
      aTimeOutOfBoundsWriteSet,
      aReadSet,
      Set(ParticipantId.assertFromString(aParticipantId))
    )

  private trait CacheFactoryFunction {
    def create(): StateCacheWithFingerprints
  }
}
