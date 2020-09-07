// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.daml.dec.DirectExecutionContext
import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.daml.ledger.participant.state.kvutils.DamlKvutils._
import com.daml.ledger.participant.state.kvutils.export.NoopLedgerDataExporter
import com.daml.ledger.participant.state.pkvutils.KeySerializationStrategy
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.LedgerStateOperations.Key
import com.daml.ledger.validator.batch.BatchedSubmissionValidator
import com.daml.ledger.validator.caching.CachingDamlLedgerStateReader.StateCache
import com.daml.ledger.validator.caching.QueryableReadSet
import com.daml.ledger.validator.privacy.{
  LedgerStateOperationsWithAccessControl,
  LogFragmentingCommitStrategy
}
import com.daml.ledger.validator.{CommitStrategy, DamlLedgerStateReader}
import com.daml.lf.data.Ref.IdString
import com.digitalasset.daml.on.vmware.common.Constants
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.{EventFromValidator, EventToValidator, ValidateRequest}
import com.google.protobuf.ByteString
import com.google.protobuf.timestamp.Timestamp
import io.grpc.stub.StreamObserver
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.{ExecutionContext, Future}

class PipelinedValidatorSpec
    extends AsyncWordSpec
    with Matchers
    with MockitoSugar
    with AkkaBeforeAndAfterAll {
  private trait DamlLedgerStateReaderWithQueryableReadSet
      extends DamlLedgerStateReader
      with QueryableReadSet

  "validateSubmissions" should {
    "validate submission and return sorted read set" in {
      val mockDamlLedgerStateReaderWithQueryableReadSet =
        mock[DamlLedgerStateReaderWithQueryableReadSet]
      val expectedReadSet = Set(ByteString.copyFromUtf8("key2"), ByteString.copyFromUtf8("key1"))
      when(mockDamlLedgerStateReaderWithQueryableReadSet.getReadSet).thenReturn(expectedReadSet)
      val mockValidator = mock[BatchedSubmissionValidator[Unit]]
      val submissionCaptor =
        ArgumentCaptor.forClass(classOf[ByteString]).asInstanceOf[ArgumentCaptor[ByteString]]
      when(
        mockValidator
          .validateAndCommit(submissionCaptor.capture(), anyString(), any(), any(), any(), any())(
            any(),
            any()))
        .thenReturn(Future.unit)
      val mockStreamObserver = mock[StreamObserver[EventFromValidator]]

      implicit val executionContext: ExecutionContext = DirectExecutionContext
      val instance =
        new PipelinedValidator(
          mockValidator,
          (_, _) => (mockDamlLedgerStateReaderWithQueryableReadSet, mock[CommitStrategy[Unit]]),
          createMetrics())
      val inputStream = instance.validateSubmissions(mockStreamObserver)
      inputStream.onNext(aValidateRequest())
      inputStream.onCompleted()

      verify(mockDamlLedgerStateReaderWithQueryableReadSet, times(1)).getReadSet
      val expectedDoneEvent =
        EventFromValidator().withDone(EventFromValidator.Done(expectedReadSet.toSeq.sorted))
      verify(mockStreamObserver, times(1)).onNext(expectedDoneEvent)
      verify(mockStreamObserver, times(1)).onCompleted()
      submissionCaptor.getValue shouldBe aSubmission
    }

    "return write-set as part of Done message" in {
      val mockDamlLedgerStateReaderWithQueryableReadSet =
        mock[DamlLedgerStateReaderWithQueryableReadSet]
      when(mockDamlLedgerStateReaderWithQueryableReadSet.getReadSet).thenReturn(Set.empty[Key])
      val mockStreamObserver = mock[StreamObserver[EventFromValidator]]
      val eventCaptor = ArgumentCaptor
        .forClass(classOf[EventFromValidator])
        .asInstanceOf[ArgumentCaptor[EventFromValidator]]
      doNothing().when(mockStreamObserver).onNext(eventCaptor.capture())
      val mockValidator = mock[BatchedSubmissionValidator[Unit]]
      when(
        mockValidator
          .validateAndCommit(any(), anyString(), any(), any(), any(), any())(any(), any()))
        .thenAnswer { invocation =>
          val commitStrategy = invocation.getArgument(5).asInstanceOf[CommitStrategy[Unit]]
          implicit val executionContext: ExecutionContext =
            invocation.getArgument(7).asInstanceOf[ExecutionContext]
          for {
            _ <- commitParty("1", commitStrategy)
            _ <- commitParty("2", commitStrategy)
          } yield ()
        }
      implicit val executionContext: ExecutionContext = DirectExecutionContext
      val instance =
        new PipelinedValidator(
          mockValidator,
          (_, ledgerStateOperations) =>
            (
              mockDamlLedgerStateReaderWithQueryableReadSet,
              new LogFragmentingCommitStrategy(
                ledgerStateOperations,
                KeySerializationStrategy.createDefault(),
                NoopLedgerDataExporter)),
          createMetrics()
        )

      val inputStream = instance.validateSubmissions(mockStreamObserver)
      inputStream.onNext(aValidateRequest())
      inputStream.onCompleted()

      verify(mockStreamObserver, times(1)).onNext(any())
      verify(mockStreamObserver, times(1)).onCompleted()
      eventCaptor.getAllValues should have size 1
      eventCaptor.getValue.fromValidator.isDone should be(true)
      val Some(actualDoneEvent) = eventCaptor.getValue.fromValidator.done
      // We expect 2 log fragments and 2 party allocation entries to be written.
      actualDoneEvent.writeSet should have size 4
    }

    "throw in case recordTime is not specified in request" in {
      val mockQueryableReadSet = mock[DamlLedgerStateReader with QueryableReadSet]
      val mockValidator = mock[BatchedSubmissionValidator[Unit]]
      when(mockValidator.validateAndCommit(any(), any(), any(), any(), any(), any())(any(), any()))
        .thenReturn(Future.unit)
      val mockStreamObserver = mock[StreamObserver[EventFromValidator]]

      val instance =
        new PipelinedValidator(
          mockValidator,
          (_, _) => (mockQueryableReadSet, mock[CommitStrategy[Unit]]),
          createMetrics())
      val inputStream = instance.validateSubmissions(mockStreamObserver)

      assertThrows[NoSuchElementException](inputStream.onNext(aValidateRequest(recordTime = None)))
    }

    "report validation error via onError() callback" in {
      val mockQueryableReadSet = mock[DamlLedgerStateReader with QueryableReadSet]
      val mockValidator = mock[BatchedSubmissionValidator[Unit]]
      when(mockValidator.validateAndCommit(any(), any(), any(), any(), any(), any())(any(), any()))
        .thenReturn(Future.failed(new IllegalArgumentException("something is not right")))
      val mockStreamObserver = mock[StreamObserver[EventFromValidator]]
      implicit val executionContext: ExecutionContext = DirectExecutionContext
      val instance =
        new PipelinedValidator(
          mockValidator,
          (_, _) => (mockQueryableReadSet, mock[CommitStrategy[Unit]]),
          createMetrics())

      val inputStream = instance.validateSubmissions(mockStreamObserver)
      inputStream.onNext(aValidateRequest())

      verify(mockStreamObserver, times(1)).onError(any())
      verify(mockValidator, times(1)).validateAndCommit(any(), any(), any(), any(), any(), any())(
        any(),
        any())
      succeed
    }
  }

  "createReaderCommitter" should {
    "create new cache for an unseen replica ID" in {
      val mockCacheFactory = mock[CacheFactoryFunction]
      when(mockCacheFactory.create()).thenReturn(mock[StateCache])
      val mockLedgerStateOperations = mock[LedgerStateOperationsWithAccessControl]
      val validatorFactory = PipelinedValidator.createReaderCommitter(mockCacheFactory.create) _

      // Create 2 instances for IDs 0 and 1.
      validatorFactory(0, mockLedgerStateOperations)
      validatorFactory(1, mockLedgerStateOperations)
      // Expect same instance to be reused as for ID 0.
      validatorFactory(0, mockLedgerStateOperations)

      verify(mockCacheFactory, times(2)).create()
      succeed
    }

    "must use same log fragment prefixes as thin-replica client" in {
      SharedKeySerializationStrategy.isFragmentKey(Constants.fragmentKeyPrefix) shouldBe true
    }
  }

  private def createMetrics(): ConcordLedgerStateOperationsMetrics =
    new ConcordLedgerStateOperationsMetrics(new MetricRegistry)

  def commitParty(partyName: String, commitStrategy: CommitStrategy[Unit])(
      implicit executionContext: ExecutionContext): Future[Unit] =
    commitStrategy.commit(
      aParticipantId(),
      "correlation ID",
      DamlLogEntryId.getDefaultInstance,
      DamlLogEntry.newBuilder
        .setPartyAllocationEntry(DamlPartyAllocationEntry.newBuilder.setDisplayName(partyName))
        .build,
      Map.empty,
      Map(DamlStateKey.newBuilder.setParty(partyName).build -> DamlStateValue.getDefaultInstance)
    )

  private def aSubmission: ByteString = ByteString.copyFromUtf8("a submission")

  private def aValidateRequest(
      recordTime: Option[Timestamp] = Some(Timestamp.of(12, 34))): EventToValidator = {
    val validateRequest =
      ValidateRequest()
        .withReplicaId(0)
        .withSubmission(aSubmission)
        .withParticipantId("aParticipantId")
        .withCorrelationId("aCorrelationId")
    recordTime
      .map { value =>
        EventToValidator().withValidateRequest(validateRequest.withRecordTime(value))
      }
      .getOrElse(EventToValidator().withValidateRequest(validateRequest))
  }

  private def aParticipantId(): IdString.ParticipantId =
    ParticipantId.assertFromString("aParticipantId")

  private trait CacheFactoryFunction {
    def create(): StateCache
  }
}
