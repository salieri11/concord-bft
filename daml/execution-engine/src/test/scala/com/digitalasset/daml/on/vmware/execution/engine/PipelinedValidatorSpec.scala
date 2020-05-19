package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.daml.dec.DirectExecutionContext
import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.daml.ledger.participant.state.kvutils.DamlKvutils
import com.daml.ledger.validator.LedgerStateOperations.Key
import com.daml.ledger.validator.batch.BatchValidator
import com.daml.ledger.validator.privacy.LedgerStateOperationsWithAccessControl
import com.daml.ledger.validator.{CommitStrategy, DamlLedgerStateReader, QueryableReadSet}
import com.digitalasset.daml.on.vmware.execution.engine
import com.digitalasset.daml.on.vmware.execution.engine.StateCaches.StateCache
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
  "validateSubmissions" should {
    "validate submission and return sorted read set" in {
      val mockQueryableReadSet = mock[QueryableReadSet]
      val mockDamlLedgerStateReader = new DamlLedgerStateReader with QueryableReadSet {
        override def readState(
            keys: Seq[DamlKvutils.DamlStateKey]): Future[Seq[Option[DamlKvutils.DamlStateValue]]] =
          Future.successful(Seq.empty) // Not called.

        override def getReadSet: Set[Key] = mockQueryableReadSet.getReadSet
      }
      val expectedReadSet = Set(ByteString.copyFromUtf8("key2"), ByteString.copyFromUtf8("key1"))
      when(mockQueryableReadSet.getReadSet).thenReturn(expectedReadSet)
      val mockValidator = mock[BatchValidator[Unit]]
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
          (_, _) => (mockDamlLedgerStateReader, mock[CommitStrategy[Unit]]),
          createMetrics())
      val inputStream = instance.validateSubmissions(mockStreamObserver)
      inputStream.onNext(aValidateRequest())
      inputStream.onCompleted()

      verify(mockQueryableReadSet, times(1)).getReadSet
      val expectedDoneEvent =
        EventFromValidator().withDone(EventFromValidator.Done(expectedReadSet.toSeq.sorted))
      verify(mockStreamObserver, times(1)).onNext(expectedDoneEvent)
      verify(mockStreamObserver, times(1)).onCompleted()
      submissionCaptor.getValue shouldBe aSubmission
    }

    "throw in case recordTime is not specified in request" in {
      val mockQueryableReadSet = mock[DamlLedgerStateReader with QueryableReadSet]
      val mockValidator = mock[BatchValidator[Unit]]
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
      val mockValidator = mock[BatchValidator[Unit]]
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
  }

  private def createMetrics(): ConcordLedgerStateOperations.Metrics =
    new ConcordLedgerStateOperations.Metrics(new MetricRegistry)

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

  private trait CacheFactoryFunction {
    def create(): StateCache
  }
}
