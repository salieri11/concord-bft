package com.digitalasset.daml.on.vmware.execution.engine

import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.daml.ledger.participant.state.kvutils.DamlKvutils
import com.daml.ledger.validator.LedgerStateOperations.Key
import com.daml.ledger.validator.batch.BatchValidator
import com.daml.ledger.validator.privacy.LedgerStateOperationsWithAccessControl
import com.daml.ledger.validator.{CommitStrategy, DamlLedgerStateReader, QueryableReadSet}
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

      implicit val executionContext: ExecutionContext = ExecutionContext.global
      val instance =
        new PipelinedValidator(
          mockValidator,
          (_, _) => (mockDamlLedgerStateReader, mock[CommitStrategy[Unit]]))
      val inputStream = instance.validateSubmissions(mockStreamObserver)
      val validateRequest = EventToValidator().withValidateRequest(
        ValidateRequest()
          .withReplicaId(0)
          .withSubmission(aSubmission)
          .withParticipantId("aParticipantId")
          .withRecordTime(Timestamp.of(12, 34))
          .withCorrelationId("aCorrelationId"))
      inputStream.onNext(validateRequest)
      // Is there a more sane way to wait for the future in onNext to complete?!
      Thread.sleep(100)
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
          (_, _) => (mockQueryableReadSet, mock[CommitStrategy[Unit]]))
      val inputStream = instance.validateSubmissions(mockStreamObserver)
      val validateRequest = EventToValidator().withValidateRequest(
        ValidateRequest()
          .withReplicaId(0)
          .withSubmission(aSubmission)
          .withParticipantId("aParticipantId")
          .withCorrelationId("aCorrelationId"))

      assertThrows[NoSuchElementException](inputStream.onNext(validateRequest))
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

  private def aSubmission: ByteString = ByteString.copyFromUtf8("a submission")

  private trait CacheFactoryFunction {
    def create(): StateCache
  }
}
