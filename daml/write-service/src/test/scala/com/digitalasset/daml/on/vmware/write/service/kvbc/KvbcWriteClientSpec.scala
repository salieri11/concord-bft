package com.digitalasset.daml.on.vmware.write.service.kvbc

import com.daml.ledger.participant.state.kvutils.api.SimpleCommitMetadata
import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.digitalasset.kvbc.daml_commit.CommitResponse.CommitStatus
import com.digitalasset.kvbc.daml_commit.{CommitRequest, CommitResponse, CommitServiceGrpc}
import com.google.protobuf.ByteString
import io.grpc.{Status, StatusRuntimeException}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class KvbcWriteClientSpec extends AsyncWordSpec with Matchers with MockitoSugar {

  "KvbcWriteClient" should {
    "return Acknowledged in case of an OK commit response" in {
      val commitClientMock = mock[CommitServiceGrpc.CommitServiceStub]
      when(commitClientMock.commitTransaction(any()))
        .thenReturn(Future.successful(new CommitResponse(CommitStatus.OK)))
      val kvbcWriteClient = createKvbcClient(commitClientMock)
      kvbcWriteClient.commitTransaction(aCommitRequest, aCommitMetadata)(executionContext).map {
        actual =>
          actual shouldBe SubmissionResult.Acknowledged
      }
    }

    "return InternalError in case of an ERROR commit response" in {
      val commitClientMock = mock[CommitServiceGrpc.CommitServiceStub]
      when(commitClientMock.commitTransaction(any()))
        .thenReturn(Future.successful(new CommitResponse(CommitStatus.ERROR)))
      val kvbcWriteClient = createKvbcClient(commitClientMock)
      kvbcWriteClient.commitTransaction(aCommitRequest, aCommitMetadata)(executionContext).map {
        actual =>
          actual shouldBe SubmissionResult.InternalError("ERROR")
      }
    }

    "return Overloaded in case of a RESOURCE_EXHAUSTED commit status exception" in {
      val commitClientMock = mock[CommitServiceGrpc.CommitServiceStub]
      when(commitClientMock.commitTransaction(any()))
        .thenReturn(Future.failed(new StatusRuntimeException(Status.RESOURCE_EXHAUSTED)))
      val kvbcWriteClient = createKvbcClient(commitClientMock)
      kvbcWriteClient.commitTransaction(aCommitRequest, aCommitMetadata)(executionContext).map {
        actual =>
          actual shouldBe SubmissionResult.Overloaded
      }
    }

    "return InternalError in case of a non-fatal exception" in {
      val commitClientMock = mock[CommitServiceGrpc.CommitServiceStub]
      val exception = new RuntimeException("ERROR")
      when(commitClientMock.commitTransaction(any()))
        .thenReturn(Future.failed(exception))
      val kvbcWriteClient = createKvbcClient(commitClientMock)
      kvbcWriteClient.commitTransaction(aCommitRequest, aCommitMetadata)(executionContext).map {
        actual =>
          actual shouldBe SubmissionResult.InternalError(exception.toString)
      }
    }
  }

  private def createKvbcClient(
      commitClientMock: CommitServiceGrpc.CommitServiceStub): KvbcWriteClient =
    new KvbcWriteClient(commitClientMock, () => true, _ => false)

  private val anEnvelope = ByteString.copyFrom(Array[Byte](0, 1, 2))
  private val aParticipantId = ParticipantId.assertFromString("aParticipantId")
  private val aCommitRequest = CommitRequest(
    anEnvelope,
    aParticipantId,
    "correlationId"
  )
  private val aCommitMetadata = SimpleCommitMetadata(None)
}
