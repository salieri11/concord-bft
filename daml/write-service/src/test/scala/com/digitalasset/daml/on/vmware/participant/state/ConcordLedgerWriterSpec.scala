package com.digitalasset.daml.on.vmware.participant.state

import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.digitalasset.kvbc.daml_commit.{CommitRequest, CommitResponse}
import com.google.protobuf.ByteString
import io.grpc.{Status, StatusRuntimeException}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class ConcordLedgerWriterSpec extends AsyncWordSpec with Matchers with MockitoSugar {

  private sealed trait CommitTransaction {
    def commit(request: CommitRequest): Future[CommitResponse]
  }

  private val anEnvelope = ByteString.copyFrom(Array[Byte](0, 1, 2))
  private val aParticipantId = ParticipantId.assertFromString("aParticipantId")
  private val aLedgerId = "aLedgerId"

  "ledger writer" should {
    "wrap parameters and call commitTransaction" in {
      val commitFunction = mock[CommitTransaction]
      val requestCaptor =
        ArgumentCaptor.forClass[CommitRequest, CommitRequest](classOf[CommitRequest])
      when(commitFunction.commit(requestCaptor.capture()))
        .thenReturn(Future.successful[CommitResponse](
          CommitResponse().withStatus(CommitResponse.CommitStatus.OK)))
      val instance = new ConcordLedgerWriter(aLedgerId, aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope).map { actual =>
        actual shouldBe SubmissionResult.Acknowledged
        val actualRequest = requestCaptor.getValue
        actualRequest.submission shouldEqual anEnvelope
        actualRequest.participantId shouldBe aParticipantId
        actualRequest.correlationId shouldBe "aCorrelationId"
      }
    }

    "return InternalError in case of an ERROR commit response" in {
      val commitFunction = mock[CommitTransaction]
      when(commitFunction.commit(any()))
        .thenReturn(Future.successful[CommitResponse](
          CommitResponse().withStatus(CommitResponse.CommitStatus.ERROR)))
      val instance = new ConcordLedgerWriter(aLedgerId, aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope).map {
        case SubmissionResult.InternalError(reason) =>
          reason should include("ERROR")
        case _ =>
          fail
      }
    }

    "return InternalError in case of exception" in {
      val commitFunction = mock[CommitTransaction]
      val expectedException =
        new IllegalArgumentException("Something went wrong")
      when(commitFunction.commit(any()))
        .thenReturn(Future.failed[CommitResponse](expectedException))
      val instance = new ConcordLedgerWriter(aLedgerId, aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope).map {
        case SubmissionResult.InternalError(reason) =>
          reason should include("Something went wrong")
        case _ =>
          fail
      }
    }

    "return Overloaded in case of resource exhaustion" in {
      val commitFunction = mock[CommitTransaction]
      val expectedException =
        new StatusRuntimeException(Status.RESOURCE_EXHAUSTED)
      when(commitFunction.commit(any()))
        .thenReturn(Future.failed[CommitResponse](expectedException))
      val instance = new ConcordLedgerWriter(aLedgerId, aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope).map { actual =>
        actual shouldBe SubmissionResult.Overloaded
      }
    }

    "return InternalError in case of a grpc error other than resource exhaustion" in {
      val commitFunction = mock[CommitTransaction]
      val expectedException = new StatusRuntimeException(Status.ABORTED)
      when(commitFunction.commit(any()))
        .thenReturn(Future.failed[CommitResponse](expectedException))
      val instance = new ConcordLedgerWriter(aLedgerId, aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope).map {
        case SubmissionResult.InternalError(_) =>
          succeed
        case _ =>
          fail
      }
    }
  }
}
