// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.participant.state

import com.daml.ledger.participant.state.kvutils.api.{CommitMetadata, SimpleCommitMetadata}
import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.digitalasset.daml.on.vmware.testing.MockitoScala
import com.digitalasset.daml.on.vmware.write.service.CommitRequest
import com.google.protobuf.ByteString
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future

class ConcordLedgerWriterSpec extends AsyncWordSpec with Matchers with MockitoScala {

  private sealed trait CommitTransaction {
    def commit(request: CommitRequest, metadata: CommitMetadata): Future[SubmissionResult]
  }

  private val anEnvelope = ByteString.copyFrom(Array[Byte](0, 1, 2))
  private val aParticipantId = ParticipantId.assertFromString("aParticipantId")

  "ledger writer" should {
    "wrap parameters and call commitTransaction" in {
      val commitFunction = mock[CommitTransaction]
      val requestCaptor = captor[CommitRequest]
      val metadataCaptor = captor[CommitMetadata]
      when(commitFunction.commit(requestCaptor.capture(), metadataCaptor.capture()))
        .thenReturn(Future.successful(SubmissionResult.Acknowledged))
      val instance = new ConcordLedgerWriter(aParticipantId, commitFunction.commit)
      val metadata = SimpleCommitMetadata(estimatedInterpretationCost = Some(60))
      instance.commit("aCorrelationId", anEnvelope, metadata).map { actual =>
        actual shouldBe SubmissionResult.Acknowledged

        val actualRequest = requestCaptor.getValue
        actualRequest.submission shouldEqual anEnvelope
        actualRequest.participantId shouldBe aParticipantId
        actualRequest.correlationId shouldBe "aCorrelationId"

        val actualMetadata = metadataCaptor.getValue
        actualMetadata shouldBe metadata
      }
    }

    "return Overloaded in case of resource exhaustion" in {
      val commitFunction = mock[CommitTransaction]
      when(commitFunction.commit(any(), any()))
        .thenReturn(Future.successful(SubmissionResult.Overloaded))
      val instance =
        new ConcordLedgerWriter(aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope, CommitMetadata.Empty).map { actual =>
        actual shouldBe SubmissionResult.Overloaded
      }
    }

    "return InternalError in case of an ERROR commit response" in {
      val commitFunction = mock[CommitTransaction]
      when(commitFunction.commit(any(), any()))
        .thenReturn(Future.successful(SubmissionResult.InternalError("ERROR")))
      val instance =
        new ConcordLedgerWriter(aParticipantId, commitFunction.commit)
      instance.commit("aCorrelationId", anEnvelope, CommitMetadata.Empty).map {
        case SubmissionResult.InternalError(reason) =>
          reason should include("ERROR")
        case _ =>
          fail
      }
    }

    "throw in case of unsupported submission" in {
      val commitFunction = mock[CommitTransaction]
      when(commitFunction.commit(any(), any()))
        .thenReturn(Future.successful[SubmissionResult](SubmissionResult.NotSupported))
      val instance =
        new ConcordLedgerWriter(aParticipantId, commitFunction.commit)
      instance
        .commit("aCorrelationId", anEnvelope, CommitMetadata.Empty)
        .failed
        .map { exception: Throwable =>
          exception shouldBe an[IllegalStateException]
          exception.getMessage should include("Unsupported submission")
        }
    }

    "throw in case of an exception thrown by the commit function" in {
      val commitFunction = mock[CommitTransaction]
      val expectedException =
        new IllegalArgumentException("Something went wrong")
      when(commitFunction.commit(any(), any()))
        .thenReturn(Future.failed(expectedException))
      val instance =
        new ConcordLedgerWriter(aParticipantId, commitFunction.commit)
      instance
        .commit("aCorrelationId", anEnvelope, CommitMetadata.Empty)
        .failed
        .map { exception: Throwable =>
          exception shouldBe an[IllegalStateException]
          exception.getMessage should include("Something went wrong")
        }

    }
  }
}
