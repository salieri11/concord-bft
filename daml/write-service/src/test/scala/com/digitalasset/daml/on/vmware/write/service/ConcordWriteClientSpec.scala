// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import com.daml.ledger.participant.state.v1.SubmissionResult.Acknowledged
import com.daml.ledger.participant.state.v1.{ParticipantId, SubmissionResult}
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient.MessageFlags
import com.digitalasset.kvbc.daml_commit.CommitRequest
import com.google.protobuf.ByteString
import org.mockito.ArgumentCaptor
import org.mockito.Mockito.when
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class ConcordWriteClientSpec extends AsyncWordSpec with Matchers with MockitoSugar {

  "markRequestForPreExecution" should {
    "set pre-execution flag" in {
      val mockDelegate = mock[CommitTransaction]
      val requestArgumentCaptor =
        ArgumentCaptor.forClass[CommitRequest, CommitRequest](classOf[CommitRequest])
      when(mockDelegate.commitTransaction(requestArgumentCaptor.capture()))
        .thenReturn(Future.successful(Acknowledged))
      val commitRequest = CommitRequest(anEnvelope, aParticipantId, aCorrelationId)

      val instance = ConcordWriteClient.markRequestForPreExecution(mockDelegate.commitTransaction) _

      instance(commitRequest).map { actual =>
        actual shouldBe Acknowledged
        requestArgumentCaptor.getAllValues should have size 1
        val expectedCommitRequest = CommitRequest(
          submission = commitRequest.submission,
          participantId = commitRequest.participantId,
          correlationId = commitRequest.correlationId,
          flags = MessageFlags.PreExecuteFlag
        )
        requestArgumentCaptor.getValue shouldBe expectedCommitRequest
        succeed
      }
    }

    "do not alter pre-execution flag if already set" in {
      val mockDelegate = mock[CommitTransaction]
      val requestArgumentCaptor =
        ArgumentCaptor.forClass[CommitRequest, CommitRequest](classOf[CommitRequest])
      when(mockDelegate.commitTransaction(requestArgumentCaptor.capture()))
        .thenReturn(Future.successful(Acknowledged))
      val commitRequest = CommitRequest(
        anEnvelope,
        aParticipantId,
        aCorrelationId,
        flags = MessageFlags.PreExecuteFlag)

      val instance = ConcordWriteClient.markRequestForPreExecution(mockDelegate.commitTransaction) _

      instance(commitRequest).map { _ =>
        requestArgumentCaptor.getAllValues should have size 1
        requestArgumentCaptor.getValue.flags shouldBe MessageFlags.PreExecuteFlag
        succeed
      }
    }
  }

  private val anEnvelope = ByteString.copyFromUtf8("some request")
  private val aParticipantId = ParticipantId.assertFromString("aParticipantId")
  private val aCorrelationId = "correlation ID"

  private trait CommitTransaction {
    def commitTransaction(request: CommitRequest): Future[SubmissionResult]
  }
}
