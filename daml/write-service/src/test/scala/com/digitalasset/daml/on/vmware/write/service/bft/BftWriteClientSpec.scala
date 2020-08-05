// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.daml.ledger.participant.state.v1.SubmissionResult.Acknowledged
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient.MessageFlags
import com.digitalasset.kvbc.daml_commit.CommitRequest
import com.google.protobuf.ByteString
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

class BftWriteClientSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  "commitTransaction" should {
    "set preExecute flag if it is set in the commit request" in {
      val mockConcordClientPool = mock[BftConcordClientPool]
      when(
        mockConcordClientPool
          .sendRequest(any[ByteString], any[Duration], ArgumentMatchers.eq(true), any[String])(
            any[ExecutionContext]))
        .thenReturn(Future.successful(Acknowledged))
      val instance = new BftWriteClient(mockConcordClientPool, Duration.Inf)

      instance
        .commitTransaction(CommitRequest(flags = MessageFlags.PreExecuteFlag))(executionContext)
        .map { actual =>
          actual shouldBe Acknowledged
        }
      succeed
    }
  }
}
