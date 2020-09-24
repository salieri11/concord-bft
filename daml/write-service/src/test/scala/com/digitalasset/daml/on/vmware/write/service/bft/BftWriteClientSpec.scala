// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.daml.ledger.participant.state.kvutils.api.SimpleCommitMetadata
import com.daml.ledger.participant.state.v1.SubmissionResult.Acknowledged
import com.digitalasset.daml.on.vmware.write.service.CommitRequest
import com.google.protobuf.ByteString
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class BftWriteClientSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  "commitTransaction" should {
    "pass the preExecute flag to sendRequest if it is set in the commit request" in {
      val mockConcordClientPool = mock[BftConcordClientPool]
      when(
        mockConcordClientPool
          .sendRequest(any[ByteString], any[Duration], ArgumentMatchers.eq(true), any[String])(
            any[ExecutionContext]))
        .thenReturn(Future.successful(Acknowledged))
      val instance = new BftWriteClient(mockConcordClientPool, (_, _) => Duration.Inf)

      instance
        .commitTransaction(
          CommitRequest.createEmpty().copy(preExecute = true),
          SimpleCommitMetadata(None))(executionContext)
        .map { actual =>
          actual shouldBe Acknowledged
        }
      succeed
    }

    "pass the request timeout to sendRequest by calling the request timeout function" in {
      val mockConcordClientPool = mock[BftConcordClientPool]
      when(
        mockConcordClientPool
          .sendRequest(any[ByteString], ArgumentMatchers.eq(1.millis), any[Boolean], any[String])(
            any[ExecutionContext]))
        .thenReturn(Future.successful(Acknowledged))
      val instance = new BftWriteClient(mockConcordClientPool, (_, _) => 1.millis)

      instance
        .commitTransaction(CommitRequest.createEmpty(), SimpleCommitMetadata(None))(
          executionContext)
        .map { actual =>
          actual shouldBe Acknowledged
        }
      succeed
    }
  }
}
