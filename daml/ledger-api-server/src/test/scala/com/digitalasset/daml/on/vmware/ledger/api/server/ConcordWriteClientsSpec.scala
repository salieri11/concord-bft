// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.Path

import com.codahale.metrics.MetricRegistry
import com.daml.dec.DirectExecutionContext
import com.daml.ledger.participant.state.kvutils.api.{CommitMetadata, SimpleCommitMetadata}
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.ledger.api.server.ConcordWriteClients._
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient.MessageFlags
import com.digitalasset.daml.on.vmware.write.service.bft.{
  BftWriteClient,
  RequestTimeoutStrategy,
  RequestTimeoutFunction
}
import com.digitalasset.daml.on.vmware.write.service.kvbc.KvbcWriteClient
import com.digitalasset.kvbc.daml_commit.CommitRequest
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{atLeastOnce, times, verify, when}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ConcordWriteClientsSpec extends WordSpec with Matchers with MockitoSugar {
  "waitForConcordWriteClientsToBeReady()" should {

    "wait for clientsToBeWaitedFor clients to be ready" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.ready).thenReturn(false, true)
      waitForConcordWriteClientsToBeReady(
        Seq(writeClient),
        clientsToBeWaitedFor = 1,
        sleepMillis = 1)
      verify(writeClient, times(2)).ready
    }

    "throw a RuntimeException if the number of attempts is exceeded" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.ready).thenReturn(false)
      a[RuntimeException] should be thrownBy waitForConcordWriteClientsToBeReady(
        Seq(writeClient),
        clientsToBeWaitedFor = 1,
        sleepMillis = 1,
        attempts = 1)
      verify(writeClient, times(1)).ready
    }

    "count ready clients once per ready count" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.ready).thenReturn(false, false, true, false, true, false, true, true)
      waitForConcordWriteClientsToBeReady(
        Seq(writeClient, writeClient),
        writeClientLabel = "secondary",
        clientsToBeWaitedFor = 2,
        sleepMillis = 1)
      verify(writeClient, times(8)).ready
    }
  }

  "createConcordWriteClients()" should {
    "create only a single BFT client when BFT client is enabled" in {
      val bftWriteClientFactory =
        mock[(Option[Path], RequestTimeoutFunction, Metrics) => BftWriteClient]
      when(bftWriteClientFactory(any[Option[Path]], any[RequestTimeoutFunction](), any[Metrics]))
        .thenReturn(mock[BftWriteClient])
      val kvbcWriteClientFactory =
        mock[String => KvbcWriteClient]
      when(kvbcWriteClientFactory(any[String])).thenReturn(mock[KvbcWriteClient])

      createConcordWriteClients(
        ExtraConfig.ReasonableDefault.copy(
          bftClient = BftClientConfig.ReasonableDefault.copy(enable = true)),
        bftWriteClientFactory,
        mock[RequestTimeoutStrategy],
        kvbcWriteClientFactory,
        aMetrics
      )

      verify(bftWriteClientFactory, times(1))(
        any[Option[Path]],
        any[RequestTimeoutFunction](),
        any[Metrics])
      verify(kvbcWriteClientFactory, times(0))(any[String])
    }

    "create a BFT client using a default request timeout if requests are not pre-executing" in {
      val bftWriteClientFactory =
        mock[(Option[Path], RequestTimeoutFunction, Metrics) => BftWriteClient]
      val requestTimeoutFunctionCaptor =
        ArgumentCaptor
          .forClass(classOf[RequestTimeoutFunction])
          .asInstanceOf[ArgumentCaptor[RequestTimeoutFunction]]
      when(
        bftWriteClientFactory(
          any[Option[Path]],
          requestTimeoutFunctionCaptor.capture(),
          any[Metrics]))
        .thenReturn(mock[BftWriteClient])
      val kvbcWriteClientFactory =
        mock[String => KvbcWriteClient]
      val requestTimeoutStrategy = mock[RequestTimeoutStrategy]

      createConcordWriteClients(
        ExtraConfig.ReasonableDefault.copy(
          bftClient = BftClientConfig.ReasonableDefault.copy(enable = true)),
        bftWriteClientFactory,
        requestTimeoutStrategy,
        kvbcWriteClientFactory,
        aMetrics
      )
      requestTimeoutFunctionCaptor.getValue.apply(aNonPreExecutingCommitRequest, aCommitMetadata)

      verify(requestTimeoutStrategy, times(1)).defaultTimeout
      verify(requestTimeoutStrategy, times(0)).calculate(any[CommitMetadata])
    }

    "create a BFT client that computes the request timeout if pre-executing" in {
      val bftWriteClientFactory =
        mock[(Option[Path], RequestTimeoutFunction, Metrics) => BftWriteClient]
      val requestTimeoutFunctionCaptor =
        ArgumentCaptor
          .forClass(classOf[RequestTimeoutFunction])
          .asInstanceOf[ArgumentCaptor[RequestTimeoutFunction]]
      when(
        bftWriteClientFactory(
          any[Option[Path]],
          requestTimeoutFunctionCaptor.capture(),
          any[Metrics]))
        .thenReturn(mock[BftWriteClient])
      val kvbcWriteClientFactory =
        mock[String => KvbcWriteClient]
      val requestTimeoutStrategy = mock[RequestTimeoutStrategy]
      when(requestTimeoutStrategy.calculate(any[CommitMetadata]))
        .thenReturn(1.millis)

      createConcordWriteClients(
        ExtraConfig.ReasonableDefault.copy(
          bftClient = BftClientConfig.ReasonableDefault.copy(enable = true)),
        bftWriteClientFactory,
        requestTimeoutStrategy,
        kvbcWriteClientFactory,
        aMetrics
      )
      requestTimeoutFunctionCaptor.getValue()(aPreExecutingCommitRequest, aCommitMetadata)

      verify(requestTimeoutStrategy, times(0)).defaultTimeout
      verify(requestTimeoutStrategy, times(1)).calculate(any[CommitMetadata])
    }

    "create only KVBC clients when BFT client is not enabled" in {
      val bftWriteClientFactory =
        mock[(Option[Path], RequestTimeoutFunction, Metrics) => BftWriteClient]
      when(bftWriteClientFactory(any[Option[Path]], any[RequestTimeoutFunction](), any[Metrics]))
        .thenReturn(mock[BftWriteClient])
      val kvbcWriteClientFactory =
        mock[String => KvbcWriteClient]
      when(kvbcWriteClientFactory(any[String])).thenReturn(mock[KvbcWriteClient])

      createConcordWriteClients(
        ExtraConfig.ReasonableDefault.copy(
          bftClient = BftClientConfig.ReasonableDefault.copy(enable = false)),
        bftWriteClientFactory,
        mock[RequestTimeoutStrategy],
        kvbcWriteClientFactory,
        aMetrics
      )

      verify(bftWriteClientFactory, times(0))(
        any[Option[Path]],
        any[RequestTimeoutFunction](),
        any[Metrics])
      verify(kvbcWriteClientFactory, atLeastOnce())(any[String])
    }
  }

  private implicit val anExecutionContext: ExecutionContext = DirectExecutionContext
  private val aMetrics = new Metrics(new MetricRegistry)
  private val aNonPreExecutingCommitRequest = CommitRequest()
  private val aPreExecutingCommitRequest = CommitRequest(flags = MessageFlags.PreExecuteFlag)
  private val aCommitMetadata = SimpleCommitMetadata(Some(1))
}
