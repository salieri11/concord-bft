// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.Path

import com.codahale.metrics.MetricRegistry
import com.daml.dec.DirectExecutionContext
import com.daml.ledger.api.health.{Healthy, Unhealthy}
import com.daml.ledger.participant.state.kvutils.api.{CommitMetadata, SimpleCommitMetadata}
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.ledger.api.server.ConcordWriteClients._
import com.digitalasset.daml.on.vmware.write.service.{CommitRequest, ConcordWriteClient}
import com.digitalasset.daml.on.vmware.write.service.bft.{
  BftWriteClient,
  RequestTimeoutFunction,
  RequestTimeoutStrategy,
  RetryStrategyFactory
}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify, when}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ConcordWriteClientsSpec extends WordSpec with Matchers with MockitoSugar {
  "waitForConcordWriteClientsToBeReady()" should {

    "wait for clientsToBeWaitedFor clients to be ready" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.currentHealth).thenReturn(Unhealthy, Healthy)
      waitForConcordWriteClientsToBeReady(
        Seq(writeClient),
        clientsToBeWaitedFor = 1,
        sleepMillis = 1)
      verify(writeClient, times(2)).currentHealth
    }

    "throw a RuntimeException if the number of attempts is exceeded" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.currentHealth).thenReturn(Unhealthy)
      a[RuntimeException] should be thrownBy waitForConcordWriteClientsToBeReady(
        Seq(writeClient),
        clientsToBeWaitedFor = 1,
        sleepMillis = 1,
        attempts = 1)
      verify(writeClient, times(1)).currentHealth
    }

    "count ready clients once per ready count" in {
      val writeClient = mock[ConcordWriteClient]
      when(writeClient.currentHealth).thenReturn(
        Unhealthy,
        Unhealthy,
        Healthy,
        Unhealthy,
        Healthy,
        Unhealthy,
        Healthy,
        Healthy)
      waitForConcordWriteClientsToBeReady(
        Seq(writeClient, writeClient),
        writeClientLabel = "secondary",
        clientsToBeWaitedFor = 2,
        sleepMillis = 1)
      verify(writeClient, times(8)).currentHealth
    }
  }

  "createConcordWriteClient()" should {
    "create a BFT client using a default request timeout if requests are not pre-executing" in {
      val requestTimeoutFunctionCaptor =
        ArgumentCaptor
          .forClass(classOf[RequestTimeoutFunction])
          .asInstanceOf[ArgumentCaptor[RequestTimeoutFunction]]
      val bftWriteClientFactory =
        newBftWriteClientFactoryMock(requestTimeoutFunctionCaptor.capture)
      val requestTimeoutStrategy = mock[RequestTimeoutStrategy]

      createConcordWriteClient(
        ExtraConfig.ReasonableDefault,
        bftWriteClientFactory,
        requestTimeoutStrategy,
        mock[RetryStrategyFactory],
        aMetrics
      )
      requestTimeoutFunctionCaptor.getValue.apply(aNonPreExecutingCommitRequest, aCommitMetadata)

      verify(requestTimeoutStrategy, times(1)).defaultTimeout
      verify(requestTimeoutStrategy, times(0)).calculate(any[CommitMetadata])
    }

    "create a BFT client that computes the request timeout if pre-executing" in {
      val requestTimeoutFunctionCaptor =
        ArgumentCaptor
          .forClass(classOf[RequestTimeoutFunction])
          .asInstanceOf[ArgumentCaptor[RequestTimeoutFunction]]
      val bftWriteClientFactory =
        newBftWriteClientFactoryMock(requestTimeoutFunctionCaptor.capture)
      val requestTimeoutStrategy = mock[RequestTimeoutStrategy]
      when(requestTimeoutStrategy.calculate(any[CommitMetadata]))
        .thenReturn(1.millis)

      createConcordWriteClient(
        ExtraConfig.ReasonableDefault,
        bftWriteClientFactory,
        requestTimeoutStrategy,
        mock[RetryStrategyFactory],
        aMetrics,
      )
      requestTimeoutFunctionCaptor.getValue()(aPreExecutingCommitRequest, aCommitMetadata)

      verify(requestTimeoutStrategy, times(0)).defaultTimeout
      verify(requestTimeoutStrategy, times(1)).calculate(any[CommitMetadata])
    }
  }

  private def newBftWriteClientFactoryMock(
      requestTimeoutFunctionArgumentFactory: () => RequestTimeoutFunction = () =>
        any[RequestTimeoutFunction](),
  ): (Path, RequestTimeoutFunction, RetryStrategyFactory, Metrics) => BftWriteClient = {
    val bftWriteClientFactoryMock =
      mock[(Path, RequestTimeoutFunction, RetryStrategyFactory, Metrics) => BftWriteClient]
    when(
      bftWriteClientFactoryMock(
        any[Path],
        requestTimeoutFunctionArgumentFactory(),
        any[RetryStrategyFactory](),
        any[Metrics]))
      .thenReturn(mock[BftWriteClient])
    bftWriteClientFactoryMock
  }

  private implicit val AnExecutionContext: ExecutionContext = DirectExecutionContext
  private def aMetrics = new Metrics(new MetricRegistry)
  private def aNonPreExecutingCommitRequest = CommitRequest.createEmpty()
  private def aPreExecutingCommitRequest = CommitRequest.createEmpty().copy(preExecute = true)
  private def aCommitMetadata = SimpleCommitMetadata(Some(1))
}
