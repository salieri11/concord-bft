// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.read.service

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}

import collection.JavaConverters._

import com.codahale.metrics.MetricRegistry

import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.digitalasset.daml.on.vmware.thin.replica.client.core.{ThinReplicaClientJni, Update}

import org.mockito.AdditionalAnswers
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.{Assertion, AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class ThinReplicaReadClientSpec
    extends AsyncWordSpec
    with AkkaBeforeAndAfterAll
    with Matchers
    with MockitoSugar {

  "ThinReplicaReadClient" should {
    "return a stream of updates from a perfect Thin Replica" in {
      testBlockDelivery(createPerfectThinReplicaReadClient)
    }

    "return an empty stream of updates from a Thin Replica that fails to initialize" in {
      val thinReplicaClient = createThinReplicaReadClientFailingToInitialize
      thinReplicaClient.committedBlocks(0) shouldBe Source.empty
    }

    "return a stream of updates from a Thin Replica that fails the first subscribe" in {
      testBlockDelivery(createThinReplicaReadClientFailingFirstSubscribe)
    }

    "return a stream of updates from a Thin Replica that fails the first pop" in {
      testBlockDelivery(createThinReplicaReadClientFailingFirstPop)
    }

    "return a stream of updates from a Thin Replica that fails the second pop" in {
      testBlockDelivery(createThinReplicaReadClientFailingSecondPop)
    }

    "return a stream of distinctive updates from a Thin Replica" in {
      val numUpdates = 10
      val originalUpdates = (1 to numUpdates).map(genUpdate(_))
      val thinReplicaClient = createThinReplicaReadClientGiviningManyUpdates(originalUpdates.toList)
      takeBlocks(thinReplicaClient.committedBlocks(0), numUpdates).map { returnedUpdates =>
        returnedUpdates.size shouldBe numUpdates
        returnedUpdates should equal(originalUpdates.flatten)
      }
    }

    "update metrics" in {
      val metrics = new ThinReplicaReadClientMetrics(new MetricRegistry)
      val thinReplicaReadClient = createThinReplicaReadClientGivenMetrics(
        createPerfectMockOfThinReplicaClient,
        metrics
      )
      testBlockDelivery(thinReplicaReadClient).map(_ => {
        val actualTimingValues = metrics.getBlockTimer.getSnapshot.getValues
        actualTimingValues.size should be >= 10
        actualTimingValues.head should be > 1L
        metrics.lastBlockId.getValue shouldBe anUpdate.get.blockId
      })
    }
  }

  private def testBlockDelivery(thinReplicaClient: ThinReplicaReadClient): Future[Assertion] = {
    val source = thinReplicaClient.committedBlocks(0)
    val n = 10
    takeBlocks(source, n).map { blocks =>
      blocks.size shouldBe n
      blocks should contain only anUpdate.get
    }
  }

  private def createPerfectThinReplicaReadClient: ThinReplicaReadClient = {
    createThinReplicaReadClient(createPerfectMockOfThinReplicaClient)
  }

  private def createPerfectMockOfThinReplicaClient: ThinReplicaClientJni = {
    val thinReplicaClientJni = mock[ThinReplicaClientJni]
    when(thinReplicaClientJni.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaClientJni.subscribe(any()))
      .thenReturn(true)
    when(thinReplicaClientJni.pop())
      .thenReturn(anUpdate)
    thinReplicaClientJni
  }

  private def createThinReplicaReadClientFailingToInitialize: ThinReplicaReadClient = {
    val thinReplicaClientJni = mock[ThinReplicaClientJni]
    when(thinReplicaClientJni.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(false)
    createThinReplicaReadClient(thinReplicaClientJni)
  }

  private def createThinReplicaReadClientFailingFirstSubscribe: ThinReplicaReadClient = {
    val thinReplicaClientJni = mock[ThinReplicaClientJni]
    when(thinReplicaClientJni.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaClientJni.subscribe(any()))
      .thenReturn(false, true)
    when(thinReplicaClientJni.pop())
      .thenReturn(anUpdate)
    createThinReplicaReadClient(thinReplicaClientJni)
  }

  private def createThinReplicaReadClientFailingFirstPop: ThinReplicaReadClient = {
    val thinReplicaClientJni = mock[ThinReplicaClientJni]
    when(thinReplicaClientJni.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaClientJni.subscribe(any()))
      .thenReturn(true)
    when(thinReplicaClientJni.pop())
      .thenReturn(None, anUpdate)
    createThinReplicaReadClient(thinReplicaClientJni)
  }

  private def createThinReplicaReadClientFailingSecondPop: ThinReplicaReadClient = {
    val thinReplicaClientJni = mock[ThinReplicaClientJni]
    when(thinReplicaClientJni.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaClientJni.subscribe(any()))
      .thenReturn(true)
    when(thinReplicaClientJni.pop())
      .thenReturn(anUpdate, None, anUpdate)
    createThinReplicaReadClient(thinReplicaClientJni)
  }

  private def createThinReplicaReadClientGiviningManyUpdates(
      updateRange: List[Option[Update]]): ThinReplicaReadClient = {
    val thinReplicaClientJni = mock[ThinReplicaClientJni]
    when(thinReplicaClientJni.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaClientJni.subscribe(any()))
      .thenReturn(true)
    when(thinReplicaClientJni.pop())
      .thenAnswer(AdditionalAnswers.returnsElementsOf(updateRange.asJava))
    createThinReplicaReadClient(thinReplicaClientJni)
  }

  private def createThinReplicaReadClient(thinReplicaClientJni: ThinReplicaClientJni) = {
    val metricsRegistry = new MetricRegistry
    val metrics = new ThinReplicaReadClientMetrics(metricsRegistry)
    createThinReplicaReadClientGivenMetrics(
      thinReplicaClientJni,
      metrics
    )
  }

  private def createThinReplicaReadClientGivenMetrics(
      thinReplicaClientJni: ThinReplicaClientJni,
      metrics: ThinReplicaReadClientMetrics) =
    new ThinReplicaReadClient(
      clientId = "doesntmatter",
      maxFaulty = 0,
      privateKey = "doesntmatter",
      servers = Array(),
      maxReadDataTimeout = 0,
      maxReadHashTimeout = 0,
      jaegerAgent = "doesntmatter",
      trcCore = thinReplicaClientJni,
      metrics = metrics
    )

  private val anUpdate = Some(
    Update(
      17,
      Array("DummmyKey".getBytes -> "DummyValue".getBytes),
      "DummyCorrelationId",
      "DummySpanContext".getBytes))

  private def genUpdate(i: Int) =
    Some(
      Update(
        i,
        Array(s"DummmyKey$i".getBytes -> s"DummyValue$i".getBytes),
        s"DummyCorrelationId$i",
        s"DummySpanContext$i".getBytes))

  private type Block = ThinReplicaReadClient.Block

  private def takeBlocks(source: Source[Block, NotUsed], n: Int): Future[Seq[Block]] =
    source.take(n).runWith(Sink.seq)

}
