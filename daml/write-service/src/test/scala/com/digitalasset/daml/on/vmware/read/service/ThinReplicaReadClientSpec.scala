package com.digitalasset.daml.on.vmware.read.service

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}

import com.codahale.metrics.MetricRegistry

import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.digitalasset.daml.on.vmware.thin.replica.client.core.{ThinReplicaClientJni, Update}

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

    "return a stream of updates from a Thin Replica that fails only the first subscribe" in {
      testBlockDelivery(createThinReplicaReadClientFailingFirstSubscribe)
    }

    "return a stream of updates from a Thin Replica that fails only the first pop" in {
      testBlockDelivery(createThinReplicaReadClientFailingFirstPop)
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
    val thinReplicaReadClientCore = mock[ThinReplicaClientJni]
    when(thinReplicaReadClientCore.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaReadClientCore.subscribe(any()))
      .thenReturn(true)
    when(thinReplicaReadClientCore.pop())
      .thenReturn(anUpdate)
    createThinReplicaClient(thinReplicaReadClientCore)
  }

  private def createThinReplicaReadClientFailingToInitialize: ThinReplicaReadClient = {
    val thinReplicaReadClientCore = mock[ThinReplicaClientJni]
    when(thinReplicaReadClientCore.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(false)
    createThinReplicaClient(thinReplicaReadClientCore)
  }

  private def createThinReplicaReadClientFailingFirstSubscribe: ThinReplicaReadClient = {
    val thinReplicaReadClientCore = mock[ThinReplicaClientJni]
    when(thinReplicaReadClientCore.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaReadClientCore.subscribe(any()))
      .thenReturn(false)
      .thenReturn(true)
    when(thinReplicaReadClientCore.pop())
      .thenReturn(anUpdate)
    createThinReplicaClient(thinReplicaReadClientCore)
  }

  private def createThinReplicaReadClientFailingFirstPop: ThinReplicaReadClient = {
    val thinReplicaReadClientCore = mock[ThinReplicaClientJni]
    when(thinReplicaReadClientCore.initialize(any(), any(), any(), any(), any(), any(), any()))
      .thenReturn(true)
    when(thinReplicaReadClientCore.subscribe(any()))
      .thenReturn(true)
    when(thinReplicaReadClientCore.pop())
      .thenReturn(None)
      .thenReturn(anUpdate)
    createThinReplicaClient(thinReplicaReadClientCore)
  }

  private def createThinReplicaClient(thinReplicaReadClientCore: ThinReplicaClientJni) = {
    val metrics = new MetricRegistry
    new ThinReplicaReadClient(
      clientId = "doesntmatter",
      maxFaulty = 0,
      privateKey = "doesntmatter",
      servers = Array(),
      maxReadDataTimeout = 0,
      maxReadHashTimeout = 0,
      jaegerAgent = "doesntmatter",
      trcCore = thinReplicaReadClientCore,
      metricRegistry = metrics
    )
  }

  private val anUpdate = Some(
    Update(
      17,
      Array("DummmyKey".getBytes -> "DummyValue".getBytes),
      "DummyCorrelationId",
      "DummySpanContext".getBytes))
  private type Block = ThinReplicaReadClient.Block

  private def takeBlocks(source: Source[Block, NotUsed], n: Int): Future[Seq[Block]] =
    source.take(n).runWith(Sink.seq)

}
