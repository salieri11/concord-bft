// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.participant.state

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}
import com.daml.ledger.participant.state.kvutils.OffsetBuilder
import com.digitalasset.daml.on.vmware.thin.replica.client.core.Update
import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.google.protobuf.ByteString
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class ConcordKeyValueLedgerReaderSpec
    extends AsyncWordSpec
    with Matchers
    with MockitoSugar
    with AkkaBeforeAndAfterAll {

  private sealed trait BlockSource {
    def streamFrom(offset: Long): Source[Update, NotUsed]
  }

  private val aSpanContext = Array[Byte](1, 2, 3)

  "ledger reader" should {
    "wrap thin-replica block" in {
      val blockSource = mock[BlockSource]
      val expectedKeyValuePairs =
        Seq((Array[Byte](0, 1), Array[Byte](2, 3)), (Array[Byte](0, 1, 2), Array[Byte](4, 5, 6)))
      when(blockSource.streamFrom(any()))
        .thenReturn(
          Source.single(Update(123, expectedKeyValuePairs.toArray, "aCorrelationId", aSpanContext)))
      val instance =
        new ConcordKeyValueLedgerReader(blockSource.streamFrom, "aLedgerId")
      val stream = instance.events(Some(OffsetBuilder.fromLong(123)))
      stream.runWith(Sink.seq).map { actual =>
        actual should have size 1
        val actualBlock = actual.head
        OffsetBuilder.highestIndex(actualBlock.offset) shouldBe 123
        actualBlock.keyValuePairs should have size expectedKeyValuePairs.size
        for ((expectedKeyValuePair, actualKeyValuePair) <- expectedKeyValuePairs
            .zip(actualBlock.keyValuePairs)) {
          ByteString
            .copyFrom(expectedKeyValuePair._1) shouldEqual actualKeyValuePair._1
          ByteString
            .copyFrom(expectedKeyValuePair._2) shouldEqual actualKeyValuePair._2
        }
        succeed
      }
    }

    "stream from StartIndex in case no offset is specified" in {
      val blockSource = mock[BlockSource]
      val expectedKeyValuePairs =
        Seq((Array[Byte](0, 1), Array[Byte](2, 3)))
      when(blockSource.streamFrom(ConcordKeyValueLedgerReader.StartIndex))
        .thenReturn(
          Source.single(Update(0, expectedKeyValuePairs.toArray, "aCorrelationId", aSpanContext)))
      val instance =
        new ConcordKeyValueLedgerReader(blockSource.streamFrom, "aLedgerId")
      val stream = instance.events(None)
      stream.runWith(Sink.seq).map { actual =>
        actual should have size 1
      }
    }

    "skip over empty blocks" in {
      val blockSource = mock[BlockSource]
      val nonEmptyKeyValuePairs =
        Seq((Array[Byte](0, 1), Array[Byte](2, 3))).toArray
      def createUpdate(i: Long, pairs: Array[(Array[Byte], Array[Byte])]) =
        Update(i, pairs, s"$i", s"$i".getBytes)
      val updatesWithEmpty = List(
        createUpdate(0, nonEmptyKeyValuePairs),
        createUpdate(1, Array()),
        createUpdate(2, nonEmptyKeyValuePairs)
      )
      when(blockSource.streamFrom(ConcordKeyValueLedgerReader.StartIndex))
        .thenReturn(Source(updatesWithEmpty))
      val instance =
        new ConcordKeyValueLedgerReader(blockSource.streamFrom, "aLedgerId")
      val stream = instance.events(None)
      stream.runWith(Sink.seq).map { actual =>
        actual should have size 2
      }
    }
  }
}
