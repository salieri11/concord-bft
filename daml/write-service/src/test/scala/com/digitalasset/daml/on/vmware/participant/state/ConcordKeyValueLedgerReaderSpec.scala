package com.digitalasset.daml.on.vmware.participant.state

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}
import com.daml.ledger.participant.state.kvutils.KVOffset
import com.digitalasset.daml.on.vmware.thin.replica.client.core.Update
import com.digitalasset.ledger.api.testing.utils.AkkaBeforeAndAfterAll
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
        Seq((Array[Byte](0, 1), Array[Byte](2, 3)),
            (Array[Byte](0, 1, 2), Array[Byte](4, 5, 6)))
      when(blockSource.streamFrom(any()))
        .thenReturn(Source.single(
          Update(123, expectedKeyValuePairs.toArray, "aCorrelationId", aSpanContext)))
      val instance =
        new ConcordKeyValueLedgerReader(blockSource.streamFrom, "aLedgerId")
      val stream = instance.events(Some(KVOffset.fromLong(123)))
      stream.runWith(Sink.seq).map { actual =>
        actual should have size 1
        val actualBlock = actual.head
        KVOffset.highestIndex(actualBlock.offset) shouldBe 123
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
      when(blockSource.streamFrom(ConcordKeyValueLedgerReader.StartIndex))
        .thenReturn(Source.single(Update(0, Array(), "aCorrelationId", aSpanContext)))
      val instance =
        new ConcordKeyValueLedgerReader(blockSource.streamFrom, "aLedgerId")
      val stream = instance.events(None)
      stream.runWith(Sink.seq).map { actual =>
        actual should have size 1
      }
    }
  }
}
