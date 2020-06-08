package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.participant.state.v1
import com.daml.ledger.validator.privacy.{PublicAccess, RestrictedAccess}
import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateOperations.{
  VisibleToThisReplicaOnly,
  accessControlListToThinReplicaIds
}
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ProtectedKeyValuePair
}
import com.google.protobuf.ByteString
import org.mockito.ArgumentCaptor
import org.mockito.Mockito._
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class ConcordLedgerStateOperationsSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  private trait SendEventFunction {
    def sendEvent(event: EventFromValidator): Unit
  }

  "ledger state operations" should {
    "resolve a read after a corresponding handleReadResult" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent, metrics)
      val expectedReadTag = instance.nextReadTag.get()
      val readFuture = instance.read(Seq(aKey()))

      instance.pendingReads should contain key expectedReadTag
      val readResult = EventToValidator
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(aKeyValuePair())
      instance.handleReadResult(readResult)
      readFuture.map { actual =>
        actual should have size 1
        actual.head shouldBe Some(aKeyValuePair().head.value)
        instance.pendingReads should have size 0
      }
    }

    "throw in case of an unknown read tag received" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent, metrics)
      val input = EventToValidator.ReadResult().withTag("an invalid tag")
      assertThrows[RuntimeException](instance.handleReadResult(input))
    }

    "update and reset correlation ID" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent, metrics)
      instance.correlationId should have size 0
      instance.updateCorrelationId(Some("correlation ID"))
      instance.correlationId.toString === "correlation ID"
      instance.updateCorrelationId(None)
      instance.correlationId should have size 0
    }
  }

  "read" should {
    "create promise and send Read event" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent, metrics)
      val expectedReadTag = instance.nextReadTag.get()
      val _ = instance.read(Seq(aKey()))

      instance.pendingReads should have size 1
      instance.pendingReads should contain key expectedReadTag
      val expectedEvent = EventFromValidator().withRead(
        EventFromValidator.Read(tag = expectedReadTag.toString, keys = Seq(aKey()))
      )
      verify(mockEventSender, times(1)).sendEvent(expectedEvent)
      succeed
    }

    "return None for an empty value" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent, metrics)
      val expectedReadTag = instance.nextReadTag.get()
      val readFuture = instance.read(Seq(aKey()))
      val readResult = EventToValidator
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(Seq(KeyValuePair().withKey(aKey()).withValue(ByteString.EMPTY)))
      instance.handleReadResult(readResult)

      readFuture.map { actual =>
        actual should have size 1
        actual.head shouldBe None
      }
    }

    "update metric" in {
      val metricRegistry = new MetricRegistry
      val metrics = new ConcordLedgerStateOperations.Metrics(metricRegistry)
      val instance =
        new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent, metrics)
      val expectedReadTag = instance.nextReadTag.get()
      val readResult = EventToValidator
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(Seq(KeyValuePair().withKey(aKey()).withValue(ByteString.EMPTY)))

      val readFuture = instance.read(Seq(aKey()))
      instance.handleReadResult(readResult)

      readFuture.map { _ =>
        val actualTimingValues = metrics.readCompletionTime.getSnapshot.getValues
        actualTimingValues should have size 1
        actualTimingValues.head should be > 1L
      }
    }
  }

  "write" should {
    "buffer key-value pairs" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent, metrics)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      instance.write(Seq((expectedKey, expectedValue, PublicAccess))).map { _ =>
        instance.pendingWrites should have size 1
        instance.pendingWrites shouldBe Seq((expectedKey, expectedValue, PublicAccess))
      }
    }
  }

  "flushing writes" should {
    "send write event" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent, metrics)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      for {
        _ <- instance.write(Seq((expectedKey, expectedValue, PublicAccess)))
        _ <- instance.flushWrites()
      } yield {
        val expectedAccessControlList =
          ConcordLedgerStateOperations.accessControlListToThinReplicaIds(PublicAccess)
        val expectedProtectedKeyValuePair = ProtectedKeyValuePair()
          .withKey(expectedKey)
          .withValue(expectedValue)
          .withTrids(expectedAccessControlList)
        val expectedEvent = EventFromValidator().withWrite(
          EventFromValidator
            .Write()
            .withUpdates(Seq(expectedProtectedKeyValuePair)))
        verify(mockEventSender, times(1)).sendEvent(expectedEvent)
        instance.pendingWrites should have size 0
      }
    }

    "update metric" in {
      val mockEventSender = mock[SendEventFunction]
      val eventCaptor =
        ArgumentCaptor
          .forClass(classOf[EventFromValidator])
          .asInstanceOf[ArgumentCaptor[EventFromValidator]]
      doNothing().when(mockEventSender).sendEvent(eventCaptor.capture())
      val metricRegistry = new MetricRegistry
      val metrics = new ConcordLedgerStateOperations.Metrics(metricRegistry)
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent, metrics)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      for {
        _ <- instance.write(Seq((expectedKey, expectedValue, PublicAccess)))
        _ <- instance.flushWrites()
      } yield {
        val capturedEvent = eventCaptor.getValue
        val expectedWrittenBytes = capturedEvent.getWrite.serializedSize
        metrics.bytesWritten.getSnapshot.getValues shouldBe Array(expectedWrittenBytes)
      }
    }
  }

  "access control to thin replica ID list conversion" should {
    "return empty list for public access" in {
      accessControlListToThinReplicaIds(PublicAccess) shouldBe Seq.empty
    }

    "sort participant IDs for restricted access" in {
      val someParticipants = Set(
        v1.ParticipantId.assertFromString("c"),
        v1.ParticipantId.assertFromString("a"),
        v1.ParticipantId.assertFromString("b")
      )
      accessControlListToThinReplicaIds(RestrictedAccess(someParticipants)) shouldBe Seq(
        "a",
        "b",
        "c"
      )
    }

    "return magic list for restricted access with no participants" in {
      accessControlListToThinReplicaIds(RestrictedAccess(Set.empty)) shouldBe VisibleToThisReplicaOnly
    }
  }

  private val metrics = new ConcordLedgerStateOperations.Metrics(new MetricRegistry)

  private def aKey(): ByteString = ByteString.copyFromUtf8("a key")

  private def aKeyValuePair(): Seq[KeyValuePair] = Seq(
    KeyValuePair().withKey(aKey()).withValue(ByteString.copyFromUtf8("some value"))
  )
}
