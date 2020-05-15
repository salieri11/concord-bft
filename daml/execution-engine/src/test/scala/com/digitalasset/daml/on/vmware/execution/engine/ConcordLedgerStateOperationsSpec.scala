package com.digitalasset.daml.on.vmware.execution.engine

import com.daml.ledger.validator.privacy.{PublicAccess, RestrictedAccess}
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ProtectedKeyValuePair
}
import com.google.protobuf.ByteString
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.Mockito._
import ConcordLedgerStateOperations.accessControlListToThinReplicaIds
import ConcordLedgerStateOperations.VisibleToThisReplicaOnly
import com.daml.ledger.participant.state.v1

class ConcordLedgerStateOperationsSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  private trait SendEventFunction {
    def sendEvent(event: EventFromValidator): Unit
  }

  "ledger state operations" should {
    "resolve a read after a corresponding handleReadResult" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent)
      val expectedReadTag = instance.nextReadTag.get()
      val readFuture = instance.read(Seq(aKey()))

      instance.readPromises should contain key expectedReadTag
      val readResult = EventToValidator
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(aKeyValuePair())
      instance.handleReadResult(readResult)
      readFuture.map { actual =>
        actual should have size 1
        actual.head shouldBe Some(aKeyValuePair().head.value)
        instance.readPromises should have size 0
      }
    }

    "throw in case of an unknown read tag received" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent)
      val input = EventToValidator.ReadResult().withTag("an invalid tag")
      assertThrows[RuntimeException](instance.handleReadResult(input))
    }
  }

  "read" should {
    "create promise and send Read event" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent)
      val expectedReadTag = instance.nextReadTag.get()
      val _ = instance.read(Seq(aKey()))

      instance.readPromises should have size 1
      instance.readPromises should contain key expectedReadTag
      val expectedEvent = EventFromValidator().withRead(
        EventFromValidator.Read(tag = expectedReadTag.toString, keys = Seq(aKey()))
      )
      verify(mockEventSender, times(1)).sendEvent(expectedEvent)
      succeed
    }

    "return None for an empty value" in {
      val instance = new ConcordLedgerStateOperations(mock[SendEventFunction].sendEvent)
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
  }

  "write" should {
    "send write event" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      instance.write(Seq((expectedKey, expectedValue, PublicAccess))).map { _ =>
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
        succeed
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

  private def aKey(): ByteString = ByteString.copyFromUtf8("a key")

  private def aKeyValuePair(): Seq[KeyValuePair] = Seq(
    KeyValuePair().withKey(aKey()).withValue(ByteString.copyFromUtf8("some value"))
  )
}
