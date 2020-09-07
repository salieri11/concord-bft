// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.participant.state.v1
import com.daml.ledger.validator.LedgerStateOperations.Value
import com.daml.ledger.validator.privacy.{PublicAccess, RestrictedAccess}
import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateOperations.{
  HiddenFromAllParticipants,
  accessControlListToThinReplicaIds
}
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ProtectedKeyValuePair
}
import com.google.protobuf.ByteString
import org.mockito.Mockito._

import scala.concurrent.Future

class ConcordLedgerStateOperationsSpec
    extends ConcordLedgerStateReadHandlerFixture[
      EventFromValidator,
      EventToValidator.ReadResult,
      KeyValuePair,
      Option[Value],
      ConcordLedgerStateOperations] {

  "read" should {
    "create promise and send Read event" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = createLedgerStateOperations(mockEventSender)
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
      val instance = createLedgerStateOperations(mock[SendEventFunction])
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
      val metrics = new ConcordLedgerStateOperationsMetrics(metricRegistry)
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
      val instance = createLedgerStateOperations(mockEventSender)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      instance.write(Seq((expectedKey, expectedValue, PublicAccess))).map { _ =>
        instance.pendingWrites should have size 1
        instance.pendingWrites shouldBe Seq((expectedKey, expectedValue, PublicAccess))
      }
    }
  }

  "clearing write set" should {
    "return written key-value pair" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = createLedgerStateOperations(mockEventSender)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      for {
        _ <- instance.write(Seq((expectedKey, expectedValue, PublicAccess)))
        actualWriteSet <- Future.successful(instance.getAndClearWriteSet())
      } yield {
        val expectedAccessControlList =
          ConcordLedgerStateOperations.accessControlListToThinReplicaIds(PublicAccess)
        val expectedProtectedKeyValuePair = ProtectedKeyValuePair()
          .withKey(expectedKey)
          .withValue(expectedValue)
          .withTrids(expectedAccessControlList)
        val expectedWriteSet = Seq(expectedProtectedKeyValuePair)
        actualWriteSet shouldBe expectedWriteSet
        instance.pendingWrites should have size 0
      }
    }

    "update metric" in {
      val mockEventSender = mock[SendEventFunction]
      val metricRegistry = new MetricRegistry
      val metrics = new ConcordLedgerStateOperationsMetrics(metricRegistry)
      val instance = new ConcordLedgerStateOperations(mockEventSender.sendEvent, metrics)
      val expectedKey = aKey()
      val expectedValue = ByteString.copyFromUtf8("some value")

      for {
        _ <- instance.write(Seq((expectedKey, expectedValue, PublicAccess)))
        actualWriteSet <- Future.successful(instance.getAndClearWriteSet())
      } yield {
        val expectedWrittenBytes = actualWriteSet.map(_.serializedSize).sum
        metrics.bytesWritten.getSnapshot.getValues shouldBe Array(expectedWrittenBytes)
      }
    }
  }

  "access control to thin replica ID list conversion" should {
    "return empty list for public access" in {
      accessControlListToThinReplicaIds(PublicAccess) shouldBe Seq.empty
    }

    "returns sorted participant IDs for restricted access" in {
      val accessControlList = RestrictedAccess(
        v1.ParticipantId.assertFromString("c"),
        v1.ParticipantId.assertFromString("a"),
        v1.ParticipantId.assertFromString("b"),
      )

      accessControlListToThinReplicaIds(accessControlList) shouldBe Seq("a", "b", "c")
    }

    "return magic list for restricted access with no participants" in {
      accessControlListToThinReplicaIds(RestrictedAccess.empty) shouldBe HiddenFromAllParticipants
    }
  }

  override protected def createLedgerStateOperations(
      sendEventFunction: SendEventFunction): ConcordLedgerStateOperations =
    new ConcordLedgerStateOperations(sendEventFunction.sendEvent, metrics)

  override protected def read(
      stateOperations: ConcordLedgerStateOperations,
      keys: Seq[Value]): Future[Seq[Option[Value]]] = stateOperations.read(keys)

  override protected def aReadResultWithTagAndOneKey(tag: String): EventToValidator.ReadResult =
    EventToValidator
      .ReadResult()
      .withTag(tag)
      .withKeyValuePairs(aKeyValuePair())

  override protected def anEmptyReadResultWithInvalidTag(): EventToValidator.ReadResult =
    EventToValidator
      .ReadResult()
      .withTag(AnInvalidTag)

  protected def aKeyValuePair(): Seq[KeyValuePair] = Seq(
    KeyValuePair().withKey(aKey()).withValue(ByteString.copyFromUtf8("some value"))
  )

  override protected def aReadElementWithOneKey(): Option[Value] = Some(aKeyValuePair().head.value)
}
