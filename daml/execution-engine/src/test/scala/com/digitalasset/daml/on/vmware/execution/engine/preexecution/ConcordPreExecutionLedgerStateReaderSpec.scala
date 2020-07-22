// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.preexecution

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.participant.state.kvutils
import com.daml.ledger.participant.state.kvutils.Fingerprint
import com.daml.ledger.validator.LedgerStateOperations.Value
import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateReadHandlerFixture
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.PreprocessorToEngine.ReadResult
import com.digitalasset.kvbc.daml_validator._
import com.google.protobuf.ByteString
import org.mockito.Mockito._

import scala.concurrent.Future

class ConcordPreExecutionLedgerStateReaderSpec
    extends ConcordLedgerStateReadHandlerFixture[
      PreprocessorFromEngine,
      ReadResult,
      KeyValueFingerprintTriple,
      (Option[Value], Fingerprint),
      ConcordPreExecutionLedgerStateReader] {

  "read" should {
    "create promise and send Read event" in {
      val mockEventSender = mock[SendEventFunction]
      val instance = createLedgerStateOperations(mockEventSender)
      val expectedReadTag = instance.nextReadTag.get()
      val _ = instance.read(Seq(aKey()))

      instance.pendingReads should have size 1
      instance.pendingReads should contain key expectedReadTag
      val expectedEvent = PreprocessorFromEngine().withReadRequest(
        PreprocessorFromEngine.ReadRequest(tag = expectedReadTag.toString, keys = Seq(aKey()))
      )
      verify(mockEventSender, times(1)).sendEvent(expectedEvent)
      succeed
    }

    "return None and empty fingerprint for an empty value" in {
      val instance = createLedgerStateOperations(mock[SendEventFunction])
      val expectedReadTag = instance.nextReadTag.get()
      val readFuture = instance.read(Seq(aKey()))
      val readResult = PreprocessorToEngine
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(
          Seq(
            KeyValueFingerprintTriple()
              .withKey(aKey())
              .withValue(ByteString.EMPTY)
              .withFingerprint(aFingerprintForEmptyValue())))
      instance.handleReadResult(readResult)

      readFuture.map { actual =>
        actual should have size 1
        actual.head shouldBe (None, aFingerprintForEmptyValue())
      }
    }

    "throw for an empty fingerprint" in {
      val instance = createLedgerStateOperations(mock[SendEventFunction])
      val expectedReadTag = instance.nextReadTag.get()
      val readFuture = instance.read(Seq(aKey()))
      val readResult = PreprocessorToEngine
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(
          Seq(
            KeyValueFingerprintTriple()
              .withKey(aKey())
              .withValue(aValue())
              .withFingerprint(ByteString.EMPTY)))
      instance.handleReadResult(readResult)

      readFuture.failed.map { exception =>
        exception shouldBe a[kvutils.Err.DecodeError]
      }
    }

    "update metric" in {
      val metrics = new ConcordLedgerStateOperationsMetrics(new MetricRegistry)
      val instance =
        new ConcordPreExecutionLedgerStateReader(mock[SendEventFunction].sendEvent, metrics)
      val expectedReadTag = instance.nextReadTag.get()
      val readResult = PreprocessorToEngine
        .ReadResult()
        .withTag(expectedReadTag.toString)
        .withKeyValuePairs(
          Seq(
            KeyValueFingerprintTriple()
              .withKey(aKey())
              .withValue(ByteString.EMPTY)
              .withFingerprint(aFingerprintForEmptyValue())))

      val readFuture = instance.read(Seq(aKey()))
      instance.handleReadResult(readResult)

      readFuture.map { _ =>
        val actualTimingValues = metrics.readCompletionTime.getSnapshot.getValues
        actualTimingValues should have size 1
        actualTimingValues.head should be > 1L
      }
    }
  }

  override protected def createLedgerStateOperations(
      sendEventFunction: SendEventFunction): ConcordPreExecutionLedgerStateReader =
    new ConcordPreExecutionLedgerStateReader(sendEventFunction.sendEvent, metrics)

  override protected def read(
      stateOperations: ConcordPreExecutionLedgerStateReader,
      keys: Seq[Value]): Future[Seq[(Option[Value], Fingerprint)]] =
    stateOperations.read(keys)

  override protected def aReadResultWithTagAndOneKey(tag: String): ReadResult =
    PreprocessorToEngine
      .ReadResult()
      .withTag(tag)
      .withKeyValuePairs(aKeyValueFingerprintTriple())

  override protected def anEmptyReadResultWithInvalidTag(): ReadResult =
    PreprocessorToEngine
      .ReadResult()
      .withTag(AnInvalidTag)

  protected def aFingerprintForEmptyValue(): ByteString =
    ByteString.copyFromUtf8("special fingerprint")

  protected def aKeyValueFingerprintTriple(): Seq[KeyValueFingerprintTriple] = {
    val value = ByteString.copyFromUtf8("some value")
    Seq(
      KeyValueFingerprintTriple()
        .withKey(aKey())
        .withValue(value)
        .withFingerprint(value)
    )
  }

  override protected def aReadElementWithOneKey(): (Option[Value], Fingerprint) =
    (Some(aKeyValueFingerprintTriple().head.value), aKeyValueFingerprintTriple().head.fingerprint)
}
