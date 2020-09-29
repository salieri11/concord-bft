// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import com.codahale.metrics.MetricRegistry
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar
import com.google.protobuf.ByteString

import scala.concurrent.Future

trait ConcordLedgerStateReadHandlerFixture[
    FromValidatorEventType,
    ReadResultType,
    ReadSetType,
    ReadElementType,
    StateOperationsType <: ConcordLedgerStateReadHandler[ReadResultType, ReadSetType]]
    extends AsyncWordSpec
    with Matchers
    with MockitoSugar {

  protected trait SendEventFunction {
    def sendEvent(event: FromValidatorEventType): Unit
  }

  protected def createLedgerStateOperations(
      sendEventFunction: SendEventFunction): StateOperationsType
  protected def read(
      stateOperations: StateOperationsType,
      keys: Seq[ByteString]): Future[Seq[ReadElementType]]

  protected def aReadResultWithTagAndOneKey(tag: String): ReadResultType
  protected def anEmptyReadResultWithInvalidTag(): ReadResultType
  protected def aReadElementWithOneKey(): ReadElementType

  "ledger state operations" should {
    "resolve a read after a corresponding handleReadResult" in {
      val instance =
        createLedgerStateOperations(mock[SendEventFunction])
      val expectedReadTag = instance.nextReadTag.get()
      val readFuture = read(instance, Seq(aKey()))

      instance.pendingReads should contain key expectedReadTag
      val result = aReadResultWithTagAndOneKey(expectedReadTag.toString)
      instance.handleReadResult(result)
      readFuture.map { actual =>
        actual should have size 1
        actual.head shouldBe aReadElementWithOneKey()
        instance.pendingReads should have size 0
      }
    }

    "throw in case of an unknown read tag received" in {
      val instance =
        createLedgerStateOperations(mock[SendEventFunction])
      assertThrows[RuntimeException](instance.handleReadResult(anEmptyReadResultWithInvalidTag()))
    }

    "update and reset correlation ID" in {
      val instance =
        createLedgerStateOperations(mock[SendEventFunction])
      instance.correlationId should have size 0
      instance.updateCorrelationId(Some("correlation ID"))
      instance.correlationId.toString === "correlation ID"
      instance.updateCorrelationId(None)
      instance.correlationId should have size 0
    }
  }

  protected def aKey(): ByteString = ByteString.copyFromUtf8("a key")
  protected def aValue(): ByteString = ByteString.copyFromUtf8("a value")

  protected val metrics = new ConcordLedgerStateOperationsMetrics(new MetricRegistry)

  protected val AnInvalidTag = "an invalid tag"
}
