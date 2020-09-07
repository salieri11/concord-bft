// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.replay

import java.util.concurrent.{CountDownLatch, TimeUnit}

import com.daml.ledger.participant.state.kvutils.export.FileBasedLedgerDataExporter.{
  SubmissionInfo,
  WriteSet
}
import com.daml.ledger.validator.LedgerStateOperations.{Key, Value}
import com.digitalasset.kvbc.daml_validator.EventFromValidator.{Done, Read}
import com.digitalasset.kvbc.daml_validator.EventToValidator.ReadResult
import com.digitalasset.kvbc.daml_validator.ValidationServiceGrpc.ValidationServiceStub
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ValidateRequest
}
import com.google.protobuf.timestamp.Timestamp
import io.grpc.stub.StreamObserver
import io.opentracing.{SpanContext, Tracer}

import scala.collection.mutable

final class ReplayFromLedgerExportThroughBatching(
    override val validationService: ValidationServiceStub,
    override val tracer: Tracer,
) extends ReplayFromLedgerExport {

  override protected def runValidation(
      validationService: ValidationServiceStub,
      spanContext: SpanContext,
      info: SubmissionInfo,
      store: InMemoryStore): Seq[WriteSet] = {
    val doneLatch = new CountDownLatch(1)
    val fromValidatorStream = new ValidationClientStreamObserver(store, doneLatch)
    val toValidatorStream = validationService.validate(fromValidatorStream)
    fromValidatorStream.toValidatorStream = toValidatorStream
    val validateRequest = ValidateRequest(
      submission = info.submissionEnvelope,
      recordTime =
        Some(Timestamp.of(info.recordTimeInstant.getEpochSecond, info.recordTimeInstant.getNano)),
      participantId = info.participantId,
      replicaId = ReplicaId,
      correlationId = info.correlationId
    )
    toValidatorStream.onNext(
      EventToValidator(EventToValidator.ToValidator.ValidateRequest(validateRequest)))
    doneLatch.await(10, TimeUnit.SECONDS)
    Seq(fromValidatorStream.getCollectedWriteSet)
  }

  override protected def validateResults(
      expectedResults: Seq[WriteSet],
      actualResults: Seq[WriteSet]): Unit =
    (expectedResults, actualResults) match {
      case (Seq(expectedResult), Seq(actualResult)) =>
        val sortedActualResult = actualResult.sortBy(_._1.asReadOnlyByteBuffer())
        if (expectedResult == sortedActualResult) {
          println("OK")
        } else {
          println("FAIL")
          if (showDetails) {
            println("Expected:")
            printWriteSet(expectedResult)
            println("Actual:")
            printWriteSet(sortedActualResult)
          }
        }
      case (Seq(_), _) =>
        println(
          s"FAIL: validation was expected to produce exactly 1 write set but got ${actualResults.size} instead")
      case _ =>
        println(
          s"FAIL: deserialization was expected to produce exactly 1 write set but got ${expectedResults.size} instead")
    }

  class ValidationClientStreamObserver(store: InMemoryStore, doneLatch: CountDownLatch)
      extends StreamObserver[EventFromValidator] {
    var toValidatorStream: StreamObserver[EventToValidator] = _

    private val collectedWriteSet = mutable.Buffer[(Key, Value)]()

    def getCollectedWriteSet: WriteSet = collectedWriteSet

    override def onNext(value: EventFromValidator): Unit =
      value match {
        case EventFromValidator(EventFromValidator.FromValidator.Read(Read(tag, keys))) =>
          val keyValuePairs = readFromStore(store, keys).collect {
            case (key, Some(value)) => KeyValuePair.of(key, value)
          }
          val readResult = ReadResult(tag, keyValuePairs)
          val response = EventToValidator(EventToValidator.ToValidator.ReadResult(readResult))
          toValidatorStream.onNext(response)
        case EventFromValidator(EventFromValidator.FromValidator.Done(Done(_, writeSet))) =>
          println(s"Received write-set of size=${writeSet.size}")
          // Save and apply write-set.
          val keyValuePairs = for {
            protectedKeyValuePair <- writeSet
            keyValuePair = (protectedKeyValuePair.key, protectedKeyValuePair.value)
            _ = collectedWriteSet.append()
          } yield keyValuePair
          writeToStore(store, keyValuePairs)
        case _ => ()
      }

    override def onError(error: Throwable): Unit = {
      println(error.getLocalizedMessage)
      doneLatch.countDown()
    }

    override def onCompleted(): Unit = {
      doneLatch.countDown()
    }
  }

  private val showDetails = false
}
