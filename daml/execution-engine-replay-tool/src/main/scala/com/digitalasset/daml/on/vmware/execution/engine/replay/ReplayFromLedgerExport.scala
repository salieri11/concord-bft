package com.digitalasset.daml.on.vmware.execution.engine.replay

import java.io.DataInputStream
import java.util
import java.util.concurrent.{CountDownLatch, TimeUnit}

import com.daml.ledger.participant.state.kvutils.export.FileBasedLedgerDataExporter.{
  SubmissionInfo,
  WriteSet
}
import com.daml.ledger.participant.state.kvutils.export.Serialization
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
import com.google.protobuf.{ByteString, Empty}
import com.google.protobuf.timestamp.Timestamp
import io.grpc.Metadata
import io.grpc.stub.{MetadataUtils, StreamObserver}
import io.opentracing.Span
import io.opentracing.contrib.tracerresolver.TracerResolver
import io.opentracing.propagation.Format.Builtin
import io.opentracing.propagation.TextMap

import scala.collection.JavaConverters._
import scala.collection.mutable

class ReplayFromLedgerExport(validationService: ValidationServiceStub) {
  type InMemoryStore = mutable.Map[ByteString, ByteString]

  private val showDetails = false

  def run(input: DataInputStream): Unit = {
    processSubmissions(input)
  }

  private def processSubmissions(input: DataInputStream): Unit = {
    val state = mutable.Map[ByteString, ByteString]()
    var counter = 0
    while (input.available() > 0) {
      val (submissionInfo, expectedWriteSet) = Serialization.readEntry(input)
      val actualWriteSet = runValidation(submissionInfo, state)
      val sortedActualWriteSet = actualWriteSet.sortBy(_._1.asReadOnlyByteBuffer())
      if (expectedWriteSet == sortedActualWriteSet) {
        println("OK")
      } else {
        println("FAIL")
        if (showDetails) {
          println("Expected:")
          printWriteSet(expectedWriteSet)
          println("Actual:")
          printWriteSet(sortedActualWriteSet)
        }
      }
      counter += 1
    }
    println(s"Processed $counter submissions")
  }

  private def printWriteSet(writeSet: WriteSet): Unit =
    for ((key, value) <- writeSet) {
      println(s"${bytesAsHexString(key)} -> ${bytesAsHexString(value)}")
    }

  private def bytesAsHexString(bytes: ByteString): String =
    bytes.toByteArray.map(byte => "%02x".format(byte)).mkString

  // Without a service name jaeger-client won't initialize.
  System.setProperty("JAEGER_SERVICE_NAME", "submission-replay")
  private val tracer = TracerResolver.resolveTracer()

  private def runValidation(info: SubmissionInfo, store: InMemoryStore): WriteSet = {
    val doneLatch = new CountDownLatch(1)
    val span = createSpan()
    val decoratedStub = decorateStubWithTracingHeaders(validationService, span)
    val fromValidatorStream = new ValidationClientStreamObserver(store, doneLatch)
    val toValidatorStream = decoratedStub.validate(fromValidatorStream)
    fromValidatorStream.toValidatorStream = toValidatorStream
    val validateRequest = ValidateRequest(
      submission = info.submissionEnvelope,
      recordTime =
        Some(Timestamp.of(info.recordTimeInstant.getEpochSecond, info.recordTimeInstant.getNano)),
      participantId = info.participantId,
      replicaId = 1,
      correlationId = info.correlationId
    )
    toValidatorStream.onNext(
      EventToValidator(EventToValidator.ToValidator.ValidateRequest(validateRequest)))
    doneLatch.await(10, TimeUnit.SECONDS)
    span.finish(nowMicros())
    fromValidatorStream.getCollectedWriteSet
  }

  class ValidationClientStreamObserver(store: InMemoryStore, doneLatch: CountDownLatch)
      extends StreamObserver[EventFromValidator] {
    var toValidatorStream: StreamObserver[EventToValidator] = _

    private val collectedWriteSet = mutable.Buffer[(Key, Value)]()

    def getCollectedWriteSet: WriteSet = collectedWriteSet

    override def onNext(value: EventFromValidator): Unit =
      value match {
        case EventFromValidator(EventFromValidator.FromValidator.Read(Read(tag, keys))) =>
          val keyValuePairs = keys.map(store.get).zip(keys).collect {
            case (Some(key), value) => KeyValuePair(key, value)
          }
          val readResult = ReadResult(tag, keyValuePairs)
          val response = EventToValidator(EventToValidator.ToValidator.ReadResult(readResult))
          toValidatorStream.onNext(response)
        case EventFromValidator(EventFromValidator.FromValidator.Done(Done(_, writeSet))) =>
          println(s"Received write-set of size=${writeSet.size}")
          // Save and apply write-set.
          for (protectedKeyValuePair <- writeSet) {
            collectedWriteSet.append((protectedKeyValuePair.key, protectedKeyValuePair.value))
            store.put(protectedKeyValuePair.key, protectedKeyValuePair.value)
          }
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

  private def nowMicros(): Long =
    TimeUnit.MILLISECONDS.toMicros(System.currentTimeMillis())

  private def createSpan(): Span =
    tracer
      .buildSpan("RunValidation")
      .withTag("test-key", "test-value")
      .withStartTimestamp(nowMicros())
      .start()

  private def decorateStubWithTracingHeaders[T <: io.grpc.stub.AbstractStub[T]](
      stub: T,
      parentSpan: Span): T = {
    val extraHeaders = injectSpanAsMetadata(parentSpan)
    MetadataUtils.attachHeaders(stub, extraHeaders)
  }

  private def injectSpanAsMetadata(span: Span): Metadata = {
    val extraHeaders = new Metadata()
    tracer.inject(
      span.context(),
      Builtin.TEXT_MAP,
      new TextMap {
        override def iterator(): util.Iterator[util.Map.Entry[String, String]] =
          extraHeaders
            .keys()
            .iterator()
            .asScala
            .map { key =>
              val entry: util.Map.Entry[String, String] =
                new util.AbstractMap.SimpleEntry[String, String](
                  key,
                  extraHeaders.get(Metadata.Key.of(key, Metadata.ASCII_STRING_MARSHALLER))
                )
              entry
            }
            .asJava

        override def put(key: String, value: String): Unit = {
          val metadataKey = Metadata.Key.of(key, Metadata.ASCII_STRING_MARSHALLER)
          extraHeaders.put(metadataKey, value)
        }
      }
    )
    extraHeaders
  }
}
