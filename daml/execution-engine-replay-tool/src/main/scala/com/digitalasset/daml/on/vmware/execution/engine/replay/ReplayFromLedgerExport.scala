// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.replay

import java.util
import java.util.concurrent.TimeUnit

import com.daml.ledger.participant.state.kvutils.export.{
  LedgerDataImporter,
  SubmissionInfo,
  WriteSet
}
import com.daml.ledger.validator.LedgerStateOperations.Key
import com.digitalasset.kvbc.daml_validator.ValidationServiceGrpc.ValidationServiceStub
import com.google.protobuf.ByteString
import io.grpc.Metadata
import io.grpc.stub.MetadataUtils
import io.opentracing.propagation.Format.Builtin
import io.opentracing.propagation.TextMap
import io.opentracing.{Span, SpanContext, Tracer}

import scala.collection.JavaConverters._
import scala.collection.mutable

trait ReplayFromLedgerExport {

  def run(importer: LedgerDataImporter): Unit = {
    processSubmissions(importer)
  }

  protected type InMemoryStore = mutable.Map[ByteString, ByteString]
  protected val validationService: ValidationServiceStub
  protected val tracer: Tracer
  protected def runValidation(
      validationService: ValidationServiceStub,
      spanContext: SpanContext,
      info: SubmissionInfo,
      store: InMemoryStore): Seq[WriteSet]
  protected def validateResults(expectedResults: Seq[WriteSet], actualResults: Seq[WriteSet]): Unit

  protected final def printWriteSet(writeSet: WriteSet): Unit =
    for ((key, value) <- writeSet) {
      println(s"${bytesAsHexString(key)} -> ${bytesAsHexString(value)}")
    }

  protected final def readFromStore(
      store: InMemoryStore,
      keys: Seq[Key]): Seq[(ByteString, Option[ByteString])] =
    store.synchronized {
      keys.map(store.get).zip(keys).map(_.swap)
    }

  protected final def writeToStore(
      store: InMemoryStore,
      keyValuePairs: Seq[(ByteString, ByteString)]): Seq[(ByteString, ByteString)] = {
    store.synchronized {
      keyValuePairs.foreach { case (key, value) => store.put(key, value) }
    }
    keyValuePairs
  }

  protected val ReplicaId = 1L

  private def processSubmissions(importer: LedgerDataImporter): Unit = {
    val state = mutable.Map[ByteString, ByteString]()
    val count = importer
      .read()
      .map {
        case (submissionInfo, expectedWriteSet) =>
          val actualWriteSets = decorateWithNewSpan(validationService) {
            (decoratedStub, spanContext) =>
              runValidation(decoratedStub, spanContext, submissionInfo, state)
          }
          validateResults(Seq(expectedWriteSet), actualWriteSets)
      }
      .length
    println(s"Processed $count submissions")
  }

  private def bytesAsHexString(bytes: ByteString): String =
    bytes.toByteArray.map(byte => "%02x".format(byte)).mkString

  private def decorateWithNewSpan[Result](validationService: ValidationServiceStub)(
      body: (ValidationServiceStub, SpanContext) => Result): Result = {
    val span = createSpan()
    val decoratedStub = decorateStubWithTracingHeaders(validationService, span)
    val result = body(decoratedStub, span.context())
    span.finish(nowMicros())
    result
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
