package com.digitalasset.daml.on.vmware.tracing

import com.daml.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.digitalasset.kvbc.daml_validator.ValidationServiceGrpc.ValidationService
import com.digitalasset.kvbc.daml_validator._
import io.grpc.inprocess.{InProcessChannelBuilder, InProcessServerBuilder}
import io.grpc.stub.{MetadataUtils, StreamObserver}
import io.grpc.{BindableService, Metadata, ServerInterceptors, ServerServiceDefinition}
import io.opentracing.Span
import io.opentracing.mock.{MockSpan, MockTracer}
import io.opentracing.propagation.Format.Builtin
import io.opentracing.propagation.TextMapInject
import org.scalatest.{AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class TracingHelperIntegrationSpec
    extends AsyncWordSpec
    with AkkaBeforeAndAfterAll
    with Matchers
    with MockitoSugar {
  private val tracer = new MockTracer(MockTracer.Propagator.TEXT_MAP)

  "tracing interceptor" should {
    "create and close new child span when receiving streaming call" in {
      val testParentSpan = createSpan()
      val validationServiceImpl = validationServiceAssertingParentSpan(testParentSpan)
      val interceptor = TracingHelper.createTracingInterceptor(tracer)
      val serverName = InProcessServerBuilder.generateName()
      val server = InProcessServerBuilder
        .forName(serverName)
        .directExecutor()
        .addService(ServerInterceptors.intercept(validationServiceImpl, interceptor))
        .build()
        .start()
      val clientChannel = InProcessChannelBuilder.forName(serverName).directExecutor().build()
      val stub =
        decorateStubWithTracingHeaders(ValidationServiceGrpc.stub(clientChannel), testParentSpan)

      val toValidatorStream = stub.validate(mock[StreamObserver[EventFromValidator]])
      toValidatorStream.onNext(EventToValidator().withValidateRequest(ValidateRequest()))
      clientChannel.shutdown()
      server.shutdown()

      tracer.finishedSpans() should not be empty
    }

    "create and close new child span for RPC call" in {
      val testParentSpan = createSpan()
      val validationServiceImpl = validationServiceAssertingParentSpan(testParentSpan)
      val interceptor = TracingHelper.createTracingInterceptor(tracer)
      val serverName = InProcessServerBuilder.generateName()
      val server = InProcessServerBuilder
        .forName(serverName)
        .directExecutor()
        .addService(ServerInterceptors.intercept(validationServiceImpl, interceptor))
        .build()
        .start()
      val clientChannel = InProcessChannelBuilder.forName(serverName).directExecutor().build()
      val stub =
        decorateStubWithTracingHeaders(ValidationServiceGrpc.stub(clientChannel), testParentSpan)

      stub.validateSubmission(ValidateRequest())
      clientChannel.shutdown()
      server.shutdown()

      tracer.finishedSpans() should not be empty
    }

  }

  private def createSpan(): Span =
    tracer
      .buildSpan("validate_client")
      .withTag("test-key", "test-value")
      .withStartTimestamp(1)
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
      Builtin.TEXT_MAP_INJECT,
      new TextMapInject {
        override def put(key: String, value: String): Unit = {
          val metadataKey = Metadata.Key.of(key, Metadata.ASCII_STRING_MARSHALLER)
          extraHeaders.put(metadataKey, value)
        }
      }
    )
    extraHeaders
  }

  private def assertParentSpan(parentSpan: Span): Unit = {
    val activeSpan = tracer.activeSpan().asInstanceOf[MockSpan]
    activeSpan.context().toTraceId shouldBe parentSpan.context().toTraceId
    activeSpan.context().toSpanId shouldNot be(parentSpan.context().toSpanId)
    activeSpan.parentId.toString shouldBe parentSpan.context().toSpanId
  }

  private def validationServiceAssertingParentSpan(
      parentSpan: Span): ValidationService with BindableService =
    new ValidationService with BindableService {
      override def validateSubmission(request: ValidateRequest): Future[ValidateResponse] =
        Future.successful {
          assertParentSpan(parentSpan)
          ValidateResponse().withResponse(ValidateResponse.Response.Empty)
        }

      override def validatePendingSubmission(
          request: ValidatePendingSubmissionRequest): Future[ValidatePendingSubmissionResponse] =
        ???

      override def validate(responseObserver: StreamObserver[EventFromValidator])
        : StreamObserver[EventToValidator] = {
        assertParentSpan(parentSpan)
        responseObserver.onCompleted()
        new StreamObserver[EventToValidator] {
          override def onNext(value: EventToValidator): Unit = ()

          override def onError(t: Throwable): Unit = ()

          override def onCompleted(): Unit = ()
        }
      }

      override def bindService(): ServerServiceDefinition =
        ValidationServiceGrpc.bindService(this, executionContext)
    }
}
