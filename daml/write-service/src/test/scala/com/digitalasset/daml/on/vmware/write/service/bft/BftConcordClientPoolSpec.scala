// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.util.concurrent.TimeUnit

import com.codahale.metrics.MetricRegistry
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.RetryStrategy
import com.google.protobuf.ByteString
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.{Assertion, AsyncWordSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class BftConcordClientPoolSpec extends AsyncWordSpec with Matchers with MockitoSugar {

  "BftConcordClientPool" should {
    "return Acknowledged in case of a 0 result from the native client" in {
      testSendRequestNative(expected = Success(0, SubmissionResult.Acknowledged))
    }

    "emit 1 total and 1 successful metrics in case of a 0 result from the native client" in {
      val (bftConcordClientPool, requestFuture) =
        sendRequestWithNativeResult(nativeResult = Success(0))
      requestFuture.map { _ =>
        testMetrics(
          bftConcordClientPool,
          expectedSubmissions = 1,
          expectedSuccessfulSubmissions = 1,
          expectedSubmissionSizes = Array(3),
          expectedAttempts = Array(1),
          expectedTotalWaitTimesNanos = Array(0),
        )
      }
    }

    "return Overloaded in case of a 1 result from the native client" in {
      testSendRequestNative(expected = Success(1, SubmissionResult.Overloaded))
    }

    "emit 1 total and 1 failed (overload) metrics in case of a 1 result from the native client" in {
      val (bftConcordClientPool, requestFuture) =
        sendRequestWithNativeResult(nativeResult = Success(1))
      requestFuture.map { _ =>
        testMetrics(
          bftConcordClientPool,
          expectedSubmissions = 1,
          expectedOverloadFailedSubmissions = 1,
          expectedSubmissionSizes = Array(3),
          expectedAttempts = Array(1),
          expectedTotalWaitTimesNanos = Array(0),
        )
      }
    }

    "use the retry strategy in case of a 1 result from the native client" in {
      val (bftConcordClientPool, requestFuture) =
        sendRequestWithNativeResult(
          nativeResult = Success(1),
          shouldRetry = _ == BftConcordClientPool.OverloadedException)
      requestFuture.map { _ =>
        testMetrics(
          bftConcordClientPool,
          expectedSubmissions = 2,
          expectedOverloadFailedSubmissions = 1,
          expectedSubmissionSizes = Array(3, 3),
          expectedAttempts = Array(2),
          expectedTotalWaitTimesNanos = Array(1.milli.toNanos),
        )
      }
    }

    "return InternalError in case of a 2 result from the native client" in {
      testSendRequestNative(
        expected = Success(2, SubmissionResult.InternalError("please see the relevant logs")),
      )
    }

    "emit 1 total and 1 failed (other) metrics in case of a 2 result from the native client" in {
      val (bftConcordClientPool, requestFuture) =
        sendRequestWithNativeResult(nativeResult = Success(2))
      requestFuture.map { _ =>
        testMetrics(
          bftConcordClientPool,
          expectedSubmissions = 1,
          expectedOtherFailedSubmissions = 1,
          expectedSubmissionSizes = Array(3),
          expectedAttempts = Array(1),
          expectedTotalWaitTimesNanos = Array(0),
        )
      }
    }

    "let the exception bubble up in case of a non-fatal throw from the native client" in {
      testSendRequestNative(
        expected = Failure(new RuntimeException("ERROR")),
      )
    }

    "emit 1 total and 1 failed (exception) metrics in case of a non-fatal throw from the native client" in {
      val (bftConcordClientPool, requestFuture) =
        sendRequestWithNativeResult(
          nativeResult = Failure(new RuntimeException("ERROR")),
        )
      requestFuture.failed.map { _ =>
        testMetrics(
          bftConcordClientPool,
          expectedSubmissions = 1,
          expectedExceptionFailedSubmissions = 1,
          expectedSubmissionSizes = Array(3),
          expectedAttempts = Array(1),
          expectedTotalWaitTimesNanos = Array(0),
        )
      }
    }
  }

  private def testMetrics(
      bftConcordClientPool: BftConcordClientPool,
      expectedSubmissions: Int = 0,
      expectedSuccessfulSubmissions: Int = 0,
      expectedOverloadFailedSubmissions: Int = 0,
      expectedOtherFailedSubmissions: Int = 0,
      expectedExceptionFailedSubmissions: Int = 0,
      expectedSubmissionSizes: Array[Int] = Array(anEnvelope.size),
      expectedAttempts: Array[Int] = Array(1),
      expectedTotalWaitTimesNanos: Array[Long] = Array(0),
  ): Assertion = {
    bftConcordClientPool.Metrics.submissions.getCount shouldBe expectedSubmissions
    bftConcordClientPool.Metrics.successfulSubmissions.getCount shouldBe expectedSuccessfulSubmissions
    bftConcordClientPool.Metrics.overloadFailedSubmissions.getCount shouldBe expectedOverloadFailedSubmissions
    bftConcordClientPool.Metrics.otherFailedSubmissions.getCount shouldBe expectedOtherFailedSubmissions
    bftConcordClientPool.Metrics.exceptionFailedSubmissions.getCount shouldBe expectedExceptionFailedSubmissions
    bftConcordClientPool.Metrics.submissionSizes.getSnapshot.getValues shouldBe expectedSubmissionSizes
    bftConcordClientPool.Metrics.sendAttempts.getSnapshot.getValues shouldBe expectedAttempts
    bftConcordClientPool.Metrics.sendRetryTotalWaitNanos.getSnapshot.getValues shouldBe expectedTotalWaitTimesNanos
  }

  private def testSendRequestNative(
      expected: Try[(Int, SubmissionResult)],
      shouldRetry: Throwable => Boolean = _ => false,
  ): Future[Assertion] =
    expected match {
      case Success((expectedNativeReturnValue, expectedValue)) =>
        sendRequestWithNativeResult(nativeResult = Success(expectedNativeReturnValue), shouldRetry)._2
          .map { actual =>
            actual shouldBe expectedValue
          }
      case Failure(expectedException) =>
        sendRequestWithNativeResult(nativeResult = Failure(expectedException), shouldRetry)._2.failed
          .map { exception =>
            exception shouldBe expectedException
          }
    }

  private def sendRequestWithNativeResult(
      nativeResult: Try[Int],
      shouldRetry: Throwable => Boolean = _ => false,
      sendRetryStrategyFactory: (Throwable => Boolean) => RetryStrategy = shouldRetry =>
        RetryStrategy.constant(shouldRetry, retries = 1, waitTime = 1.milli),
  ): (BftConcordClientPool, Future[SubmissionResult]) = {
    val metrics = new Metrics(new MetricRegistry)
    val bftConcordClientPoolJni = mock[BftConcordClientPoolJni]
    val premise = when(bftConcordClientPoolJni.sendRequest(any(), any(), any(), any()))
    nativeResult match {
      case Success(nativeReturnValue) =>
        premise
          .thenReturn(nativeReturnValue)
      case Failure(expectedException) =>
        premise
          .thenThrow(expectedException)
    }
    val bftConcordClientPool =
      new BftConcordClientPool(
        bftConcordClientPoolJni,
        () => sendRetryStrategyFactory(shouldRetry),
        metrics)
    (
      bftConcordClientPool,
      bftConcordClientPool
        .sendRequest(anEnvelope, aDuration, preExecute = false, aCorrelationId))
  }

  private val anEnvelope = ByteString.copyFrom(Array[Byte](0, 1, 2))
  private val aCorrelationId = "correlationId"
  private val aDuration = Duration.create(30, TimeUnit.SECONDS)
}
