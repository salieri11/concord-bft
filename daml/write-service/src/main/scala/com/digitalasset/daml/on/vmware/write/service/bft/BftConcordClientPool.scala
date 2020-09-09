// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.codahale.metrics.{Counter, Histogram, Timer}
import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.pkvutils.metrics.Timed
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.daml.metrics.{MetricName, Metrics}
import com.digitalasset.daml.on.vmware.write.service.RetryStrategy
import com.google.protobuf.ByteString
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * This trait contains the JVM implementation part of the native BFT Concord Client Pool, while the core functionality
  * is delegated to a [[BftConcordClientPoolCore]].
  */
private[bft] class BftConcordClientPool(
    coreClient: BftConcordClientPoolCore,
    sendRetryStrategyFactory: RetryStrategyFactory,
    metrics: Metrics,
) extends AutoCloseable {

  import com.digitalasset.daml.on.vmware.write.service.bft.BftConcordClientPool._

  /**
    * Asynchronously sends a request to Concord and reports the submission acceptance status.
    *
    * @param request A serialized [[com.vmware.concord.concord.ConcordRequest]] Protobuf instance.
    * @param timeout The timeout used by the synchronous client that will send the request to Concord.
    * @param preExecute Whether to pre-execute.
    * @param correlationId The correlation ID of the request for observability.
    * @return A [[Future]] delivering an [[SubmissionResult]] value asynchronously.
    */
  def sendRequest(
      request: ByteString,
      timeout: Duration,
      preExecute: Boolean,
      correlationId: String)(
      implicit executionContext: ExecutionContext): Future[SubmissionResult] = {
    val retry = sendRetryStrategyFactory()
    Timed.future(
      Metrics.submitTimer,
      retry { _ =>
        Future {
          val requestSize = request.size
          Metrics.submissionSizes.update(requestSize)
          logger.debug(
            s"Submitting request, correlationId=$correlationId serializedRequestSize=$requestSize timeout=$timeout")
          Metrics.submissions.inc()
          val sendRequestNativeResult = coreClient
            .sendRequest(request.toByteArray, timeout.toMillis, preExecute, correlationId)
          logger.debug(
            s"Request submitted, correlationId=$correlationId nativeSendResult=$sendRequestNativeResult")
          nativeSendResultToSubmissionResult(sendRequestNativeResult)
        }
      }.transform { result =>
          Metrics.sendAttempts.update(retry.getAttemptsCount)
          Metrics.sendRetryTotalWaitNanos.update(
            java.time.Duration.ofNanos(retry.extractTotalWait.toNanos))

          result match {
            case success @ Success(submissionResult) =>
              submissionResult match {
                case SubmissionResult.Acknowledged =>
                  Metrics.successfulSubmissions.inc()
                  logger.debug(
                    s"The submission has been acknowledged, correlationId=$correlationId")
                case SubmissionResult.InternalError(_) =>
                  Metrics.otherFailedSubmissions.inc()
                  logger.debug(
                    s"The submission failed due to an internal error, please see the log; correlationId=$correlationId")
                case SubmissionResult.Overloaded =>
                case SubmissionResult.NotSupported => // This should never happen; if it happens, then there's a bug.
                  val msg = s"Unsupported submission, correlationId=$correlationId"
                  logger.debug(msg)
                  throw new IllegalStateException(msg)
              }
              success
            case Failure(OverloadedException) =>
              Metrics.overloadFailedSubmissions.inc()
              logger.debug(s"The submission failed due to overload, correlationId=$correlationId")
              Success(SubmissionResult.Overloaded)
            case otherFailure => otherFailure
          }
        }
        .recoverWith {
          case t =>
            Metrics.exceptionFailedSubmissions.inc()
            Future.failed(t)
        }
    )
  }

  def currentHealth: HealthStatus = {
    val nativeHealth = coreClient.currentHealth
    logger.debug(s"The native BFT client health check returned $nativeHealth")
    nativeHealthToHealthStatus(nativeHealth)
  }

  override def close(): Unit = {
    logger.debug("Closing the native BFT client")
    coreClient.close()
    logger.debug("Native BFT client closed")
  }

  private[bft] object Metrics {
    val prefix: MetricName = MetricName("daml") :+ "bft"

    val submissions: Counter = metrics.registry.counter(prefix :+ "submissions")
    val successfulSubmissions: Counter =
      metrics.registry.counter(prefix :+ "submissions.successful")
    val overloadFailedSubmissions: Counter =
      metrics.registry.counter(prefix :+ "submissions.failed.overload")
    val otherFailedSubmissions: Counter =
      metrics.registry.counter(prefix :+ "submissions.failed.other")
    val exceptionFailedSubmissions: Counter =
      metrics.registry.counter(prefix :+ "submissions.failed.exception")
    val submissionSizes: Histogram = metrics.registry.histogram(prefix :+ "submission-sizes")
    val submitTimer: Timer = metrics.registry.timer(prefix :+ "submit-timer")
    val sendAttempts: Histogram =
      metrics.registry.histogram(prefix :+ "submissions.send.attempts")
    val sendRetryTotalWaitNanos: Timer =
      metrics.registry.timer(prefix :+ "submissions.send.retry-total-wait-nanos")
  }

  private[this] val logger = LoggerFactory.getLogger(this.getClass)
}

object BftConcordClientPool {
  object OverloadedException extends RuntimeException

  private def nativeHealthToHealthStatus(currentHealthNative: Int): HealthStatus =
    currentHealthNative match {
      case 0 => HealthStatus.healthy
      case 1 => HealthStatus.unhealthy
    }

  private def nativeSendResultToSubmissionResult(nativeSendResult: Int): SubmissionResult =
    nativeSendResult match {
      case 0 =>
        SubmissionResult.Acknowledged
      case 1 =>
        throw OverloadedException
      case 2 =>
        SubmissionResult.InternalError("please see the relevant logs")
    }
}
