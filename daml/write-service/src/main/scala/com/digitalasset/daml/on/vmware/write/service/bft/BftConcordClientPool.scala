// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.codahale.metrics.{Counter, Histogram, Timer}
import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.pkvutils.metrics.Timed
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.daml.metrics.{MetricName, Metrics}
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient.backOff
import com.google.protobuf.ByteString
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

sealed abstract class ClientMsgFlag(val value: Int)
object ClientMsgFlag {
  case object None extends ClientMsgFlag(value = 0)
  case object ReadOnly extends ClientMsgFlag(value = 1)
  case object PreExec extends ClientMsgFlag(value = 2)
}

/**
  * This trait contains the JVM implementation part of the native BFT Concord Client Pool, while the native parts
  * are left abstract here and are implemented natively by [[BftConcordClientPoolNative]].
  */
private[bft] class BftConcordClientPool(
    nativeClient: BftConcordClientPoolNative,
    metrics: Metrics,
    shouldRetry: Throwable => Boolean = BftConcordClientPool.shouldRetry)
    extends AutoCloseable {

  import com.digitalasset.daml.on.vmware.write.service.bft.BftConcordClientPool._

  /**
    * Asynchronously sends a request to Concord and reports the submission acceptance status.
    *
    * @param request A serialized [[com.vmware.concord.concord.ConcordRequest]] Protobuf instance.
    * @param timeout The timeout used by the synchronous client that will send the request to Concord.
    * @param flag The pre-execution mode, if any.
    * @param correlationId The correlation ID of the request for observability.
    * @return A [[Future]] delivering an [[SubmissionResult]] value asynchronously.
    */
  def sendRequest(
      request: ByteString,
      timeout: Duration,
      flag: ClientMsgFlag,
      correlationId: String)(
      implicit executionContext: ExecutionContext): Future[SubmissionResult] =
    Timed.future(
      Metrics.submitTimer,
      backOff(shouldRetry) { _ =>
        Future {
          val requestSize = request.size
          Metrics.submissionSizes.update(requestSize)
          logger.debug(
            s"Submitting request, correlationId=$correlationId serializedRequestSize=$requestSize")
          Metrics.submissions.inc()
          nativeSendResultToSubmissionResult(
            nativeClient
              .sendRequestNative(request.toByteArray, timeout.toMillis, flag.value, correlationId))
        }
      }.transform {
          case success @ Success(submissionResult) =>
            submissionResult match {
              case SubmissionResult.Acknowledged =>
                Metrics.successfulSubmissions.inc()
                logger.debug(s"The submission has been acknowledged, correlationId=$correlationId")
              case SubmissionResult.Overloaded =>
              case SubmissionResult.InternalError(_) =>
                Metrics.otherFailedSubmissions.inc()
                logger.debug(
                  s"The submission failed due to an internal error, please see the log; correlationId=$correlationId")
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
        .recoverWith {
          case t =>
            Metrics.exceptionFailedSubmissions.inc()
            Future.failed(t)
        }
    )

  def currentHealth: HealthStatus = nativeHealthToHealthStatus(nativeClient.currentHealthNative)

  override def close(): Unit = nativeClient.close()

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
  }

  private[this] val logger = LoggerFactory.getLogger(this.getClass)
}

object BftConcordClientPool {
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

  private object OverloadedException extends RuntimeException

  private def shouldRetry(throwable: Throwable): Boolean =
    throwable == OverloadedException
}