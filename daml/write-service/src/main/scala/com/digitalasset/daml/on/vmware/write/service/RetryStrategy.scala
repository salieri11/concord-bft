// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import java.util.{Timer, TimerTask}

import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try
import scala.util.control.NonFatal

object RetryStrategy {

  private[this] val timer: Timer = new Timer("retry-scheduler", true)

  private class PromiseTask[A](value: => Future[A]) extends TimerTask with Promise[A] {

    private[this] val p = Promise[A]

    override def run(): Unit = {
      p.tryCompleteWith {
        try value
        catch { case NonFatal(t) => Future.failed(t) }
      }
    }

    override def future: Future[A] = p.future

    override def isCompleted: Boolean = p.isCompleted

    override def tryComplete(result: Try[A]): Boolean = p.tryComplete(result)
  }

  private def after[T](t: Duration)(value: => Future[T])(implicit ec: ExecutionContext): Future[T] =
    if (!t.isFinite) {
      Future.failed(new IllegalArgumentException(s"Cannot schedule task after $t"))
    } else if (t.length < 1) {
      try value
      catch { case NonFatal(e) => Future.failed(e) }
    } else {
      val task = new PromiseTask(value)
      timer.schedule(task, t.toMillis)
      task.future
    }

  def exponentialBackoff(
      shouldRetry: Throwable => Boolean,
      retries: Int,
      firstWaitTime: Duration): RetryStrategy =
    new RetryStrategy(
      shouldRetry,
      retries,
      firstWaitTime,
      exponentialBackoffWaitTimeCap(retries, firstWaitTime),
      exponentialBackoffProgression)

  def exponentialBackoffProgression(duration: Duration): Duration =
    duration * ExponentialBackoffMultiplier

  def exponentialBackoffWaitTimeCap(retries: Int, firstWaitTime: Duration): Duration =
    firstWaitTime * math.pow(ExponentialBackoffMultiplier.toDouble, retries.toDouble)

  def constant(shouldRetry: Throwable => Boolean, retries: Int, waitTime: Duration): RetryStrategy =
    new RetryStrategy(shouldRetry, retries, waitTime, waitTime, identity)

  // The following predicates are accurate only under the assumption that 'progression' is stateless (as it should)

  def isConstant(retryStrategy: RetryStrategy): Boolean =
    isProgressionConstant(retryStrategy.progression)

  def isProgressionConstant(progression: Duration => Duration): Boolean =
    progression(1.milli) == 1.milli

  def isExponential(retryStrategy: RetryStrategy): Boolean =
    retryStrategy.progression(1.milli) == ExponentialBackoffMultiplier.millis

  val ExponentialBackoffMultiplier = 2

}

final case class RetryStrategy(
    shouldRetry: Throwable => Boolean,
    retries: Int,
    firstWaitTime: Duration,
    waitTimeCap: Duration,
    progression: Duration => Duration,
) {

  import RetryStrategy.after

  def apply[A](run: Int => Future[A])(implicit ec: ExecutionContext): Future[A] = {
    def go(attempt: Int, wait: Duration): Future[A] = {
      attemptsCount = attempt
      run(attempt)
        .recoverWith {
          case err =>
            if (attempt <= retries && shouldRetry(err)) {
              synchronized { waits += wait }
              after(wait)(go(attempt + 1, clip(progression(wait))))
            } else {
              Future.failed(err)
            }
        }
    }
    go(1, clip(firstWaitTime))
  }

  def extractTotalWait: Duration = extractWaits.reduceLeftOption(_ plus _).getOrElse(Duration.Zero)

  def getAttemptsCount: Integer = attemptsCount

  private def extractWaits: Seq[Duration] = waits.result()

  private def clip(t: Duration): Duration = t.min(waitTimeCap).max(0.millis)

  @volatile private var attemptsCount: Integer = 0

  private val waits = Vector.newBuilder[Duration]
}
