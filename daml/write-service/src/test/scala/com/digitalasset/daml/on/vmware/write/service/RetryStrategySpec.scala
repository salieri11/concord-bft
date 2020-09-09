// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Assertion, AsyncWordSpec, Matchers, Succeeded}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future
import scala.concurrent.duration._

class RetryStrategySpec
    extends AsyncWordSpec
    with Matchers
    with MockitoSugar
    with TableDrivenPropertyChecks {

  "RetryStrategy" should {

    "observe 'retries' when retrying" in {
      @volatile var attempts = 0
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.milli, 2.milli, identity)
      retryStrategy { n =>
        Future {
          attempts = n
          throw new RuntimeException
        }
      }.failed.map { e =>
        e shouldBe a[RuntimeException]
        attempts shouldBe 3
      }
    }

    "count attempts when retrying" in {
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.milli, 2.milli, identity)
      retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
        retryStrategy.getAttemptsCount shouldBe 3
      }
    }

    "propagate failure after retrying" in {
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.milli, 2.milli, identity)
      retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
        e shouldBe a[RuntimeException]
      }
    }

    "propagate success" in {
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.milli, 2.milli, identity)
      retryStrategy(_ =>
        Future.unit.map { _ =>
          succeed
      })
    }

    "record waits when retrying" in {
      val retryStrategy =
        RetryStrategy(_ => true, 1, 1.milli, 2.milli, identity)
      retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
        e shouldBe a[RuntimeException]
        retryStrategy.extractTotalWait shouldBe 1.millis
      }
    }

    "observe 'progression' when retrying" in {
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.milli, 2.milli, identity)
      retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
        e shouldBe a[RuntimeException]
        retryStrategy.extractTotalWait shouldBe 2.millis
      }
    }

    "observe 'waitTimeCap' when retrying" in {
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.minute, Duration.Zero, identity)
      retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
        retryStrategy.getAttemptsCount shouldBe 3
        retryStrategy.extractTotalWait shouldBe Duration.Zero
      }
    }

    "avoid retrying when succeeding" in {
      val retryStrategy =
        RetryStrategy(_ => true, 2, 1.milli, 2.milli, identity)
      retryStrategy(_ =>
        Future.unit.map { _ =>
          retryStrategy.getAttemptsCount shouldBe 1
          retryStrategy.extractTotalWait shouldBe Duration.Zero
      })
    }

    "avoid retrying if retries is <= 0" in {
      val retryValuesToBeTested = Seq(0, -1)
      allOK(
        retryValuesToBeTested
          .map { retryValueToBeTested =>
            val retryStrategy =
              RetryStrategy(_ => true, retryValueToBeTested, 1.minute, 2.minutes, identity)
            retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
              e shouldBe a[RuntimeException]
              retryStrategy.getAttemptsCount shouldBe 1
            }
          }
      )
    }

    "avoid retrying if shouldRetry is always false" in {
      val retryStrategy =
        RetryStrategy(_ => false, 10, 1.minute, 2.minutes, identity)
      retryStrategy(_ => Future.failed(new RuntimeException)).failed.map { e =>
        e shouldBe a[RuntimeException]
        retryStrategy.getAttemptsCount shouldBe 1
      }
    }
  }

  private def allOK(eventualAssertions: Seq[Future[Assertion]]): Future[Assertion] = {
    Future
      .sequence(eventualAssertions)
      .map(listOfAssertions => assert(listOfAssertions.forall(_ == Succeeded)))
  }
}
