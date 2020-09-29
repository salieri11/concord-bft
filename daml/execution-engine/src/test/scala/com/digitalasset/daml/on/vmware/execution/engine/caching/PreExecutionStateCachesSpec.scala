package com.digitalasset.daml.on.vmware.execution.engine.caching

import com.codahale.metrics.MetricRegistry
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ValidatorCacheMetrics
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.{ExecutionContext, Future}

class PreExecutionStateCachesSpec extends AsyncWordSpec with Matchers {
  "createDefault" should {
    "work on multiple threads for the same metrics instance" in {
      val validatorCacheMetrics = new ValidatorCacheMetrics(new MetricRegistry)
      implicit val executionContext: ExecutionContext = ExecutionContext.global
      val instances =
        (1 to 1000).map(_ => Future(PreExecutionStateCaches.createDefault(validatorCacheMetrics)))
      Future.sequence(instances).map { _ =>
        succeed
      }
    }
  }
}
