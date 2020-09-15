// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.daml.ledger.participant.state.kvutils.api.SimpleCommitMetadata
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class RequestTimeoutStrategySpec extends WordSpec with Matchers {
  "linear transform function" should {
    "assume estimated interpretation cost to be nanoseconds" in {
      val instance = LinearAffineInterpretationCostTransform(
        slope = 1.0,
        intercept = 0.0,
        defaultTimeout = 30.seconds)
      val inputNanos = 123.millis.toNanos
      val commitMetadata = SimpleCommitMetadata(Some(inputNanos))

      instance.calculate(commitMetadata) shouldBe 123.millis
    }

    "return default time-out in case no interpretation cost is available" in {
      val instance = LinearAffineInterpretationCostTransform(
        slope = 1.0,
        intercept = 0.0,
        defaultTimeout = 123.seconds)
      val noCommitMetadata = SimpleCommitMetadata(None)

      instance.calculate(noCommitMetadata) shouldBe 123.seconds
    }
  }

}
