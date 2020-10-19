// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.daml.ledger.participant.state.kvutils.DamlKvutils.DamlSubmission
import com.daml.ledger.participant.state.kvutils.api.CommitMetadata
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class RequestTimeoutStrategySpec extends WordSpec with Matchers {
  "linear transform function" should {
    "assume estimated interpretation time to be nanoseconds" in {
      val instance = LinearAffineInterpretationTimeTransform(
        slope = 1.0,
        intercept = 0.second,
        defaultTimeout = 30.seconds)

      instance.calculate(aCommitMetadata) shouldBe 123.millis
    }

    "calculate time-out based on slope and intercept" in {
      val instance = LinearAffineInterpretationTimeTransform(
        slope = 2.0,
        intercept = 3.second,
        defaultTimeout = 30.seconds)

      instance.calculate(aCommitMetadata) shouldBe (123.millis * 2.0 + 3.second)
    }

    "return default time-out in case no interpretation time is available" in {
      val instance = LinearAffineInterpretationTimeTransform(
        slope = 1.0,
        intercept = 0.second,
        defaultTimeout = 123.seconds)

      instance.calculate(CommitMetadata.Empty) shouldBe 123.seconds
    }
  }

  private def aCommitMetadata =
    CommitMetadata(DamlSubmission.getDefaultInstance, Some(inputNanos))

  private def inputNanos = 123.millis.toNanos
}
