// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import com.daml.ledger.participant.state.kvutils.api.CommitMetadata

import scala.concurrent.duration._

/**
  * Produces a BFT client request timeout.
  */
sealed trait RequestTimeoutStrategy {

  def calculate(metadata: CommitMetadata): Duration

  /**
    * The BFT client request timeout that will be used if not pre-executing or if the interpretation cost is missing.
    */
  def defaultTimeout: Duration

  /**
    * @return A new [[RequestTimeoutStrategy]] with the same behavior, except for the default timeout.
    */
  def withDefaultTimeout(defaultTimeout: Duration): RequestTimeoutStrategy
}

/**
  * Produces a BFT client request timeout by applying a linear-affine transform to the estimated interpretation cost.
  */
case class LinearAffineInterpretationCostTransform(
    slope: Double,
    intercept: Duration,
    defaultTimeout: Duration,
) extends RequestTimeoutStrategy {

  override def calculate(metadata: CommitMetadata): Duration =
    metadata.estimatedInterpretationCost
      .map(_.nanos)
      .map(cost => cost * slope + intercept)
      .getOrElse(defaultTimeout)

  override def withDefaultTimeout(
      defaultTimeout: Duration): LinearAffineInterpretationCostTransform =
    copy(defaultTimeout = defaultTimeout)
}

object LinearAffineInterpretationCostTransform {
  val ReasonableDefault: LinearAffineInterpretationCostTransform =
    LinearAffineInterpretationCostTransform(
      // The current working assumption is: give a BFT client twice the estimation for the request to complete;
      // add 5s for the fixed costs, e.g. network, consensus, post-execution.
      slope = 2.0,
      intercept = 5.second,
      defaultTimeout = 30.seconds,
    )
}

/**
  * Produces a constant BFT client request timeout.
  */
case class ConstantRequestTimeout(defaultTimeout: Duration) extends RequestTimeoutStrategy {

  override def calculate(metadata: CommitMetadata): Duration =
    defaultTimeout

  override def withDefaultTimeout(defaultTimeout: Duration): ConstantRequestTimeout =
    copy(defaultTimeout = defaultTimeout)
}

object ConstantRequestTimeout {
  val ReasonableDefault: ConstantRequestTimeout = ConstantRequestTimeout(30.seconds)
}
