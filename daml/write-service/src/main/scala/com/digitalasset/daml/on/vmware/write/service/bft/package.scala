// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import com.daml.ledger.participant.state.kvutils.api.CommitMetadata

import scala.concurrent.duration.Duration

package object bft {
  type RequestTimeoutFunction = (CommitRequest, CommitMetadata) => Duration
  type RetryStrategyFactory = () => RetryStrategy
}
