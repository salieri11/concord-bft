// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.kvutils.api.CommitMetadata
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.digitalasset.kvbc.daml_commit.CommitRequest

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

/**
  * Implementations of this trait allow to asynchronously submit a [[CommitRequest]] to the Concord
  * ledger and must be thread-safe.
  * They also provide resource management, health check and initialization status information.
  */
trait ConcordWriteClient extends AutoCloseable {

  /**
    * Asynchronously commits a submission to Concord.
    *
    * @param request The [[CommitRequest]] containing a serialized submission.
    * @param metadata The [[CommitMetadata]] containing metadata about the commit operation.
    */
  def commitTransaction(request: CommitRequest, metadata: CommitMetadata)(
      executionContext: ExecutionContext): Future[SubmissionResult]

  /**
    * This function will be polled at regular intervals and needs to return immediately.
    */
  def currentHealth: HealthStatus

  /**
    * This function will be called at initialization time and needs to return immediately.
    */
  def ready: Boolean
}

object ConcordWriteClient {
  // The below flags may be used for 'flags' field in [[CommitRequest]].
  object MessageFlags {
    val EmptyFlag = 0
    val ReadOnlyFlag = 1
    val PreExecuteFlag = 2
  }

  def markRequestForPreExecution(
      delegate: (CommitRequest, CommitMetadata) => Future[SubmissionResult])(
      request: CommitRequest,
      commitMetadata: CommitMetadata): Future[SubmissionResult] = {
    val flaggedRequest = request.copy(
      flags = request.flags | MessageFlags.PreExecuteFlag
    )
    delegate(flaggedRequest, commitMetadata)
  }

  private[service] def backOff(shouldRetry: Throwable => Boolean): RetryStrategy =
    RetryStrategy.exponentialBackoff(shouldRetry, attempts = 10, firstWaitTime = 100.milliseconds)
}
