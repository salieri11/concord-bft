// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.kvutils.api.CommitMetadata
import com.daml.ledger.participant.state.v1.SubmissionResult

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
}

object ConcordWriteClient {
  def markRequestForPreExecution(
      delegate: (CommitRequest, CommitMetadata) => Future[SubmissionResult])(
      request: CommitRequest,
      commitMetadata: CommitMetadata): Future[SubmissionResult] = {
    val flaggedRequest = request.copy(preExecute = true)
    delegate(flaggedRequest, commitMetadata)
  }
}
