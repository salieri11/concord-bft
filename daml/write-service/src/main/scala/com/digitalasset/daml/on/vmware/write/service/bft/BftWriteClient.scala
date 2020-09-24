// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.nio.file.Path

import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.kvutils.api.CommitMetadata
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.{CommitRequest, ConcordWriteClient}
import com.digitalasset.kvbc.daml_commit.Command.Cmd
import com.digitalasset.kvbc.daml_commit.{Command, CommitRequest => LegacyCommitRequest}
import com.google.protobuf.ByteString
import com.vmware.concord.concord.{ConcordRequest, DamlRequest}

import scala.concurrent.{ExecutionContext, Future}

/**
  * A [[ConcordWriteClient]] using an asynchronous [[BftConcordClientPool]] to send submissions.
  *
  * @param requestTimeout the function that computes the BFT timeout based on transaction and commit metadata.
  */
class BftWriteClient(
    concordClientPool: BftConcordClientPool,
    requestTimeout: RequestTimeoutFunction)
    extends ConcordWriteClient {

  override def commitTransaction(request: CommitRequest, metadata: CommitMetadata)(
      executionContext: ExecutionContext): Future[SubmissionResult] = {
    implicit val ec: ExecutionContext = executionContext
    val commitRequest = LegacyCommitRequest.of(
      submission = request.submission,
      participantId = request.participantId,
      correlationId = request.correlationId,
      spanContext = ByteString.EMPTY,
      flags = 0)
    val command = Command.of(Cmd.Commit(commitRequest))
    val damlRequest = DamlRequest.of(command.toByteString)
    val concordRequest = new ConcordRequest(damlRequest = Some(damlRequest))
    concordClientPool.sendRequest(
      concordRequest.toByteString,
      requestTimeout(request, metadata),
      preExecute = request.preExecute,
      request.correlationId)
  }

  override def currentHealth: HealthStatus = concordClientPool.currentHealth

  override def close(): Unit = concordClientPool.close()
}

object BftWriteClient {
  def apply(
      configPath: Path,
      requestTimeout: RequestTimeoutFunction,
      sendRetryStrategyFactory: RetryStrategyFactory,
      metrics: Metrics,
  ): BftWriteClient = {
    val concordClientPool =
      new BftConcordClientPool(
        new BftConcordClientPoolJni(configPath),
        sendRetryStrategyFactory,
        metrics)
    new BftWriteClient(concordClientPool, requestTimeout)
  }
}
