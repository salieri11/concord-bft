// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.nio.file.Path

import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.kvutils.api.CommitMetadata
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient.MessageFlags
import com.digitalasset.daml.on.vmware.write.service.bft.BftWriteClient._
import com.digitalasset.kvbc.daml_commit.Command.Cmd
import com.digitalasset.kvbc.daml_commit.{Command, CommitRequest}
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
    val command = Command.of(Cmd.Commit(request))
    val damlRequest = DamlRequest.of(command.toByteString)
    val concordRequest = new ConcordRequest(damlRequest = Some(damlRequest))
    concordClientPool.sendRequest(
      concordRequest.toByteString,
      requestTimeout(request, metadata),
      preExecute = hasPreExecuteFlagSet(request),
      request.correlationId)
  }

  override def currentHealth: HealthStatus = concordClientPool.currentHealth

  override def ready: Boolean = currentHealth == HealthStatus.healthy

  override def close(): Unit = concordClientPool.close()
}

object BftWriteClient {
  def apply(
      configPath: Path,
      requestTimeout: RequestTimeoutFunction,
      metrics: Metrics): BftWriteClient = {
    val concordClientPool =
      new BftConcordClientPool(new BftConcordClientPoolJni(configPath), metrics)
    new BftWriteClient(concordClientPool, requestTimeout)
  }

  def hasPreExecuteFlagSet(request: CommitRequest): Boolean =
    (request.flags & MessageFlags.PreExecuteFlag) != 0
}
