// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.bft

import java.nio.file.Path

import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.kvbc.daml_commit.Command.Cmd
import com.digitalasset.kvbc.daml_commit.{Command, CommitRequest}
import com.vmware.concord.concord.{ConcordRequest, DamlRequest}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

class BftWriteClient(configPath: Path, requestTimeout: Duration, metrics: Metrics)
    extends ConcordWriteClient {

  private[this] val concordClientPool =
    new BftConcordClientPool(new BftConcordClientPoolNative(configPath, metrics), metrics)

  override def commitTransaction(request: CommitRequest)(
      executionContext: ExecutionContext): Future[SubmissionResult] = {
    implicit val ec: ExecutionContext = executionContext
    val command = Command.of(Cmd.Commit(request))
    //noinspection SpellCheckingInspection
    val damlRequest = DamlRequest.of(command.toByteString)
    val concordRequest = new ConcordRequest(damlRequest = Some(damlRequest))
    concordClientPool.sendRequest(
      concordRequest.toByteString,
      requestTimeout,
      ClientMsgFlag.None, // Flags will only be used with pre-execution
      request.correlationId)
  }

  override def currentHealth: HealthStatus = concordClientPool.currentHealth

  override def ready: Boolean = currentHealth == HealthStatus.healthy

  override def close(): Unit = concordClientPool.close()
}

object BftWriteClient {
  def apply(configPath: Path, requestTimeout: Duration, metrics: Metrics): BftWriteClient =
    new BftWriteClient(configPath, requestTimeout, metrics)
}
