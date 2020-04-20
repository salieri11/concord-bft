// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service

import com.digitalasset.kvbc.daml_commit._
import com.daml.grpc.adapter.ExecutionSequencerFactory
import com.daml.grpc.adapter.client.akka._
import com.daml.ledger.api.health.{HealthStatus, Healthy, Unhealthy}
import io.grpc.{
  ManagedChannel,
  ManagedChannelBuilder,
  ConnectivityState,
  Status,
  StatusRuntimeException
}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import akka.NotUsed
import akka.stream.scaladsl.Source

/**
  * Raw client for the KVBC API
  */
class KVBCClient private (host: String, port: Int)(implicit val ec: ExecutionContext) {
  private val ledgerInboundMessageSizeMax: Int = 50 * 1024 * 1024 // 50 MiBytes

  private val logger = LoggerFactory.getLogger(this.getClass)

  val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(ledgerInboundMessageSizeMax)
      .usePlaintext()
      .build

  val commitClient: CommitServiceGrpc.CommitServiceStub = CommitServiceGrpc.stub(channel)
  def shouldRetry(err: Throwable): Boolean = {
    err match {
      case sre: StatusRuntimeException
          if (sre.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED) =>
        true
      case _ => false
    }
  }
  val backOff: RetryStrategy = RetryStrategy.exponentialBackoff(shouldRetry, 10, 100.milliseconds)

  @volatile
  private var channelHealth: HealthStatus = Healthy

  private val statusCallback: Runnable = { () =>
    val currentState = channel.getState(false)
    if (channelHealth == Healthy)
      channelHealth =
        if (currentState == ConnectivityState.TRANSIENT_FAILURE) Unhealthy else Healthy
    else
      channelHealth = if (currentState == ConnectivityState.READY) Healthy else Unhealthy
    logger.info(s"gRPC state changed $currentState")
    channel.notifyWhenStateChanged(currentState, statusCallback)
  }

  channel.notifyWhenStateChanged(channel.getState(false), statusCallback)

  def currentHealth: HealthStatus = channelHealth

  //
  // Commit API
  //
  def commitTransaction(req: CommitRequest): Future[CommitResponse] =
    backOff(n => commitClient.commitTransaction(req))

}

object KVBCClient {
  def apply(host: String, port: Int)(implicit ec: ExecutionContext): KVBCClient =
    new KVBCClient(host, port)
}
