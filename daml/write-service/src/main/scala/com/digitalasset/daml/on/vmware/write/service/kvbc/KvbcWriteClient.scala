// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.write.service.kvbc

import com.daml.ledger.api.health.{HealthStatus, Healthy, Unhealthy}
import com.daml.ledger.participant.state.kvutils.api.CommitMetadata
import com.daml.ledger.participant.state.v1.SubmissionResult
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient.backOff
import com.digitalasset.daml.on.vmware.write.service.kvbc.KvbcWriteClient._
import com.digitalasset.kvbc.daml_commit._
import io.grpc._
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/**
  * Raw client for the KVBC API
  */
class KvbcWriteClient private[kvbc] (
    commitClient: CommitServiceGrpc.CommitServiceStub,
    readyReporter: () => Boolean,
    shouldRetry: Throwable => Boolean = KvbcWriteClient.shouldRetry
)(implicit val ec: ExecutionContext)
    extends ConcordWriteClient {

  @volatile
  private var channelHealth: HealthStatus = Healthy

  override def currentHealth: HealthStatus = channelHealth

  override def commitTransaction(request: CommitRequest, metadata: CommitMetadata)(
      ec: ExecutionContext): Future[SubmissionResult] = {
    implicit val executionContext: ExecutionContext = ec
    val correlationId = request.correlationId
    backOff(shouldRetry) { _ =>
      commitClient
        .commitTransaction(request)
    }.map {
        case response if response.status == CommitResponse.CommitStatus.OK =>
          SubmissionResult.Acknowledged
        case response =>
          val responseStatus = response.status.toString
          SubmissionResult.InternalError(responseStatus)
      }
      .recover {
        case grpc: StatusRuntimeException
            if grpc.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED =>
          logger.error(
            s"Submission failed with an exception, correlationId=$correlationId exception='$grpc'")
          SubmissionResult.Overloaded
        case NonFatal(exception) =>
          logger.error(
            s"Submission failed with an exception, correlationId=$correlationId exception='$exception'")
          SubmissionResult.InternalError(exception.toString)
      }
  }

  override def ready: Boolean = readyReporter()

  override def close(): Unit = ()
}

object KvbcWriteClient {

  def apply(host: String, port: Int)(implicit ec: ExecutionContext): KvbcWriteClient = {
    val channel = createManagedChannel(host, port)
    val commitClient: CommitServiceGrpc.CommitServiceStub = CommitServiceGrpc.stub(channel)
    val client =
      new KvbcWriteClient(commitClient, () => channel.getState(true) == ConnectivityState.READY)

    channel.notifyWhenStateChanged(channel.getState(false), statusCallback(channel, client))

    client
  }

  //noinspection ConvertExpressionToSAM
  private[this] def statusCallback(channel: ManagedChannel, client: KvbcWriteClient): Runnable =
    new Runnable() {
      override def run(): Unit = {
        val currentState = channel.getState(false)
        client.channelHealth = (client.channelHealth, currentState) match {
          case (Healthy, ConnectivityState.TRANSIENT_FAILURE) => Unhealthy
          case (Healthy, _) => Healthy
          case (_, ConnectivityState.READY) => Healthy
          case _ => Unhealthy
        }
        logger.info(s"gRPC state changed $currentState")
        channel.notifyWhenStateChanged(currentState, this)
      }
    }

  private def createManagedChannel(host: String, port: Int): ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(LedgerInboundMessageSizeMax)
      .usePlaintext()
      .build

  private def shouldRetry(err: Throwable): Boolean =
    PartialFunction.cond(err) {
      case sre: StatusRuntimeException if sre.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED =>
        true
    }

  private val LedgerInboundMessageSizeMax: Int = 50 * 1024 * 1024 // 50 MiBytes

  private val logger = LoggerFactory.getLogger(this.getClass)
}
