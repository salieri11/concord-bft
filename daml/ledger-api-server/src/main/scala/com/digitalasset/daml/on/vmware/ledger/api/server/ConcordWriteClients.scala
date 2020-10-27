// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.Path

import com.daml.ledger.api.health.Healthy
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.bft.{
  BftWriteClient,
  RequestTimeoutFunction,
  RequestTimeoutStrategy,
  RetryStrategyFactory
}
import com.digitalasset.daml.on.vmware.write.service.{ConcordWriteClient, RetryStrategy}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

private[server] object ConcordWriteClients {

  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  def createAndInitializeConcordWriteClient(config: WriteClientsConfig, metrics: Metrics)(
      implicit executionContext: ExecutionContext): ConcordWriteClient = {
    val concordWriteClient =
      createConcordWriteClient(
        config,
        createBftWriteClient,
        config.bftClient.requestTimeoutStrategy,
        () => config.bftClient.sendRetryStrategy,
        metrics,
      )
    waitForConcordWriteClientsToBeReady(
      Seq(concordWriteClient),
      config.bftClient.initRetryStrategy,
    )
    concordWriteClient
  }

  private[server] def createConcordWriteClient(
      config: WriteClientsConfig,
      bftWriteClientFactory: (
          Path,
          RequestTimeoutFunction,
          RetryStrategyFactory,
          Metrics,
      ) => BftWriteClient,
      requestTimeoutStrategy: RequestTimeoutStrategy,
      sendRetryStrategyFactory: RetryStrategyFactory,
      metrics: Metrics,
  )(implicit executionContext: ExecutionContext): ConcordWriteClient = {
    val result = bftWriteClientFactory(
      config.bftClient.configPath,
      computeTimeoutIfPreExecutingElseDefault(requestTimeoutStrategy),
      sendRetryStrategyFactory,
      metrics,
    )
    logger.debug("BFT Client created")
    result
  }

  private[server] def computeTimeoutIfPreExecutingElseDefault(
      requestTimeoutStrategy: RequestTimeoutStrategy): RequestTimeoutFunction =
    (request, metadata) =>
      if (request.preExecute) {
        requestTimeoutStrategy.calculate(metadata)
      } else {
        requestTimeoutStrategy.defaultTimeout
    }

  private def createBftWriteClient(
      configPath: Path,
      requestTimeoutFunction: RequestTimeoutFunction,
      sendRetryStrategyFactory: RetryStrategyFactory,
      metrics: Metrics,
  ) = {
    logger.debug("Loading the native 'bft-client-native0' library")
    System.loadLibrary("bft-client-native0")
    logger.debug("Creating the BFT Client")

    BftWriteClient(
      configPath,
      requestTimeoutFunction,
      sendRetryStrategyFactory,
      metrics,
    )
  }

  private[server] def waitForConcordWriteClientsToBeReady(
      concordWriteClients: Seq[ConcordWriteClient],
      retryStrategy: RetryStrategy,
      writeClientLabel: String = "primary",
  ): Unit = {
    val clientsToBeWaitedFor = concordWriteClients.size
    val totalAttempts = retryStrategy.retries + 1

    var remainingAttempts = totalAttempts
    var waitTime: Option[Duration] = None

    def nextWait(waitTime: Option[Duration]): Duration =
      waitTime match {
        case None => retryStrategy.firstWaitTime
        case Some(duration) => retryStrategy.nextWait(duration)
      }

    def missingReadyWriteClientsAndWaitTime: Stream[(Int, Duration)] = {
      if (remainingAttempts <= 0) {
        sys.error(
          s"""Couldn't connect to enough replicas in $totalAttempts attempts. Please check that
             |at least $clientsToBeWaitedFor $writeClientLabel replicas are healthy and restart the
             |ledger API server.
             |""".stripMargin)
      } else {
        val ready = concordWriteClients.count(_.currentHealth == Healthy)
        if (ready < clientsToBeWaitedFor) {
          remainingAttempts -= 1
          val nextWaitTime = nextWait(waitTime)
          waitTime = Some(nextWaitTime)
          (clientsToBeWaitedFor - ready -> nextWaitTime) #:: missingReadyWriteClientsAndWaitTime
        } else {
          Stream.empty
        }
      }
    }

    for ((numMissing, waitTime) <- missingReadyWriteClientsAndWaitTime) {
      logger.info(
        s"Waiting $waitTime for $clientsToBeWaitedFor $writeClientLabel Concord replicas " +
          s"to be ready for writing (missing: $numMissing)")
      Thread.sleep(waitTime.toMillis)
    }
  }
}
