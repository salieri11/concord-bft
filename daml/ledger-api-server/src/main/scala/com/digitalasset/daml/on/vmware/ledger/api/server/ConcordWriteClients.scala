// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.Path

import com.daml.ledger.api.health.Healthy
import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.daml.on.vmware.write.service.bft.{
  BftWriteClient,
  RequestTimeoutFunction,
  RequestTimeoutStrategy,
  RetryStrategyFactory
}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

private[server] object ConcordWriteClients {

  private[this] val DefaultReplicaPort = 50051

  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  def createAndInitializeConcordWriteClient(config: WriteClientsConfig, metrics: Metrics)(
      implicit executionContext: ExecutionContext): ConcordWriteClient = {
    val concordWriteClient =
      createConcordWriteClient(
        config,
        createBftWriteClient,
        config.bftClient.requestTimeoutStrategy,
        () => config.bftClient.sendRetryStrategy,
        metrics)
    waitForConcordWriteClientsToBeReady(Seq(concordWriteClient), clientsToBeWaitedFor = 1)
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

  private[this] val WaitForConcordToBeReadyAttempts: Int = 30
  private[this] val WaitForConcordToBeReadySleepMillis: Int = 1000

  private[server] def waitForConcordWriteClientsToBeReady(
      concordWriteClients: Seq[ConcordWriteClient],
      writeClientLabel: String = "primary",
      clientsToBeWaitedFor: Int,
      attempts: Int = WaitForConcordToBeReadyAttempts,
      sleepMillis: Int = WaitForConcordToBeReadySleepMillis,
  ): Unit = {
    var remainingAttempts = attempts

    def missingReadyWriteClients: Stream[Int] = {
      if (remainingAttempts <= 0) {
        sys.error(s"""Couldn't connect to enough replicas in $attempts attempts. Please check that
             |at least $clientsToBeWaitedFor $writeClientLabel replicas are healthy and restart the
             |ledger API server.
             |""".stripMargin)
      } else {
        val ready = concordWriteClients.count(_.currentHealth == Healthy)
        if (ready < clientsToBeWaitedFor) {
          remainingAttempts -= 1
          (clientsToBeWaitedFor - ready) #:: missingReadyWriteClients
        } else {
          Stream.empty
        }
      }
    }

    for (numMissing <- missingReadyWriteClients) {
      logger.info(
        s"Waiting for $clientsToBeWaitedFor $writeClientLabel Concord replicas to be ready for writing (missing: $numMissing)")
      Thread.sleep(sleepMillis)
    }
  }
}
