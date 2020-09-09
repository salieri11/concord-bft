// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.Path

import com.daml.metrics.Metrics
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.daml.on.vmware.write.service.bft.{
  BftWriteClient,
  RequestTimeoutFunction,
  RequestTimeoutStrategy,
  RetryStrategyFactory
}
import com.digitalasset.daml.on.vmware.write.service.kvbc.KvbcWriteClient
import com.google.common.net.HostAndPort
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

private[server] object ConcordWriteClients {

  private[server] case class PrimarySecondaryConcordWriteClients(
      primaryWriteClient: ConcordWriteClient,
      secondaryKvbcWriteClients: Option[Seq[KvbcWriteClient]])

  private[this] val DefaultReplicaPort = 50051

  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  def createAndInitializeConcordWriteClient(config: WriteClientsConfig, metrics: Metrics)(
      implicit executionContext: ExecutionContext): ConcordWriteClient = {
    val concordClients =
      createConcordWriteClients(
        config,
        createBftWriteClient,
        config.bftClient.requestTimeoutStrategy,
        () => config.bftClient.sendRetryStrategy,
        createKvbcWriteClient,
        metrics)
    waitForConcordWriteClientsToBeReady(
      Seq(concordClients.primaryWriteClient),
      clientsToBeWaitedFor = 1)
    concordClients.secondaryKvbcWriteClients.foreach(
      waitForConcordWriteClientsToBeReady(
        _,
        writeClientLabel = "secondary",
        clientsToBeWaitedFor = 2 * config.maxFaultyReplicas))
    concordClients.primaryWriteClient
  }

  private[server] def createConcordWriteClients(
      config: WriteClientsConfig,
      bftWriteClientFactory: (
          Option[Path],
          RequestTimeoutFunction,
          RetryStrategyFactory,
          Metrics,
      ) => BftWriteClient,
      requestTimeoutStrategy: RequestTimeoutStrategy,
      sendRetryStrategyFactory: RetryStrategyFactory,
      kvbcWriteClientFactory: String => KvbcWriteClient,
      metrics: Metrics,
  )(implicit executionContext: ExecutionContext): PrimarySecondaryConcordWriteClients =
    if (config.bftClient.enable) {
      val result = PrimarySecondaryConcordWriteClients(
        bftWriteClientFactory(
          config.bftClient.configPath,
          computeTimeoutIfPreExecutingElseDefault(requestTimeoutStrategy),
          sendRetryStrategyFactory,
          metrics,
        ),
        None,
      )
      logger.debug("BFT Client created")
      result
    } else {
      assert(config.replicas.nonEmpty)

      PrimarySecondaryConcordWriteClients(
        kvbcWriteClientFactory(config.replicas.head),
        Some(config.replicas.tail.map(createKvbcWriteClient)),
      )
    }

  private[server] def computeTimeoutIfPreExecutingElseDefault(
      requestTimeoutStrategy: RequestTimeoutStrategy): RequestTimeoutFunction =
    (request, metadata) =>
      if (BftWriteClient.hasPreExecuteFlagSet(request))
        requestTimeoutStrategy.calculate(metadata)
      else requestTimeoutStrategy.defaultTimeout

  private def createBftWriteClient(
      configPath: Option[Path],
      requestTimeoutFunction: RequestTimeoutFunction,
      sendRetryStrategyFactory: RetryStrategyFactory,
      metrics: Metrics,
  ) = {
    logger.debug("Loading the native 'bft-client-native0' library")
    System.loadLibrary("bft-client-native0")
    logger.debug("Creating the BFT Client")

    BftWriteClient(
      configPath.getOrElse {
        sys.error(
          "When BFT Client is selected, the BFT Client configuration file path is required but none was specified.")
      },
      requestTimeoutFunction,
      sendRetryStrategyFactory,
      metrics,
    )
  }

  private[this] def createKvbcWriteClient(replicaHostAndPortString: String)(
      implicit executionContext: ExecutionContext): KvbcWriteClient = {
    val (host, port) = parseHostAndPort(replicaHostAndPortString)
    KvbcWriteClient(host, port)
  }

  private[this] def parseHostAndPort(input: String): (String, Int) = {
    val hostAndPort =
      HostAndPort.fromString(input).withDefaultPort(DefaultReplicaPort)
    (hostAndPort.getHost, hostAndPort.getPort)
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
        val ready = concordWriteClients.count(_.ready)
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
