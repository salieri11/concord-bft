// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.util.UUID

import akka.stream.Materializer
import com.codahale.metrics.MetricRegistry
import com.daml.buildinfo.BuildInfo
import com.daml.ledger.api.auth.AuthService
import com.daml.ledger.participant.state.kvutils.api.{BatchingLedgerWriter, DefaultBatchingQueue}
import com.daml.ledger.participant.state.kvutils.app.{
  Config,
  LedgerFactory,
  ParticipantConfig,
  ReadWriteService
}
import com.daml.ledger.participant.state.pkvutils.api.PrivacyAwareKeyValueParticipantState
import com.daml.lf.data.Ref
import com.daml.lf.data.Ref.ParticipantId
import com.daml.lf.engine.Engine
import com.daml.logging.LoggingContext
import com.daml.metrics.Metrics
import com.daml.platform.apiserver.ApiServerConfig
import com.daml.resources.ResourceOwner
import com.digitalasset.daml.on.vmware.common.{KVBCHttpServer, KVBCPrometheusMetricsEndpoint}
import com.digitalasset.daml.on.vmware.participant.state.{
  ConcordKeyValueLedgerReader,
  ConcordLedgerWriter
}
import com.digitalasset.daml.on.vmware.read.service.ThinReplicaReadClient
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import com.digitalasset.daml.on.vmware.write.service.bft.BftWriteClient
import com.digitalasset.daml.on.vmware.write.service.kvbc.KvbcWriteClient
import com.google.common.net.HostAndPort
import org.slf4j.LoggerFactory
import scopt.OptionParser

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ConcordLedgerFactory extends LedgerFactory[ReadWriteService, ExtraConfig] {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  override val defaultExtraConfig: ExtraConfig = ExtraConfig.Default

  override def extraConfigParser(parser: OptionParser[Config[ExtraConfig]]): Unit =
    ExtraConfig.addCommandLineArguments(parser)

  // We explicitly set that for all participants we are going to migrate and start the database.
  override def manipulateConfig(config: Config[ExtraConfig]): Config[ExtraConfig] =
    config.copy(participants = config.participants.map(_.copy(allowExistingSchemaForIndex = true)))

  override def readWriteServiceOwner(
      config: Config[ExtraConfig],
      participantConfig: ParticipantConfig,
      engine: Engine,
  )(
      implicit materializer: Materializer,
      logCtx: LoggingContext): ResourceOwner[ReadWriteService] = {
    implicit val executionContext: ExecutionContextExecutor =
      materializer.executionContext
    logger.info(
      s"""Initializing vDAML ledger api server: version=${BuildInfo.Version}
         |participantId=${config.participants.head.participantId} replicas=${config.extra.replicas}
         |jdbcUrl=${config.participants.head.serverJdbcUrl}
         |dar_file(s)=${config.archiveFiles.mkString("(", ";", ")")}
         |use-bft-client=${config.extra.useBftClient}""".stripMargin
        .replaceAll("\n", " "))
    val metrics = createMetrics(participantConfig, config)
    val thinReplicaClient =
      createThinReplicaClient(participantConfig.participantId, config.extra, metrics.registry)
    val concordClients = createConcordWriteClients(config.extra, metrics)
    waitForConcordToBeReady(concordClients, config.extra.maxFaultyReplicas)
    val ledgerId =
      config.ledgerId.getOrElse(Ref.LedgerString.assertFromString(UUID.randomUUID.toString))
    val reader = new ConcordKeyValueLedgerReader(
      thinReplicaClient.committedBlocks,
      ledgerId,
      () => concordClients.head.currentHealth)
    logger.info(s"Connecting to the first core replica ${config.extra.replicas.head}")
    val concordWriter =
      new ConcordLedgerWriter(
        ledgerId,
        participantConfig.participantId,
        concordClients.head.commitTransaction(_)(executionContext),
        () => concordClients.head.currentHealth
      )

    lazy val batchingWriter =
      new BatchingLedgerWriter(
        DefaultBatchingQueue(
          maxQueueSize = config.extra.maxBatchQueueSize,
          maxBatchSizeBytes = config.extra.maxBatchSizeBytes,
          maxWaitDuration = config.extra.maxBatchWaitDuration,
          maxConcurrentCommits = config.extra.maxBatchConcurrentCommits,
        ),
        concordWriter
      )

    val writer =
      if (config.extra.enableBatching) {
        batchingWriter
      } else {
        concordWriter
      }

    val participantState = PrivacyAwareKeyValueParticipantState(reader, writer, metrics)

    for {
      closeableHttpServer <- ResourceOwner.forCloseable(() => new KVBCHttpServer())
      closeableMetricsEndpoint = {
        val metricsEndPoint = KVBCPrometheusMetricsEndpoint.createEndpoint(
          metrics.registry,
          closeableHttpServer.context)
        closeableHttpServer.start()
        metricsEndPoint
      }
      _ <- ResourceOwner.forCloseable(() => closeableMetricsEndpoint)
    } yield participantState
  }

  override def apiServerConfig(
      participantConfig: ParticipantConfig,
      config: Config[ExtraConfig]): ApiServerConfig =
    super
      .apiServerConfig(participantConfig, config)
      .copy(tlsConfig = config.tlsConfig)

  override def authService(config: Config[ExtraConfig]): AuthService =
    config.extra.authService.getOrElse(super.authService(config))

  private[this] val DefaultReplicaPort = 50051

  private[this] def createConcordWriteClients(config: ExtraConfig, metrics: Metrics)(
      implicit executionContext: ExecutionContext): Seq[ConcordWriteClient] =
    if (config.useBftClient) {
      Seq(
        BftWriteClient(
          config.bftClientConfigPath.getOrElse {
            sys.error(
              "When BFT Client is selected, the BFT Client configuration file path is required but none was specified.")
          },
          config.bftClientRequestTimeout,
          metrics
        ))
    } else {
      assert(config.replicas.nonEmpty)
      config.replicas.map { replica =>
        val (host, port) = parseHostAndPort(replica)
        KvbcWriteClient(host, port)
      }
    }

  private[this] def createThinReplicaClient(
      participantId: ParticipantId,
      config: ExtraConfig,
      metricRegistry: MetricRegistry): ThinReplicaReadClient = {
    if (!config.useThinReplica) {
      throw new IllegalArgumentException("Must have thin replica client enabled")
    }
    // format: off
    new ThinReplicaReadClient(
      participantId,
      config.maxFaultyReplicas,
      privateKey = "",
      config.replicas.toArray,
      config.maxTrcReadDataTimeout,
      config.maxTrcReadHashTimeout,
      config.jaegerAgentAddress,
      metricRegistry)
    // format: on
  }

  private[this] def waitForConcordToBeReady(clients: Seq[ConcordWriteClient], f: Int): Unit = {
    // The first client is used to send requests to and therefore we need a working connection.
    while (!clients.head.ready) {
      logger.info("Waiting for first Concord to be ready")
      Thread.sleep(1000)
    }

    var numReady = 0
    do {
      clients.tail.foreach { client =>
        if (client.ready)
          numReady += 1
      }
      logger.info(
        s"Waiting for 2*f=${2 * f} more Concords to be ready (currently $numReady are ready)")
      if (numReady < (2 * f)) Thread.sleep(1000)
    } while (numReady < (2 * f))
  }

  private[this] def parseHostAndPort(input: String): (String, Int) = {
    val hostAndPort =
      HostAndPort.fromString(input).withDefaultPort(DefaultReplicaPort)
    (hostAndPort.getHost, hostAndPort.getPort)
  }
}
