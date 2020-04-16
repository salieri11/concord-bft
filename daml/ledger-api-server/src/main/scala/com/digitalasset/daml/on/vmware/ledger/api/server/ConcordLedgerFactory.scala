package com.digitalasset.daml.on.vmware.ledger.api.server

import java.util.UUID

import akka.stream.Materializer
import com.daml.ledger.participant.state.kvutils.app.{Config, LedgerFactory, ParticipantConfig, ReadWriteService}
import com.daml.ledger.participant.state.pkvutils.api.PrivacyAwareKeyValueParticipantState
import com.daml.lf.data.Ref
import com.daml.lf.data.Ref.ParticipantId
import com.digitalasset.daml.on.vmware.common.{KVBCHttpServer, KVBCPrometheusMetricsEndpoint}
import com.digitalasset.daml.on.vmware.participant.state.{ConcordKeyValueLedgerReader, ConcordLedgerWriter}
import com.digitalasset.daml.on.vmware.write.service.{KVBCClient, TRClient}
import com.daml.ledger.api.auth.AuthService
import com.daml.logging.LoggingContext
import com.daml.platform.apiserver.ApiServerConfig
import com.daml.resources.ResourceOwner
import com.google.common.net.HostAndPort
import io.grpc.ConnectivityState
import org.slf4j.LoggerFactory
import scopt.OptionParser
import com.codahale.metrics.MetricRegistry
import com.daml.ledger.participant.state.kvutils.api.{BatchingLedgerWriter, DefaultBatchingQueue}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ConcordLedgerFactory
    extends LedgerFactory[ReadWriteService, ExtraConfig] {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  override val defaultExtraConfig: ExtraConfig = ExtraConfig.Default

  override def extraConfigParser(parser: OptionParser[Config[ExtraConfig]]): Unit =
    ExtraConfig.addCommandLineArguments(parser)

  // We explicitly set that for all participants we are going to migrate and start the database.
  override def manipulateConfig(config: Config[ExtraConfig]): Config[ExtraConfig] =
    config.copy(participants = config.participants.map(_.copy(allowExistingSchemaForIndex = true)))

  def readWriteServiceOwner(
      config: Config[ExtraConfig],
      participantConfig: ParticipantConfig,
  )(implicit materializer: Materializer,
    logCtx: LoggingContext): ResourceOwner[ReadWriteService] = {
    implicit val executionContext: ExecutionContextExecutor =
      materializer.executionContext
    logger.info(
      s"""Initializing vDAML ledger api server: version=${BuildInfo.Version}
         |participantId=${config.participants.head.participantId} replicas=${config.extra.replicas}
         |jdbcUrl=${config.participants.head.serverJdbcUrl}
         |dar_file(s)=${config.archiveFiles.mkString("(", ";", ")")}""".stripMargin
        .replaceAll("\n", " "))
    val thinReplicaClient =
      createThinReplicaClient(participantConfig.participantId, config.extra)
    val concordClient = createConcordClient(config.extra)
    waitForConcordToBeReady(concordClient)
    val ledgerId =
      config.ledgerId.getOrElse(
        Ref.LedgerString.assertFromString(UUID.randomUUID.toString))
    val reader = new ConcordKeyValueLedgerReader(
      thinReplicaClient.committedBlocks,
      ledgerId,
      () => concordClient.currentHealth)
    val concordWriter = new ConcordLedgerWriter(ledgerId,
                                         participantConfig.participantId,
                                         concordClient.commitTransaction,
                                         () => concordClient.currentHealth)

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

    val theMetricRegistry = metricRegistry(participantConfig, config)
    val participantState = PrivacyAwareKeyValueParticipantState(
      reader, writer, theMetricRegistry)


    for {
      closeableHttpServer <- ResourceOwner.forCloseable(() => new KVBCHttpServer())
      closeableMetricsEndpoint = {
        val metricsEndPoint = KVBCPrometheusMetricsEndpoint.createEndpoint(
          metricRegistry(participantConfig, config),
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
      .copy(tlsConfig = config.tlsConfig,
            maxInboundMessageSize = config.extra.maxInboundMessageSize)

  override def authService(config: Config[ExtraConfig]): AuthService =
    config.extra.authService.getOrElse(super.authService(config))

  private[this] val DefaultReplicaPort = 50051

  private[this] def createConcordClient(config: ExtraConfig)(
      implicit executionContext: ExecutionContext): KVBCClient = {
    assert(config.replicas.nonEmpty)
    val (host, port) = parseHostAndPort(config.replicas.head)
    logger.info(s"Connecting to the first core replica ${config.replicas.head}")
    KVBCClient(host, port)
  }

  private[this] def createThinReplicaClient(participantId: ParticipantId,
                                            config: ExtraConfig): TRClient = {
    if (!config.useThinReplica) {
      throw new IllegalArgumentException(
        "Must have thin replica client enabled")
    }
    new TRClient(participantId,
                 config.maxFaultyReplicas,
                 "",
                 config.replicas.toArray,
                 config.jaegerAgentAddress)
  }

  private[this] def waitForConcordToBeReady(client: KVBCClient): Unit = {
    // Make sure the server is ready to receive requests.
    while (client.channel.getState(true) != ConnectivityState.READY) {
      logger.info("Waiting for Concord to be ready")
      Thread.sleep(1000)
    }
  }

  private[this] def parseHostAndPort(input: String): (String, Int) = {
    val hostAndPort =
      HostAndPort.fromString(input).withDefaultPort(DefaultReplicaPort)
    (hostAndPort.getHost, hostAndPort.getPort)
  }
}
