package com.digitalasset.daml.on.vmware.ledger.api.server

import java.util.UUID

import akka.stream.Materializer
import com.daml.ledger.participant.state.kvutils.app.{LedgerFactory, ParticipantConfig, ReadWriteService, Config => KVUtilsConfig}
import com.daml.ledger.participant.state.pkvutils.api.PrivacyAwareKeyValueParticipantState
import com.daml.ledger.participant.state.v1.SeedService.Seeding
import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.daml.lf.data.Ref.ParticipantId
import com.digitalasset.daml.on.vmware.common.{KVBCHttpServer, KVBCPrometheusMetricsEndpoint}
import com.digitalasset.daml.on.vmware.participant.state.{ConcordKeyValueLedgerReader, ConcordLedgerWriter}
import com.digitalasset.daml.on.vmware.write.service.{KVBCClient, TRClient}
import com.digitalasset.ledger.api.auth.AuthService
import com.digitalasset.logging.LoggingContext
import com.digitalasset.platform.apiserver.ApiServerConfig
import com.digitalasset.platform.indexer.IndexerConfig
import com.digitalasset.ports.Port
import com.digitalasset.resources.ResourceOwner
import com.google.common.net.HostAndPort
import io.grpc.ConnectivityState
import org.slf4j.LoggerFactory
import scopt.OptionParser

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ConcordLedgerFactory
    extends LedgerFactory[ReadWriteService, ExtraConfig] {

  // This is unused since we're providing our own parser by overriding Runner's `owner`
  override def extraConfigParser(
      parser: OptionParser[KVUtilsConfig[ExtraConfig]]): Unit = ()

  def readWriteServiceOwner(
      config: KVUtilsConfig[ExtraConfig],
      participantConfig: ParticipantConfig,
  )(implicit materializer: Materializer,
    logCtx: LoggingContext): ResourceOwner[ReadWriteService] = {
    implicit val executionContext: ExecutionContextExecutor =
      materializer.executionContext

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
    val writer = new ConcordLedgerWriter(ledgerId,
                                         participantConfig.participantId,
                                         concordClient.commitTransaction,
                                         () => concordClient.currentHealth)
    for {
      closeableHttpServer <- ResourceOwner.forCloseable(() => new KVBCHttpServer())
      closeableEndpoint = {
        val endPoint = KVBCPrometheusMetricsEndpoint.createEndpoint(
          metricRegistry(participantConfig, config),
          closeableHttpServer.context)
        closeableHttpServer.start()
        endPoint
      }
      _ <- ResourceOwner.forCloseable(() => closeableEndpoint)
    } yield PrivacyAwareKeyValueParticipantState(reader, writer)
  }

  override def apiServerConfig(
      participantConfig: ParticipantConfig,
      config: KVUtilsConfig[ExtraConfig]): ApiServerConfig =
    super
      .apiServerConfig(participantConfig, config)
      .copy(tlsConfig = config.tlsConfig,
            maxInboundMessageSize = config.extra.maxInboundMessageSize)

  override def indexerConfig(
      participantConfig: ParticipantConfig,
      config: KVUtilsConfig[ExtraConfig]): IndexerConfig =
    super
      .indexerConfig(participantConfig, config)
      .copy(startupMode = config.extra.startupMode)

  override def authService(config: KVUtilsConfig[ExtraConfig]): AuthService =
    config.extra.authService.getOrElse(super.authService(config))

  override val defaultExtraConfig: ExtraConfig = ExtraConfig.Default

  private[server] def toKVUtilsAppConfig(
      config: Config): KVUtilsConfig[ExtraConfig] =
    KVUtilsConfig(
      Some("KVBC"),
      config.archiveFiles.map(_.toPath),
      config.tlsConfig,
      List(
        ParticipantConfig(
          config.participantId,
          Some("0.0.0.0"),
          Port(config.port),
          config.portFile,
          config.jdbcUrl,
          allowExistingSchemaForIndex = true,
        )),
      seeding = Seeding.Strong,
      ExtraConfig(
        config.maxInboundMessageSize,
        config.timeProvider,
        config.startupMode,
        config.replicas,
        config.useThinReplica,
        config.maxFaultyReplicas,
        config.jaegerAgentAddress,
        config.authService,
      ),
    )

  private[this] val logger = LoggerFactory.getLogger(this.getClass)
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
