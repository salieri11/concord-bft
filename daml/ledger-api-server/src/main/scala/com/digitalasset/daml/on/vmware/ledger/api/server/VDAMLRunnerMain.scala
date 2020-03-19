package com.digitalasset.daml.on.vmware.ledger.api.server

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.codahale.metrics.{MetricRegistry, SharedMetricRegistries}
import com.daml.ledger.participant.state.kvutils.app.{Config => KVUtilsConfig}
import com.digitalasset.daml.on.vmware.write.service.KVBCParticipantState
import com.digitalasset.api.util.TimeProvider
import com.digitalasset.daml.on.vmware.ledger.api.server.Config.DefaultMaxInboundMessageSize
import com.digitalasset.daml.on.vmware.ledger.api.server.app.{LedgerFactory, Runner}
import com.digitalasset.ledger.api.tls.TlsConfiguration
import com.digitalasset.platform.apiserver.ApiServerConfig
import com.digitalasset.platform.indexer.{IndexerConfig, IndexerStartupMode}
import com.digitalasset.resources.{ProgramResource, ResourceOwner}
import org.slf4j.LoggerFactory
import scopt.OptionParser

import scala.concurrent.ExecutionContext

object VDAMLRunnerMain {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)
  private[this] var prometheusMetrics = Set[MetricRegistry]()

  def main(args: Array[String]): Unit = {
    new ProgramResource(new Runner("vDAML Ledger API Server", KVBCLedgerFactory) {
      override def owner(args: Seq[String]): ResourceOwner[Unit] = {
        val config =
          Cli
            .parse(args.toArray, "ledger-api-server", "vDAML Ledger API Server")
            .getOrElse(sys.exit(1))

        logger.info(
          s"""Initialized vDAML ledger api server: version=${BuildInfo.Version}
             |participantId=${config.participantId} replicas=${config.replicas}
             |jdbcUrl=${config.jdbcUrl}
             |dar_file(s)=${args.drop(2).mkString("(", ";", ")")}""".stripMargin
            .replaceAll("\n", " "))

        logger.info(s"Connecting to the first core replica ${config.replicas.head}")

        val kvUtilsAppConfig =
          KVUtilsConfig(
            config.participantId,
            Some("0.0.0.0"),
            config.port,
            config.portFile,
            config.jdbcUrl,
            Some("KVBC"),
            config.archiveFiles.map(_.toPath),
            allowExistingSchemaForIndex = true,
            KVBCConfigExtra(
              config.maxInboundMessageSize,
              config.timeProvider,
              config.tlsConfig,
              config.startupMode,
              config.replicas,
              config.useThinReplica,
              config.maxFaultyReplicas
            ),
          )

        super.owner(kvUtilsAppConfig)
      }
    }.owner(args)).run()
  }

  final case class KVBCConfigExtra(
      maxInboundMessageSize: Int,
      timeProvider: TimeProvider, // enables use of non-wall-clock time in tests
      tlsConfig: Option[TlsConfiguration],
      startupMode: IndexerStartupMode,
      replicas: Seq[String],
      useThinReplica: Boolean,
      maxFaultyReplicas: Short,
  ) {
    def withTlsConfig(modify: TlsConfiguration => TlsConfiguration): KVBCConfigExtra =
      copy(tlsConfig = Some(modify(tlsConfig.getOrElse(TlsConfiguration.Empty))))
  }

  object KVBCLedgerFactory extends LedgerFactory[KVBCParticipantState, KVBCConfigExtra] {

    // This is unused since we're providing our own parser by overriding Runner's `owner`
    override def extraConfigParser(parser: OptionParser[KVUtilsConfig[KVBCConfigExtra]]): Unit = ()

    override def readWriteServiceOwner(config: KVUtilsConfig[KVBCConfigExtra])(
        implicit executionContext: ExecutionContext,
        materializer: Materializer,
        logCtx: com.digitalasset.logging.LoggingContext
    ): ResourceOwner[KVBCParticipantState] = {
      implicit val akkaSystem: ActorSystem = materializer.system
      KVBCParticipantState.owner(
        "KVBC",
        config.participantId,
        config.extra.replicas.toArray,
        config.extra.useThinReplica,
        config.extra.maxInboundMessageSize.toShort,
        prometheusMetrics.toList
      )
    }

    override def apiServerConfig(config: KVUtilsConfig[KVBCConfigExtra]): ApiServerConfig =
      super
        .apiServerConfig(config)
        .copy(
          tlsConfig = config.extra.tlsConfig,
          maxInboundMessageSize = config.extra.maxInboundMessageSize)

    override def indexerConfig(config: KVUtilsConfig[KVBCConfigExtra]): IndexerConfig =
      super.indexerConfig(config).copy(startupMode = config.extra.startupMode)

    override def indexerMetricRegistry(config: KVUtilsConfig[KVBCConfigExtra]): MetricRegistry = {
      val indexerMetricRegistry = SharedMetricRegistries.getOrCreate(s"indexer")
      prometheusMetrics += indexerMetricRegistry
      indexerMetricRegistry
    }

    override def apiServerMetricRegistry(config: KVUtilsConfig[KVBCConfigExtra]): MetricRegistry =
      super.apiServerMetricRegistry(config)

    override val defaultExtraConfig: KVBCConfigExtra = KVBCConfigExtra(
      maxInboundMessageSize = DefaultMaxInboundMessageSize,
      timeProvider = TimeProvider.UTC,
      tlsConfig = None,
      startupMode = IndexerStartupMode.MigrateAndStart,
      replicas = Seq("localhost:50051"),
      useThinReplica = false,
      maxFaultyReplicas = 1,
    )
  }
}
