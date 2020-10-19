// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import akka.stream.Materializer
import com.codahale.metrics.MetricRegistry
import com.daml.buildinfo.BuildInfo
import com.daml.ledger.api.auth.AuthService
import com.daml.ledger.participant.state.kvutils.api.{
  BatchingLedgerWriter,
  DefaultBatchingQueue,
  InterpretationCostBasedLedgerWriterChooser,
  LedgerWriter
}
import com.daml.ledger.participant.state.kvutils.app.{
  Config,
  LedgerFactory,
  ParticipantConfig,
  ReadWriteService
}
import com.daml.ledger.participant.state.pkvutils.api.PrivacyAwareKeyValueParticipantState
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
import com.digitalasset.daml.on.vmware.read.service.{
  ThinReplicaReadClient,
  ThinReplicaReadClientMetrics
}
import com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
import com.digitalasset.daml.on.vmware.write.service.ConcordWriteClient
import org.slf4j.LoggerFactory
import scopt.OptionParser

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ConcordLedgerFactory extends LedgerFactory[ReadWriteService, ExtraConfig] {
  private[this] val logger = LoggerFactory.getLogger(this.getClass)

  override val defaultExtraConfig: ExtraConfig = ExtraConfig.ReasonableDefault

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
         |darFileList=${config.archiveFiles.mkString("(", ";", ")")}""".stripMargin
        .replaceAll("\n", " "))
    val metrics = createMetrics(participantConfig, config)
    val thinReplicaClient =
      createThinReplicaClient(participantConfig.participantId, config.extra, metrics.registry)
    val concordWriteClient =
      ConcordWriteClients.createAndInitializeConcordWriteClient(config.extra, metrics)

    val reader = new ConcordKeyValueLedgerReader(
      thinReplicaClient.committedBlocks,
      config.ledgerId,
      () => concordWriteClient.currentHealth(),
    )
    logger.info(s"Connecting to the first core replica ${config.extra.replicas.head}")
    val writer =
      createLedgerWriter(config, participantConfig.participantId, metrics, concordWriteClient)
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

  private[server] def createLedgerWriter(
      config: Config[ExtraConfig],
      participantId: ParticipantId,
      metrics: Metrics,
      concordWriteClient: ConcordWriteClient)(
      implicit executionContext: ExecutionContext,
      materializer: Materializer,
      loggingContext: LoggingContext): LedgerWriter = {
    val concordWriter =
      new ConcordLedgerWriter(
        participantId = participantId,
        commitTransaction = (commitRequest, commitMetadata) =>
          concordWriteClient.commitTransaction(commitRequest, commitMetadata)(executionContext),
        fetchCurrentHealth = () => concordWriteClient.currentHealth
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
    config.extra.preExecutionTimeThreshold
      .map { preExecutionTimeThreshold =>
        val preExecutingConcordWriter =
          createPreExecutingLedgerWriter(participantId, concordWriteClient)
        InterpretationCostBasedLedgerWriterChooser(
          preExecutionTimeThreshold.toNanos,
          batchingWriter,
          preExecutingConcordWriter,
          metrics)
      }
      .getOrElse {
        if (config.extra.enableBatching) {
          batchingWriter
        } else {
          concordWriter
        }
      }
  }

  private def createPreExecutingLedgerWriter(
      participantId: ParticipantId,
      concordWriteClient: ConcordWriteClient)(
      implicit executionContext: ExecutionContext): ConcordLedgerWriter = {
    new ConcordLedgerWriter(
      participantId = participantId,
      commitTransaction =
        ConcordWriteClient.markRequestForPreExecution((commitRequest, commitMedatata) =>
          concordWriteClient.commitTransaction(commitRequest, commitMedatata)(executionContext)),
      fetchCurrentHealth = () => concordWriteClient.currentHealth()
    )
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
      new ThinReplicaClientJni,
      new ThinReplicaReadClientMetrics(metricRegistry))
    // format: on
  }
}
