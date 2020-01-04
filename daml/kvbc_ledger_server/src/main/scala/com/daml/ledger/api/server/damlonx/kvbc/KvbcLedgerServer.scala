// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.daml.ledger.api.server.damlonx.kvbc

import java.io.File
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}

import com.codahale.metrics.{MetricRegistry, SharedMetricRegistries}

import com.daml.ledger.participant.state.v1.SubmissionId
import com.digitalasset.daml.lf.archive.DarReader
import com.digitalasset.daml_lf_dev.DamlLf.Archive
import com.digitalasset.kvbc.common.{KVBCHttpServer, KVBCMetricsRegistry, KVBCPrometheusMetricsEndpoint}
import com.digitalasset.kvbc_sync_adapter.KVBCParticipantState
import com.digitalasset.ledger.api.auth.AuthServiceWildcard
import com.digitalasset.platform.apiserver.{ApiServerConfig, StandaloneApiServer}
import com.digitalasset.platform.common.logging.NamedLoggerFactory
import com.digitalasset.platform.indexer.{IndexerConfig, StandaloneIndexerServer}

import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

object KvbcLedgerServer extends App {
  val logger = LoggerFactory.getLogger(this.getClass)

  // Initialize Akka and log exceptions in flows.
  implicit val system = ActorSystem("ledger-api-server")
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val materializer = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy { e =>
        logger.error(s"Supervision caught an exception $e")
        Supervision.Stop
      })

  def archivesFromDar(file: File): List[Archive] = {
    DarReader[Archive] { case (_, x) => Try(Archive.parseFrom(x)) }
      .readArchiveFromFile(file)
      .fold(t => throw new RuntimeException(s"Failed to parse DAR from $file", t), dar => dar.all)
  }

  val config = Cli.parse(args, "ledger-api-server","vDAML Ledger API Server").getOrElse(sys.exit(1))
  val participantId = config.participantId

  private val kvMetrics = new KVBCMetricsRegistry("kvutils")
  private val indexerMetrics = SharedMetricRegistries.getOrCreate(s"indexer")
  private val serverMetrics = SharedMetricRegistries.getOrCreate(s"ledger-api-server")
  private val httpServer = new KVBCHttpServer()
  KVBCPrometheusMetricsEndpoint.createEndpoint(List(kvMetrics.registry,indexerMetrics,serverMetrics), httpServer.context)
  httpServer.start()

  logger.info(
    s"""Initialized vDAML ledger api server: version=${BuildInfo.Version}
       |participantId=${participantId.toString} replicas=${config.replicas}
       |jdbcUrl=${config.jdbcUrl}
       |dar_file(s)=${args.drop(2).mkString("(", ";", ")")}""".stripMargin.replaceAll("\n", " "))

  logger.info(s"Connecting to the first core replica ${config.replicas.head}")
  val address = config.replicas.head.split(":")
  val ledger = KVBCParticipantState("KVBC", participantId, address(0), address(1).toInt)

  val readService = ledger
  val writeService = ledger
  val loggerFactory = NamedLoggerFactory.forParticipant(participantId)
  val authService = AuthServiceWildcard


  val participantF: Future[(AutoCloseable, AutoCloseable)] = for {
    indexer <- newIndexer(config, indexerMetrics)
    apiServer <- newApiServer(config, serverMetrics).start()
  } yield (indexer, apiServer)

  config.archiveFiles.foreach { f =>
    val submissionId = SubmissionId.assertFromString(UUID.randomUUID().toString)
    val archives = archivesFromDar(f)
    archives.foreach { archive =>
      logger.info(s"Uploading package ${archive.getHash}...")
    }
    ledger.uploadPackages(submissionId, archives, Some("uploaded on startup by participant"))
  }

  def newIndexer(config: Config, metrics: MetricRegistry) =
    StandaloneIndexerServer(
      readService,
      IndexerConfig(config.participantId, config.jdbcUrl, config.startupMode),
      NamedLoggerFactory.forParticipant(config.participantId),
      metrics,
    )

  def newApiServer(config: Config, metrics: MetricRegistry) =
    new StandaloneApiServer(
      ApiServerConfig(
        config.participantId,
        config.archiveFiles,
        config.port,
        config.jdbcUrl,
        config.tlsConfig,
        config.timeProvider,
        config.maxInboundMessageSize,
        config.portFile,
      ),
      readService,
      writeService,
      authService,
      NamedLoggerFactory.forParticipant(config.participantId),
      metrics,
    )

  val closed = new AtomicBoolean(false)

  def closeServer(): Unit = {
    if (closed.compareAndSet(false, true)) {
      participantF.foreach {
        case (indexer, apiServer) =>
          indexer.close()
          apiServer.close()
      }
      httpServer.stop()
      materializer.shutdown()
      val _ = system.terminate()
    }
  }

  private def startupFailed(e: Throwable): Unit = {
    logger.error("Shutting down vDAML Ledger API Server because of initialization error", e)
    closeServer()
  }

  participantF.failed.foreach(startupFailed)


  try
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      closeServer()
    }))
  catch {
    case NonFatal(e) =>
      startupFailed(e)
  }

}
