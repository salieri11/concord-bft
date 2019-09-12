// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.daml.ledger.api.server.damlonx.kvbc

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.digitalasset.daml.lf.archive.DarReader
import com.digitalasset.daml_lf.DamlLf.Archive
import org.slf4j.LoggerFactory

import scala.util.Try
import com.digitalasset.platform.common.util.DirectExecutionContext
import com.digitalasset.kvbc_sync_adapter.KVBCParticipantState
import com.digitalasset.platform.index.{StandaloneIndexServer, StandaloneIndexerServer}

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object KvbcLedgerServer extends App {
  val logger = LoggerFactory.getLogger(this.getClass)

  // Initialize Akka and log exceptions in flows.
  implicit val system = ActorSystem("ReferenceServer")
  implicit val materializer = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy { e =>
        logger.error(s"Supervision caught exception: $e")
        Supervision.Stop
      })

  def archivesFromDar(file: File): List[Archive] = {
    DarReader[Archive] { case (_, x) => Try(Archive.parseFrom(x)) }
      .readArchiveFromFile(file)
      .fold(t => throw new RuntimeException(s"Failed to parse DAR from $file", t), dar => dar.all)
  }

  val extConfig = Cli.parse(args, "ledger-api-server","vDAML Ledger API Server").getOrElse(sys.exit(1))

  logger.info(
    s"""Initialized vDAML ledger api server: version=${BuildInfo.Version},
       |participantId=${extConfig.participantId.toString}, port=${extConfig.config.port},
       |jdbcUrl=${extConfig.config.jdbcUrl},
       |dar file(s)=${args.drop(2).mkString("(", ";", ")")}""".stripMargin.replaceAll("\n", " "))

  logger.info(s"Connecting to core replica ${args(0)}")
  val ledger = KVBCParticipantState(
    "KVBC", extConfig.participantId, extConfig.replicaHost, extConfig.replicaPort, true)

  val readService = ledger
  val writeService = ledger

  val indexerServer = StandaloneIndexerServer(readService, extConfig.config)
  val indexServer = StandaloneIndexServer(extConfig.config, readService, writeService).start()

  extConfig.config.archiveFiles.foreach { f =>
    val archives = archivesFromDar(f)
    archives.foreach { archive =>
      logger.info(s"Uploading package ${archive.getHash}...")
    }
    ledger.uploadPackages(archives, Some("uploaded on startup by participant"))
  }

  implicit val ec: ExecutionContext = DirectExecutionContext

  val closed = new AtomicBoolean(false)

  def closeServer(): Unit = {
    if (closed.compareAndSet(false, true)) {
      indexServer.close()
      indexerServer.close()
      //ledger.close()
      materializer.shutdown()
      val _ = system.terminate()
    }
  }

  try
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      closeServer()
    }))
  catch {
    case NonFatal(t) =>
      logger.error("Shutting down Sandbox application because of initialization error", t)
      closeServer()
  }

}
