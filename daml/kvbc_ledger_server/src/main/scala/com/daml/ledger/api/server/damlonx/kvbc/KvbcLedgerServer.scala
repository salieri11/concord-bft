// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.daml.ledger.api.server.damlonx.kvbc

import java.io.File
import java.time.Instant
import java.util.zip.ZipFile

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.daml.ledger.api.server.damlonx.Server
import com.daml.ledger.participant.state.index.v1.impl.reference.ReferenceIndexService
import com.digitalasset.daml.lf.archive.DarReader
import com.digitalasset.daml_lf.DamlLf.Archive
import com.daml.ledger.participant.state.backport.TimeModel
import org.slf4j.LoggerFactory

import scala.util.Try
import com.digitalasset.platform.common.util.DirectExecutionContext
import com.digitalasset.kvbc_sync_adapter.KVBCParticipantState

import scala.concurrent.{ExecutionContext, Future}

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

  val timeModel = TimeModel.reasonableDefault

  def archivesFromDar(file: File): List[Archive] = {
    DarReader[Archive] { case (_, x) => Try(Archive.parseFrom(x)) }
      .readArchive(new ZipFile(file))
      .fold(t => throw new RuntimeException(s"Failed to parse DAR from $file", t), dar => dar.all)
  }

  // We expect the first argument to be the core replica address
  val replicaAddr = args(0).split(":")
  val (ip, port) = (replicaAddr(0), replicaAddr(1).toInt)

  logger.info("Connecting to Core Replica {}", args(0))
  val kvbcPS = new KVBCParticipantState(
    "KVBC", ip, port)

  implicit val ec: ExecutionContext = DirectExecutionContext
  kvbcPS.getLedgerInitialConditions.runWith(Sink.head)
    .flatMap { initialConditions =>
      // Upload archives.
      Future.sequence(
        args.drop(1)
          .flatMap { arg => archivesFromDar(new File(arg)) }
          .map { archive => kvbcPS.uploadArchive(archive) }
          .toList
      ).map { _ =>
        initialConditions
      }
    }
    .foreach { initialConditions =>
      val indexService = ReferenceIndexService(kvbcPS, initialConditions)

      val server = Server(
        serverPort = 6865,
        indexService = indexService,
        writeService = kvbcPS,
        sslContext = None
      )

      // Add a hook to close the server. Invoked when Ctrl-C is pressed.
      Runtime.getRuntime.addShutdownHook(new Thread(() => server.close()))

    }
}
