// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine.replay

import java.nio.file.{Path, Paths}

import com.daml.ledger.participant.state.kvutils.export.v2.ProtobufBasedLedgerDataImporter
import com.digitalasset.kvbc.daml_validator.ValidationServiceGrpc
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import io.opentracing.Tracer
import io.opentracing.contrib.tracerresolver.TracerResolver
import io.opentracing.mock.MockTracer
import scopt.Read

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

case object ReplayLedgerExportTool extends App {

  private val commandLineOptions = parseCommandLine
  println(
    s"Replaying submissions from ${commandLineOptions.ledgerExportPath.get} " +
      s"to ${commandLineOptions.targetHost.get}:${commandLineOptions.targetPort.get}")
  replaySubmissions(commandLineOptions)

  private final case class CommandLineOptions(
      ledgerExportPath: Option[Path],
      targetHost: Option[String],
      targetPort: Option[Int],
      preExecute: Boolean,
      maxInboundMessageSize: Int,
      tracer: Tracer,
  )

  private def defaultCommandLineOptions =
    CommandLineOptions(
      ledgerExportPath = None,
      targetHost = None,
      targetPort = None,
      preExecute = false,
      maxInboundMessageSize = 64 * 1024 * 1024,
      tracer = new MockTracer()
    )

  private implicit def pathReader: Read[Path] =
    Read.reads(Paths.get(_))

  private def parseCommandLine: CommandLineOptions = {
    val programName = ReplayLedgerExportTool.productPrefix

    new scopt.OptionParser[CommandLineOptions](programName) {
      head(programName)

      arg[Path]("<path-to-ledger-export>")
        .maxOccurs(1)
        .action((path, options) => options.copy(ledgerExportPath = Some(path)))
      arg[String]("<target-host>")
        .maxOccurs(1)
        .action((host, options) => options.copy(targetHost = Some(host)))
      arg[Int]("<target-port>")
        .maxOccurs(1)
        .action((port, options) => options.copy(targetPort = Some(port)))

      opt[Unit]("pre-execute")
        .text(
          "If specified, the pre-execution code path will be used for validation (default = false).")
        .optional()
        .maxOccurs(1)
        .action((_, options) => options.copy(preExecute = true))
      opt[Int]("max-inbound-message-size")
        .text("If specified, the maximum gRPC inbound message size (default = 64 MB).")
        .optional()
        .maxOccurs(1)
        .action((messageSize, options) => options.copy(maxInboundMessageSize = messageSize))
      opt[String]("tracing")
        .text("If specified, enables OpenTracing and uses the specified Jaeger service name (default = disabled).")
        .optional()
        .maxOccurs(1)
        .action((serviceName, options) => {
          System.setProperty("JAEGER_SERVICE_NAME", serviceName)
          options.copy(tracer = TracerResolver.resolveTracer())
        })

      note(
        lineSeparator + "In order to generate a ledger export, set the environment variable " +
          "KVUTILS_LEDGER_EXPORT to a file path before starting the DAML Execution Engine."
          + lineSeparator)
    }.parse(args, defaultCommandLineOptions).getOrElse(sys.exit(-1))
  }

  private def replaySubmissions(commandLineOptions: CommandLineOptions): Unit =
    for {
      host <- commandLineOptions.targetHost
      port <- commandLineOptions.targetPort
      ledgerExportPath <- commandLineOptions.ledgerExportPath
      clientChannel = createManagedChannel(host, port, commandLineOptions.maxInboundMessageSize)
      validationServiceClient = ValidationServiceGrpc.stub(clientChannel)
      instance = if (commandLineOptions.preExecute) {
        implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
        new ReplayFromLedgerExportThroughPreExecution(
          validationServiceClient,
          commandLineOptions.tracer)
      } else
        new ReplayFromLedgerExportThroughBatching(
          validationServiceClient,
          commandLineOptions.tracer)
      importer = ProtobufBasedLedgerDataImporter(ledgerExportPath)
    } yield instance.run(importer)

  private def createManagedChannel(
      host: String,
      port: Int,
      maxInboundMessageSize: Int,
  ): ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxInboundMessageSize)
      .usePlaintext()
      .build

  private val lineSeparator = System.lineSeparator()
}
