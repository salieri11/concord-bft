package com.digitalasset.daml.on.vmware.execution.engine.replay

import java.io.{DataInputStream, FileInputStream}

import com.digitalasset.kvbc.daml_validator.ValidationServiceGrpc
import io.grpc.{ManagedChannel, ManagedChannelBuilder}

object ReplayLedgerExportTool {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Usage: <replay-tool> <path-to-ledger-export> <target-host> <target-port>")
      println(
        "In order to generate a ledger export, set the environment variable " +
          "KVUTILS_LEDGER_EXPORT to a file path before starting the DAML Execution Engine.")
      sys.exit(1)
    }
    val inputPath = args(0)
    val targetHost = args(1)
    val targetPort = args(2).toInt
    println(s"Replaying submissions from $inputPath to $targetHost:$targetPort")
    replaySubmissions(inputPath, targetHost, targetPort)
  }

  private def replaySubmissions(inputPath: String, targetHost: String, targetPort: Int): Unit = {
    val clientChannel = createManagedChannel(targetHost, targetPort)
    val validationServiceClient = ValidationServiceGrpc.stub(clientChannel)
    val instance = new ReplayFromLedgerExport(validationServiceClient)
    val ledgerDumpStream =
      new DataInputStream(new FileInputStream(inputPath))
    instance.run(ledgerDumpStream)
  }

  private def createManagedChannel(host: String, port: Int): ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(64 * 1024 * 1024)
      .usePlaintext()
      .build
}
