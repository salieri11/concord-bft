// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.ledger.api.server

import java.io.File

import com.digitalasset.daml.lf.data.Ref

import scopt.Read

object Cli {

  private implicit val participantIdRead: Read[Ref.ParticipantId] =
    Read.stringRead.map(Ref.ParticipantId.assertFromString)

  private def cmdArgParser(binaryName: String, description: String) =
    new scopt.OptionParser[Config](binaryName) {
      head(description)
      opt[Int]("port")
        .optional()
        .action((p, config) => config.copy(port = p))
        .text("Server port. If not set, a random port is allocated.")
      opt[File]("port-file")
        .optional()
        .action((f, config) => config.copy(portFile = Some(f.toPath)))
        .text("File to write the allocated port number to. Used to inform clients in CI about the allocated port.")
      opt[String]("pem")
        .optional()
        .text("TLS: The pem file to be used as the private key.")
        .action((path, config) => config.withTlsConfig(c => c.copy(keyFile = Some(new File(path)))))
      opt[String]("crt")
        .optional()
        .text("TLS: The crt file to be used as the cert chain. Required if any other TLS parameters are set.")
        .action((path, config) =>
          config.withTlsConfig(c => c.copy(keyCertChainFile = Some(new File(path)))))
      opt[String]("cacrt")
        .optional()
        .text("TLS: The crt file to be used as the the trusted root CA.")
        .action((path, config) =>
          config.withTlsConfig(c => c.copy(trustCertCollectionFile = Some(new File(path)))))
      opt[Int]("maxInboundMessageSize")
        .action((x, config) => config.copy(maxInboundMessageSize = x))
        .text(
          s"Max inbound message size in bytes. Defaults to ${Config.DefaultMaxInboundMessageSize}.")
      opt[String]("jdbc-url")
        .text(s"The JDBC URL to the postgres database used for the indexer and the index.")
        .action((u, config) => config.copy(jdbcUrl = u))
      opt[Ref.ParticipantId]("participant-id")
        .optional()
        .text(s"The participant id given to all components of a ledger api server. Defaults to ${Config.default.participantId}")
        .action((p, config) => config.copy(participantId = p))
      opt[Unit]("use-thin-replica")
        .optional()
        .action((_, c) => c.copy(useThinReplica = true))
        .text("Use thin replica client as a source ledger updates.")
      opt[Int]("max-faulty-replicas")
        .optional()
        .action((x, c) => c.copy(maxFaultyReplicas = x.toShort))
        .text("Maximum number of faulty replicas that thin replica client should tolerate.")
      opt[Seq[String]]("replicas")
        .optional()
        .action((x, c) => c.copy(replicas = x))
        .valueName("<IP:PORT>,<IP:PORT>,...")
        .text(s"List of replicas (<IP:PORT>). Initially, the server connects to the first replica in the list. Defaults to ${Config.default.replicas}.")
      arg[File]("<archive>...")
        .optional()
        .unbounded()
        .action((f, config) => config.copy(archiveFiles = f :: config.archiveFiles))
        .text("DAR files to load. Scenarios are ignored. The server starts with an empty ledger by default.")
    }

  def parse(args: Array[String], binaryName: String, description: String): Option[Config] =
    cmdArgParser(binaryName, description).parse(args, Config.default)
}
