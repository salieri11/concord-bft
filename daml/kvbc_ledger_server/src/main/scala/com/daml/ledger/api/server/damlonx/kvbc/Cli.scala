package com.daml.ledger.api.server.damlonx.kvbc

import java.io.File

import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.ledger.api.tls.TlsConfiguration
import com.digitalasset.platform.index.config.Config

import scopt.Read

//TODO: Replace with Config class after upgrade to version 100.13.22
final case class ExtConfig(
    config: Config,
    replicas: Seq[String])

object ExtConfig {
  def default: ExtConfig =
    new ExtConfig(
      Config.default,
      Seq("localhost:50051"))
}

object Cli {

  private implicit val ledgerStringRead: Read[Ref.LedgerString] =
    Read.stringRead.map(Ref.LedgerString.assertFromString)

  private val pemConfig = (path: String, ec: ExtConfig) =>
    ec.copy(
      config = ec.config.copy(
        tlsConfig = ec.config.tlsConfig.fold(
          Some(TlsConfiguration(enabled = true, None, Some(new File(path)), None)))(c =>
          Some(c.copy(keyFile = Some(new File(path)))))))

  private val crtConfig = (path: String, ec: ExtConfig) =>
    ec.copy(
      config = ec.config.copy(
        tlsConfig = ec.config.tlsConfig.fold(
          Some(TlsConfiguration(enabled = true, Some(new File(path)), None, None)))(c =>
          Some(c.copy(keyCertChainFile = Some(new File(path)))))))

  private val cacrtConfig = (path: String, ec: ExtConfig) =>
    ec.copy(
      config = ec.config.copy(
        tlsConfig = ec.config.tlsConfig.fold(
          Some(TlsConfiguration(enabled = true, None, None, Some(new File(path)))))(c =>
          Some(c.copy(trustCertCollectionFile = Some(new File(path)))))))

  private def cmdArgParser(binaryName: String, description: String) =
    new scopt.OptionParser[ExtConfig](binaryName) {
      head(description)
      opt[Int]("port")
        .optional()
        .action((p, ec) => ec.copy(config = ec.config.copy(port = p)))
        .text("Server port. If not set, a random port is allocated.")
      opt[File]("port-file")
        .optional()
        .action((f, ec) => ec.copy(config = ec.config.copy(portFile = Some(f))))
        .text("File to write the allocated port number to. Used to inform clients in CI about the allocated port.")
      opt[String]("pem")
        .optional()
        .text("TLS: The pem file to be used as the private key.")
        .action(pemConfig)
      opt[String]("crt")
        .optional()
        .text("TLS: The crt file to be used as the cert chain. Required if any other TLS parameters are set.")
        .action(crtConfig)
      opt[String]("cacrt")
        .optional()
        .text("TLS: The crt file to be used as the the trusted root CA.")
        .action(cacrtConfig)
      opt[Int]("maxInboundMessageSize")
        .action((x, ec) => ec.copy(config = ec.config.copy(maxInboundMessageSize = x)))
        .text(
          s"Max inbound message size in bytes. Defaults to ${Config.DefaultMaxInboundMessageSize}.")
      opt[String]("jdbc-url")
        .text(s"The JDBC URL to the postgres database used for the indexer and the index.")
        .action((u, ec) => ec.copy(config = ec.config.copy(jdbcUrl = u)))
      opt[Ref.LedgerString]("participant-id")
        .optional()
        .text(s"The participant id given to all components of a ledger api server. Defaults to ${Config.default.participantId}")
        .action((p, ec) => ec.copy(config = ec.config.copy(participantId = p)))
      opt[Seq[String]]("replicas")
        .optional()
        .action((x, c) => c.copy(replicas = x))
        .valueName("<IP:PORT>,<IP:PORT>,...")
        .text(s"List of replicas (<IP:PORT>). Initially, the server connects to the first replica in the list. Defaults to ${ExtConfig.default.replicas}.")
      arg[File]("<archive>...")
        .optional()
        .unbounded()
        .action((f, ec) => ec.copy(config = ec.config.copy(archiveFiles = f :: ec.config.archiveFiles)))
        .text("DAR files to load. Scenarios are ignored. The servers starts with an empty ledger by default.")
    }

  def parse(args: Array[String], binaryName: String, description: String): Option[ExtConfig] =
    cmdArgParser(binaryName, description).parse(args, ExtConfig.default)
}
