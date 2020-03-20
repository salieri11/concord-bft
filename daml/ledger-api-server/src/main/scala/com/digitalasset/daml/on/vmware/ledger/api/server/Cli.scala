// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.ledger.api.server

import java.io.File

import com.auth0.jwt.algorithms.Algorithm
import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.jwt.{ECDSAVerifier, HMAC256Verifier, JwksVerifier, RSA256Verifier}
import com.digitalasset.ledger.api.auth.AuthServiceJWT
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
      opt[String]("jaeger-agent-address")
        .optional()
        .text(s"The address of the Jaeger agent, the default address is 'localhost:6831'.")
        .action((u, config) => config.copy(jaegerAgentAddress = u))
      opt[Seq[String]]("replicas")
        .optional()
        .action((x, c) => c.copy(replicas = x))
        .valueName("<IP:PORT>,<IP:PORT>,...")
        .text(s"List of replicas (<IP:PORT>). Initially, the server connects to the first replica in the list. Defaults to ${Config.default.replicas}.")
      opt[String]("auth-jwt-hs256-unsafe")
        .optional()
        .hidden()
        .validate(v => Either.cond(v.length > 0, (), "HMAC secret must be a non-empty string"))
        .text("[UNSAFE] Enables JWT-based authorization with shared secret HMAC256 signing: USE THIS EXCLUSIVELY FOR TESTING")
        .action( (secret, config) => config.copy(authService = Some(AuthServiceJWT(HMAC256Verifier(secret).valueOr(err => sys.error(s"Failed to create HMAC256 verifier: $err"))))))
      opt[String]("auth-jwt-rs256-crt")
        .optional()
        .validate(v => Either.cond(v.length > 0, (), "Certificate file path must be a non-empty string"))
        .text("Enables JWT-based authorization, where the JWT is signed by RSA256 with a public key loaded from the given X509 certificate file (.crt)")
        .action( (path, config) => config.copy(authService = Some(AuthServiceJWT(RSA256Verifier.fromCrtFile(path).valueOr(err => sys.error(s"Failed to create RSA256 verifier: $err"))))))
      opt[String]("auth-jwt-es256-crt")
        .optional()
        .validate(v => Either.cond(v.length > 0, (), "Certificate file path must be a non-empty string"))
        .text("Enables JWT-based authorization, where the JWT is signed by ECDSA256 with a public key loaded from the given X509 certificate file (.crt)")
        .action( (path, config) => config.copy(
          authService = Some(AuthServiceJWT(
            ECDSAVerifier.fromCrtFile(path, Algorithm.ECDSA256(_, null)).valueOr(err => sys.error(s"Failed to create ECDSA256 verifier: $err"))))))
      opt[String]("auth-jwt-es512-crt")
        .optional()
        .validate(v => Either.cond(v.length > 0, (), "Certificate file path must be a non-empty string"))
        .text("Enables JWT-based authorization, where the JWT is signed by ECDSA512 with a public key loaded from the given X509 certificate file (.crt)")
        .action( (path, config) => config.copy(
          authService = Some(AuthServiceJWT(
            ECDSAVerifier.fromCrtFile(path, Algorithm.ECDSA512(_, null)).valueOr(err => sys.error(s"Failed to create ECDSA512 verifier: $err"))))))
      opt[String]("auth-jwt-rs256-jwks")
        .optional()
        .validate(v => Either.cond(v.length > 0, (), "JWK server URL must be a non-empty string"))
        .text("Enables JWT-based authorization, where the JWT is signed by RSA256 with a public key loaded from the given JWKS URL")
        .action( (url, config) => config.copy(authService = Some(AuthServiceJWT(JwksVerifier(url)))))
      opt[Unit]("version")
        .optional()
        .action((_, _) => { println(BuildInfo.Version) ; sys.exit(0) })
        .text("Prints the version on stdout and exit.")
      arg[File]("<archive>...")
        .optional()
        .unbounded()
        .action((f, config) => config.copy(archiveFiles = f :: config.archiveFiles))
        .text("DAR files to load. Scenarios are ignored. The server starts with an empty ledger by default.")
    }

  def parse(args: Array[String], binaryName: String = "ledger-api-server", description: String = "vDAML Ledger API Server"): Option[Config] =
    cmdArgParser(binaryName, description).parse(args, Config.default)
}
