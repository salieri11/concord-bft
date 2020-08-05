package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit

import com.auth0.jwt.algorithms.Algorithm
import com.daml.buildinfo.BuildInfo
import com.daml.jwt.{ECDSAVerifier, HMAC256Verifier, JwksVerifier, RSA256Verifier}
import com.daml.ledger.api.auth.{AuthService, AuthServiceJWT, AuthServiceWildcard}
import com.daml.ledger.participant.state.kvutils.app.Config
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.lf.data.Ref.IdString
import scopt.OptionParser

import scala.concurrent.duration._

sealed trait WriteClientsConfig {
  val replicas: Seq[String]
  val maxFaultyReplicas: Short
  val useBftClient: Boolean
  val bftClientConfigPath: Option[Path]
  val bftClientRequestTimeout: Duration
}

final case class ExtraConfig(
    override val replicas: Seq[String],
    useThinReplica: Boolean,
    override val maxFaultyReplicas: Short,
    jaegerAgentAddress: String,
    authService: Option[AuthService],
    preExecutionCostThreshold: Option[Long],
    enableBatching: Boolean, // Whether we're batching requests or not.
    maxBatchQueueSize: Int, // Number of submissions we're willing to queue before dropping.
    maxBatchSizeBytes: Long, // The maximum size for a batch before it is forcefully sent.
    maxBatchWaitDuration: FiniteDuration, // Maximum duration we're willing to wait to fill a batch.
    maxTrcReadDataTimeout: Short,
    maxTrcReadHashTimeout: Short,
    maxBatchConcurrentCommits: Int,
    override val useBftClient: Boolean, // Whether to use the new BFT Concord Client Pool in the writer.
    override val bftClientConfigPath: Option[Path],
    override val bftClientRequestTimeout: Duration,
) extends WriteClientsConfig

object ExtraConfig {
  val DefaultParticipantId: IdString.ParticipantId =
    ParticipantId.assertFromString("standalone-participant")

  val Default: ExtraConfig = ExtraConfig(
    replicas = Seq("localhost:50051"),
    useThinReplica = false,
    maxFaultyReplicas = 1,
    jaegerAgentAddress = "localhost:6831",
    authService = Some(AuthServiceWildcard),
    preExecutionCostThreshold = None,
    enableBatching = false,
    maxBatchQueueSize = 100,
    maxBatchSizeBytes = 4 * 1024 * 1024 /* 4MB */,
    maxBatchWaitDuration = 100.millis,
    maxBatchConcurrentCommits = 5,
    maxTrcReadDataTimeout = 0,
    maxTrcReadHashTimeout = 0,
    useBftClient = false,
    bftClientConfigPath = None,
    bftClientRequestTimeout = Duration.create(30, TimeUnit.SECONDS),
  )

  def addCommandLineArguments(parser: OptionParser[Config[ExtraConfig]]): Unit = {
    parser
      .opt[Unit]("version")
      .optional()
      .action((_, _) => {
        println(BuildInfo.Version)
        sys.exit(0)
      })
      .text("Prints the version on stdout and exit.")
    parser
      .opt[Int]("maxInboundMessageSize")
      .action((size, config) => config.copy(maxInboundMessageSize = size))
      .text(s"Deprecated parameter -- please use --max-inbound-message-size instead.")
    //
    // Thin replica client
    //
    parser
      .opt[Seq[String]]("replicas")
      .optional()
      .action((replicas, config) => config.copy(extra = config.extra.copy(replicas = replicas)))
      .valueName("<IP:PORT>,<IP:PORT>,...")
      .text(
        s"List of replicas (<IP:PORT>). Initially, the server connects to the first replica in the list. Defaults to ${ExtraConfig.Default.replicas}.")
    parser
      .opt[Unit]("use-thin-replica")
      .optional()
      .action((_, config) => config.copy(extra = config.extra.copy(useThinReplica = true)))
      .text("Use thin replica client as a source ledger updates.")
    parser
      .opt[Int]("maxFaultyReplicas")
      .optional()
      .action((maxFaultyReplicas, config) =>
        config.copy(extra = config.extra.copy(maxFaultyReplicas = maxFaultyReplicas.toShort)))
      .text("Maximum number of faulty replicas that thin replica client should tolerate.")
    // format: off
    parser
      .opt[Int]("maxTrcReadDataTimeout")
      .optional()
      .action((maxTrcReadDataTimeout, config) =>
        config.copy(extra = config.extra.copy(maxTrcReadDataTimeout = maxTrcReadDataTimeout.toShort)))
      .text("Maximum time in seconds until the data read request gets cancelled.")
    parser
      .opt[Int]("maxTrcReadHashTimeout")
      .optional()
      .action((maxTrcReadHashTimeout, config) =>
        config.copy(extra = config.extra.copy(maxTrcReadHashTimeout = maxTrcReadHashTimeout.toShort)))
      .text("Maximum time in seconds until the hash read request gets cancelled.")
    // format: on
    parser
      .opt[String]("jaeger-agent-address")
      .optional()
      .text(
        s"The address of the Jaeger agent in <HOST:PORT> format. Defaults to ${ExtraConfig.Default.jaegerAgentAddress}.")
      .action((hostAndPort, config) =>
        config.copy(extra = config.extra.copy(jaegerAgentAddress = hostAndPort)))

    //
    // BFT concord client pool
    //
    // format: off
    parser
      .opt[Unit]("use-bft-client")
      .optional()
      .action((_, config) => config.copy(extra = config.extra.copy(useBftClient = true)))
      .text("Use the BFT Client to submit DAML transactions to Concord.")
    // format: on
    implicit val pathReader: scopt.Read[Path] =
      scopt.Read.reads(Paths.get(_))
    parser
      .opt[Path]("bft-client-config-path")
      .optional()
      .action((bftClientConfigPath, config) => {
        config.copy(extra = config.extra.copy(bftClientConfigPath = Some(bftClientConfigPath)))
      })
      .text("Specify the BFT Client configuration file path.")
    parser
      .opt[Duration]("bft-client-request-timeout")
      .optional()
      .action((bftClientRequestTimeout, config) =>
        config.copy(extra = config.extra.copy(bftClientRequestTimeout = bftClientRequestTimeout)))
      .text("Maximum time until the data write request gets cancelled by the BFT Client.")

    //
    // auth-* parameters.
    //
    parser
      .opt[String]("auth-jwt-hs256-unsafe")
      .optional()
      .hidden()
      .validate(v => Either.cond(v.length > 0, (), "HMAC secret must be a non-empty string"))
      .text("[UNSAFE] Enables JWT-based authorization with shared secret HMAC256 signing: USE THIS EXCLUSIVELY FOR TESTING")
      .action((secret, config) =>
        config.copy(extra =
          config.extra.copy(authService = Some(AuthServiceJWT(HMAC256Verifier(secret).valueOr(err =>
            sys.error(s"Failed to create HMAC256 verifier: $err")))))))
    // format: off
    parser.opt[String]("auth-jwt-rs256-crt")
      .optional()
      .validate(v => Either.cond(v.length > 0, (), "Certificate file path must be a non-empty string"))
      .text("Enables JWT-based authorization, where the JWT is signed by RSA256 with a public key loaded from the given X509 certificate file (.crt)")
      .action((path, config) => config.copy(extra = config.extra.copy(authService = Some(AuthServiceJWT(RSA256Verifier.fromCrtFile(path).valueOr(err => sys.error(s"Failed to create RSA256 verifier: $err")))))))
    //format: on
    parser
      .opt[String]("auth-jwt-es256-crt")
      .optional()
      .validate(v => Either.cond(v.length > 0, (), "Certificate file path must be a non-empty string"))
      .text("Enables JWT-based authorization, where the JWT is signed by ECDSA256 with a public key loaded from the given X509 certificate file (.crt)")
      .action((path, config) => config.copy(extra = config.extra.copy(
        authService = Some(AuthServiceJWT(
          ECDSAVerifier.fromCrtFile(path, Algorithm.ECDSA256(_, null)).valueOr(err => sys.error(s"Failed to create ECDSA256 verifier: $err")))))))
    parser
      .opt[String]("auth-jwt-es512-crt")
      .optional()
      .validate(v => Either.cond(v.length > 0, (), "Certificate file path must be a non-empty string"))
      .text("Enables JWT-based authorization, where the JWT is signed by ECDSA512 with a public key loaded from the given X509 certificate file (.crt)")
      .action((path, config) => config.copy(extra = config.extra.copy(
        authService = Some(AuthServiceJWT(
          ECDSAVerifier.fromCrtFile(path, Algorithm.ECDSA512(_, null)).valueOr(err => sys.error(s"Failed to create ECDSA512 verifier: $err")))))))
    parser
      .opt[String]("auth-jwt-rs256-jwks")
      .optional()
      .validate(v => Either.cond(v.length > 0, (), "JWK server URL must be a non-empty string"))
      .text("Enables JWT-based authorization, where the JWT is signed by RSA256 with a public key loaded from the given JWKS URL")
      .action((url, config) => config.copy(extra = config.extra.copy(authService = Some(AuthServiceJWT(JwksVerifier(url))))))

    parser
        .opt[Long]("pre-execution-cost-threshold")
        .optional()
        .text("Controls the interpretation cost threshold based on which requests are marked or not for pre-execution. Default is that we don't pre-execute any transactions.")
      .action((preExecutionCostThreshold, config) => {
        config.copy(extra = config.extra.copy(preExecutionCostThreshold = Some(preExecutionCostThreshold)))
      })
    parser
      .opt[Map[String, String]]("batching")
      .optional()
      .text("Parameters for batching of submissions. The optional keys are [enable, max-queue-size, max-batch-size-bytes, max-wait-millis, max-concurrent-commits].")
      .action({ case (kv, config) =>
        config.copy(
          extra = config.extra.copy(
            enableBatching = kv.get("enable") match {
              case Some("true") => true
              case Some("false") => false
              case None => false
              case _ => sys.error("enable should be 'true' or 'false'")
            },
            maxBatchQueueSize = kv.get("max-queue-size").map(_.toInt).getOrElse(config.extra.maxBatchQueueSize),
            maxBatchSizeBytes = kv.get("max-batch-size-bytes").map(_.toLong).getOrElse(config.extra.maxBatchSizeBytes),
            maxBatchWaitDuration = kv.get("max-wait-millis").map(millis => Duration(millis.toInt, MILLISECONDS)).getOrElse(config.extra.maxBatchWaitDuration),
            maxBatchConcurrentCommits = kv.get("max-concurrent-commits").map(_.toInt).getOrElse(config.extra.maxBatchConcurrentCommits)
          )
        )
      })
  }
}