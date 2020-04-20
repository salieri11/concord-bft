package com.digitalasset.daml.on.vmware.ledger.api.server

import java.io.File
import java.nio.file.Path

import com.auth0.jwt.algorithms.Algorithm
import com.daml.ledger.participant.state.kvutils.app.Config
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.lf.data.Ref
import com.daml.lf.data.Ref.IdString
import com.daml.jwt.{ECDSAVerifier, HMAC256Verifier, JwksVerifier, RSA256Verifier}
import com.daml.ledger.api.auth.{AuthService, AuthServiceJWT, AuthServiceWildcard}
import scopt.{OptionParser, Read}
import scala.concurrent.duration._

final case class ExtraConfig(
    maxInboundMessageSize: Int,
    replicas: Seq[String],
    useThinReplica: Boolean,
    maxFaultyReplicas: Short,
    jaegerAgentAddress: String,
    authService: Option[AuthService],
    enableBatching: Boolean, // Whether we're batching requests or not.
    maxBatchQueueSize: Int, // Number of submissions we're willing to queue before dropping.
    maxBatchSizeBytes: Long, // The maximum size for a batch before it is forcefully sent.
    maxBatchWaitDuration: FiniteDuration, // Maximum duration we're willing to wait to fill a batch.
    maxBatchConcurrentCommits: Int
)

object ExtraConfig {
  val DefaultParticipantId: IdString.ParticipantId =
    ParticipantId.assertFromString("standalone-participant")

  val Default = ExtraConfig(
    maxInboundMessageSize = Config.DefaultMaxInboundMessageSize,
    replicas = Seq("localhost:50051"),
    useThinReplica = false,
    maxFaultyReplicas = 1,
    jaegerAgentAddress = "localhost:6831",
    authService = Some(AuthServiceWildcard),
    enableBatching = false,
    maxBatchQueueSize = 100,
    maxBatchSizeBytes = 4 * 1024 * 1024 /* 4MB */,
    maxBatchWaitDuration = 100.millis,
    maxBatchConcurrentCommits = 5,
  )

  def addCommandLineArguments(parser: OptionParser[Config[ExtraConfig]]): Unit = {
    parser
      .opt[Unit]("version")
      .optional()
      .action((_, _) => {
        println(BuildInfo.Version);
        sys.exit(0)
      })
      .text("Prints the version on stdout and exit.")
    parser
      .opt[Int]("maxInboundMessageSize")
      .action(
        (size, config) => config.copy(extra = config.extra.copy(maxInboundMessageSize = size)))
      .text(
        s"Max inbound message size in bytes. Defaults to ${Config.DefaultMaxInboundMessageSize}.")

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
      .opt[Int]("max-faulty-replicas")
      .optional()
      .action((maxFaultyReplicas, config) =>
        config.copy(extra = config.extra.copy(maxFaultyReplicas = maxFaultyReplicas.toShort)))
      .text("Maximum number of faulty replicas that thin replica client should tolerate.")
    parser
      .opt[String]("jaeger-agent-address")
      .optional()
      .text(
        s"The address of the Jaeger agent in <HOST:PORT> format. Defaults to ${ExtraConfig.Default.jaegerAgentAddress}.")
      .action((hostAndPort, config) =>
        config.copy(extra = config.extra.copy(jaegerAgentAddress = hostAndPort)))

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

    // Deprecated arguments -- these cannot be added right now as kvutils-app requires at least one
    // --participant argument to be specified.
    parser
      .opt[Int]("port")
      .optional()
      .action(deprecatedParameter[Int]("port"))
      .text("[DEPRECATED] Server port. If not set, a random port is allocated.")
    parser
      .opt[File]("port-file")
      .optional()
      .action(deprecatedParameter[File]("port-file"))
      .text("[DEPRECATED] File to write the allocated port number to. Used to inform clients in CI about the allocated port.")
    parser
      .opt[String]("jdbc-url")
      .text(s"[DEPRECATED] The JDBC URL to the postgres database used for the indexer and the index.")
      .action(deprecatedParameter[String]("jdbc-url"))

    implicit val participantIdRead: Read[Ref.ParticipantId] =
      Read.stringRead.map(Ref.ParticipantId.assertFromString)
    parser
      .opt[Ref.ParticipantId]("participant-id")
      .optional()
      .text(s"[DEPRECATED] The participant id given to all components of a ledger api server. Defaults to ${ExtraConfig.DefaultParticipantId}")
      .action(deprecatedParameter[ParticipantId]("participant-id"))


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

  private def deprecatedParameter[T](name: String)(value: T, config: Config[ExtraConfig]): Config[ExtraConfig] =
    sys.error("Parameter '--$name' is deprecated, see --help for correct usage on how to set up a participant via --participant parameter.")
}
