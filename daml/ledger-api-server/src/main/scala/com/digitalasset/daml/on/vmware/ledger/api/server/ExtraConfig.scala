// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.ledger.api.server

import java.nio.file.{Path, Paths}

import com.auth0.jwt.algorithms.Algorithm
import com.daml.buildinfo.BuildInfo
import com.daml.jwt.{ECDSAVerifier, HMAC256Verifier, JwksVerifier, RSA256Verifier}
import com.daml.ledger.api.auth.{AuthService, AuthServiceJWT, AuthServiceWildcard}
import com.daml.ledger.participant.state.kvutils.app.Config
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.lf.data.Ref.IdString
import com.digitalasset.daml.on.vmware.write.service.bft.{
  ConstantRequestTimeout,
  LinearAffineInterpretationCostTransform,
  RequestTimeoutStrategy
}
import scopt.{OptionParser, Read}

import scala.concurrent.duration._

sealed trait WriteClientsConfig {
  val replicas: Seq[String]
  val maxFaultyReplicas: Short
  val bftClient: BftClientConfig
}

final case class BftClientConfig(
    enable: Boolean, // Whether to use the BFT Concord Client Pool in the writer.
    configPath: Option[Path],
    requestTimeoutStrategy: RequestTimeoutStrategy,
)

object BftClientConfig {
  val ReasonableDefault: BftClientConfig = BftClientConfig(
    enable = false,
    configPath = None,
    requestTimeoutStrategy = LinearAffineInterpretationCostTransform.ReasonableDefault,
  )
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
    override val bftClient: BftClientConfig,
) extends WriteClientsConfig

object ExtraConfig {
  val DefaultParticipantId: IdString.ParticipantId =
    ParticipantId.assertFromString("standalone-participant")

  val ReasonableDefault: ExtraConfig = ExtraConfig(
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
    bftClient = BftClientConfig.ReasonableDefault,
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

    addThinReplicaClientCommandLineArguments(parser)
    addBftClientCommandLineArguments(parser)
    addAuthCommandLineArguments(parser)

    parser
      .opt[Long]("pre-execution-cost-threshold")
      .optional()
      .text("Controls the interpretation cost threshold based on which requests are marked or not for pre-execution. Default is that we don't pre-execute any transactions.")
      .action((preExecutionCostThreshold, config) => {
        config.copy(
          extra = config.extra.copy(preExecutionCostThreshold = Some(preExecutionCostThreshold)))
      })
    parser
      .opt[Map[String, String]]("batching")
      .optional()
      .text("Parameters for batching of submissions. The optional keys are [enable, max-queue-size, max-batch-size-bytes, max-wait-millis, max-concurrent-commits].")
      .action({
        case (kv, config) =>
          config.copy(
            extra = config.extra.copy(
              enableBatching = kv.get("enable") match {
                case Some("true") => true
                case Some("false") => false
                case None => false
                case _ => sys.error("enable should be 'true' or 'false'")
              },
              maxBatchQueueSize =
                kv.get("max-queue-size").map(_.toInt).getOrElse(config.extra.maxBatchQueueSize),
              maxBatchSizeBytes = kv
                .get("max-batch-size-bytes")
                .map(_.toLong)
                .getOrElse(config.extra.maxBatchSizeBytes),
              maxBatchWaitDuration = kv
                .get("max-wait-millis")
                .map(millis => Duration(millis.toInt, MILLISECONDS))
                .getOrElse(config.extra.maxBatchWaitDuration),
              maxBatchConcurrentCommits = kv
                .get("max-concurrent-commits")
                .map(_.toInt)
                .getOrElse(config.extra.maxBatchConcurrentCommits)
            )
          )
      })
  }

  private def addThinReplicaClientCommandLineArguments(
      parser: OptionParser[Config[ExtraConfig]]): Unit = {
    parser
      .opt[Seq[String]]("replicas")
      .optional()
      .action((replicas, config) => config.copy(extra = config.extra.copy(replicas = replicas)))
      .valueName("<IP:PORT>,<IP:PORT>,...")
      .text(
        s"List of replicas (<IP:PORT>). Initially, the server connects to the first replica in the list. Defaults to ${ExtraConfig.ReasonableDefault.replicas}.")
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
        s"The address of the Jaeger agent in <HOST:PORT> format. Defaults to ${ExtraConfig.ReasonableDefault.jaegerAgentAddress}.")
      .action((hostAndPort, config) =>
        config.copy(extra = config.extra.copy(jaegerAgentAddress = hostAndPort)))
  }

  private def addAuthCommandLineArguments(parser: OptionParser[Config[ExtraConfig]]): Unit = {
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
  }

  private def addBftClientCommandLineArguments(parser: OptionParser[Config[ExtraConfig]]): Unit = {
    //
    // BFT concord client pool
    //
    val bftClientDefaultConfig = BftClientConfig.ReasonableDefault
    parser
      .opt[BftClientConfig]("bft-client")
      .optional()
      .action { (bftClientConfig, config) =>
        config.withBftClientConfig(bftClientConfig)
      }
      .text(
        s"Enable and configure the BFT Client. " +
          s"The optional keys are '$EnableKey' (default = ${bftClientDefaultConfig.enable}), " +
          s"'$ConfigPathKey' (default = ${
            bftClientDefaultConfig.configPath
              .getOrElse("<not defined>")
          }) " +
          s"and timeout strategy keys (use --bft-timeout-strategies for more info).")

    parser
      .opt[Unit]("bft-timeout-strategies")
      .optional()
      .action((_, _) => {
        val linearDefault = LinearAffineInterpretationCostTransform.ReasonableDefault
        println(
          s"""The supported BFT client request timeout strategies are:
            |
            | * Constant timeout; key: $ConstantTimeoutKey (default value: ${durationToCommandlineText(ConstantRequestTimeout.ReasonableDefault.defaultTimeout)})
            | * Linear transform in the estimated interpretation cost as milliseconds; keys:
            |   - $LinearTimeoutSlopeKey (default value: ${linearDefault.slope})
            |   - $LinearTimeoutInterceptKey (default value: ${linearDefault.intercept})
            |   - $LinearTimeoutDefaultKey  (default value: ${durationToCommandlineText(linearDefault.defaultTimeout)})""".stripMargin)
        sys.exit(0)
      })
      .text("Prints the BFT client request timeout strategies and exits.")

    addDeprecatedBftClientCommandLineArguments(parser)
  }

  private def addDeprecatedBftClientCommandLineArguments(parser: OptionParser[Config[ExtraConfig]]) = {
    parser
      .opt[Unit]("use-bft-client")
      .optional()
      .action { (_, config) =>
        config.withBftClientConfig(enable = true)
      }
      .text("Deprecated parameter -- please use --bft-client instead.")
    parser
      .opt[Path]("bft-client-config-path")
      .optional()
      .action { (path, config) =>
        config.withBftClientConfig(configPath = Some(path))
      }
      .text("Deprecated parameter -- please use --bft-client instead.")
    parser
      .opt[Duration]("bft-client-request-timeout")
      .optional()
      .action { (timeout, config) =>
        config.withBftClientConfig(
          requestTimeoutComputation =
            config.extra.bftClient.requestTimeoutStrategy.withDefaultTimeout(timeout))
      }
      .text("Deprecated parameter -- please use --bft-client instead.")
  }

  implicit class WithBftClientConfig(val config: Config[ExtraConfig]) extends AnyVal {
    def withBftClientConfig(
      enable: Boolean = config.extra.bftClient.enable,
      configPath: Option[Path] = config.extra.bftClient.configPath,
      requestTimeoutComputation: RequestTimeoutStrategy = config.extra.bftClient.requestTimeoutStrategy,
    ): Config[ExtraConfig] =
      config.copy(
        extra = config.extra.copy(bftClient = config.extra.bftClient.copy(enable = enable, configPath = configPath, requestTimeoutStrategy = requestTimeoutComputation)))

    def withBftClientConfig(bftClientConfig: BftClientConfig): Config[ExtraConfig] =
      config.copy(
        extra = config.extra.copy(bftClient = bftClientConfig))
  }

  private implicit val pathReader: Read[Path] =
    Read.reads(Paths.get(_))

  private val EnableKey = "enable"
  private val ConfigPathKey = "config-path"
  private val ConstantTimeoutKey = "constant-timeout"
  private val LinearTimeoutSlopeKey = "linear-timeout-slope"
  private val LinearTimeoutInterceptKey = "linear-timeout-intercept"
  private val LinearTimeoutDefaultKey = "linear-timeout-default"

  private val GeneralParametersKeySet = Set(EnableKey, ConfigPathKey)
  private val LinearTimeoutKeysSet = Set(LinearTimeoutSlopeKey, LinearTimeoutInterceptKey, LinearTimeoutDefaultKey)

  private implicit val bftClientConfigReader: Read[BftClientConfig] =
    Read.mapRead[String, String].map { keyValuePairs =>
      val configKeys = keyValuePairs.keySet
      verifyBftClientConfigurationKeysOrThrow(configKeys)
      val config = keyValuePairs.filterKeys(GeneralParametersKeySet.contains)
        .foldLeft(ExtraConfig.ReasonableDefault.bftClient) {
        case (config, (EnableKey, value)) => config.copy(enable = Read.booleanRead.reads(value))
        case (config, (ConfigPathKey, value)) =>
          config.copy(configPath = Some(pathReader.reads(value)))
        }
      if (configKeys.intersect(LinearTimeoutKeysSet).nonEmpty) {
        val linearTimeoutStrategy = keyValuePairs.filterKeys(LinearTimeoutKeysSet.contains)
          .foldLeft(LinearAffineInterpretationCostTransform.ReasonableDefault) {
            case (strategy, (LinearTimeoutSlopeKey, value)) =>
              strategy.copy(slope = Read.doubleRead.reads(value))
            case (strategy, (LinearTimeoutInterceptKey, value)) =>
              strategy.copy(intercept = Read.doubleRead.reads(value))
            case (strategy, (LinearTimeoutDefaultKey, value)) =>
              strategy.copy(defaultTimeout = Read.durationRead.reads(value))
          }
        config.copy(requestTimeoutStrategy = linearTimeoutStrategy)
      } else {
        val constantTimeoutStrategy = keyValuePairs.get(ConstantTimeoutKey).map { value =>
          ConstantRequestTimeout(Read.durationRead.reads(value))
        }
          .getOrElse(ConstantRequestTimeout.ReasonableDefault)
        config.copy(requestTimeoutStrategy = constantTimeoutStrategy)
      }
    }

  private def verifyBftClientConfigurationKeysOrThrow(configKeys: Set[String]): Unit = {
    if (configKeys.intersect(LinearTimeoutKeysSet).nonEmpty && configKeys.contains(ConstantTimeoutKey)) {
      throwRequestStrategyKeysCannotBeMixed()
    }
    val invalidKeys = configKeys -- GeneralParametersKeySet -- LinearTimeoutKeysSet -- Set(ConstantTimeoutKey)
    if (invalidKeys.nonEmpty) {
      throw new IllegalArgumentException(s"Unsupported BFT client configuration key(s): ${invalidKeys.mkString(", ")}.")
    }
  }

  private def durationToCommandlineText(defaultTimeout: Duration) = s"${defaultTimeout.toSeconds}s"

  def throwRequestStrategyKeysCannotBeMixed(): Nothing =
    throw new IllegalArgumentException("Request timeout strategy keys cannot be mixed.")
}
