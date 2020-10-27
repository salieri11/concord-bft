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
import com.digitalasset.daml.on.vmware.write.service.RetryStrategy
import com.digitalasset.daml.on.vmware.write.service.bft.{
  BftConcordClientPool,
  ConstantRequestTimeout,
  LinearAffineInterpretationTimeTransform,
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
    configPath: Path,
    requestTimeoutStrategy: RequestTimeoutStrategy,
    initRetryStrategy: RetryStrategy,
    sendRetryStrategy: RetryStrategy,
)

object BftClientConfig {
  val DefaultRequestTimeoutStrategy: LinearAffineInterpretationTimeTransform =
    LinearAffineInterpretationTimeTransform.ReasonableDefault
  val DefaultInitRetryStrategy: RetryStrategy =
    RetryStrategy.exponentialBackoff(
      retries = 8,
      firstWaitTime = 100.millis,
    ) // ~ 26 secs maximum
  val DefaultSendRetryStrategy: RetryStrategy =
    RetryStrategy.exponentialBackoff(
      retries = 10,
      firstWaitTime = 100.millis,
      _ == BftConcordClientPool.OverloadedException,
    ) // ~ 102 secs maximum

  def withReasonableDefaults(configPath: Path): BftClientConfig = BftClientConfig(
    configPath = configPath,
    requestTimeoutStrategy = DefaultRequestTimeoutStrategy,
    initRetryStrategy = DefaultInitRetryStrategy,
    sendRetryStrategy = DefaultSendRetryStrategy,
  )
}

final case class ExtraConfig(
    override val replicas: Seq[String],
    useThinReplica: Boolean,
    insecureThinReplicaClient: Boolean,
    thinReplicaTlsCertPath: String,
    override val maxFaultyReplicas: Short,
    jaegerAgentAddress: String,
    authService: Option[AuthService],
    preExecutionTimeThreshold: Option[Duration],
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
  val DefaultBftClientConfigPath: Path = Paths.get("/concord/config-public/bftclient.config")

  val ReasonableDefault: ExtraConfig = ExtraConfig(
    replicas = Seq("localhost:50051"),
    useThinReplica = false,
    insecureThinReplicaClient = true,
    thinReplicaTlsCertPath = "",
    maxFaultyReplicas = 1,
    jaegerAgentAddress = "localhost:6831",
    authService = Some(AuthServiceWildcard),
    preExecutionTimeThreshold = None,
    enableBatching = false,
    maxBatchQueueSize = 100,
    maxBatchSizeBytes = 4 * 1024 * 1024 /* 4MB */,
    maxBatchWaitDuration = 100.millis,
    maxBatchConcurrentCommits = 5,
    maxTrcReadDataTimeout = 0,
    maxTrcReadHashTimeout = 0,
    bftClient = BftClientConfig.withReasonableDefaults(DefaultBftClientConfigPath),
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
      .opt[Duration]("pre-execution-time-threshold")
      .optional()
      .text("Controls the interpretation time threshold based on which requests are marked or not for pre-execution. Default is that we don't pre-execute any transactions.")
      .action((preExecutionTimeThreshold, config) => {
        config.copy(
          extra = config.extra.copy(preExecutionTimeThreshold = Some(preExecutionTimeThreshold)))
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
    parser
      .opt[Unit]("insecure-thin-replica-client")
      .optional()
      .action((_, config) =>
        config.copy(extra = config.extra.copy(insecureThinReplicaClient = false)))
      .text("Use mTLS for thin replica client and thrn replica server communication.")
    parser
      .opt[String]("thin-replica-tls-cert-path")
      .optional()
      .action { (path, config) =>
        config.copy(extra = config.extra.copy(thinReplicaTlsCertPath = path))
      }
      .text("Provide directory for all the trc-trs certs.")
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
    val bftClientDefaultConfig = ExtraConfig.ReasonableDefault.bftClient
    parser
      .opt[BftClientConfig]("bft-client")
      .optional()
      .action { (bftClientConfig, config) =>
        config.withBftClientConfig(bftClientConfig)
      }
      .text(
        s"Configure the BFT Client. The accepted keys are " +
          s"'$ConfigPathKey' (default = ${bftClientDefaultConfig.configPath}) " +
          s"as well as timeout strategy and send retry strategy keys " +
          s"(use --bft-timeout-strategies, --bft-init-retry-strategies or --bft-send-retry-strategies for more info).")

    parser
      .opt[Unit]("bft-timeout-strategies")
      .optional()
      .action((_, _) => {
        val linearDefault = BftClientConfig.DefaultRequestTimeoutStrategy
        println(
          s"""The supported BFT client request timeout strategies are:
            |
            | * Linear transform in the estimated interpretation time (default); keys:
            |   - $LinearTimeoutSlopeKey (default value: ${linearDefault.slope})
            |   - $LinearTimeoutInterceptKey (default value: ${linearDefault.intercept})
            |   - $LinearTimeoutDefaultKey (default value: ${durationToCommandlineText(linearDefault.defaultTimeout)})
            |
            | * Constant timeout; keys:
            |   - $ConstantTimeoutKey (default value: ${durationToCommandlineText(ConstantRequestTimeout.ReasonableDefault.defaultTimeout)})""".stripMargin)
        sys.exit(0)
      })
      .text("Prints the BFT client request timeout strategies and exits.")

    parser
      .opt[Unit]("bft-send-retry-strategies")
      .optional()
      .action((_, _) => {
        println(
          s"""BFT client send retry strategies determine how the Ledger API Server retries sending through the BFT client if its resources are exhausted. They can be selected through the '$SendRetryStrategyKey' key that supports the following values:
             |
             | * $ExponentialBackOff (default; the multiplier is fixed to ${RetryStrategy.ExponentialBackoffMultiplier})
             | * $ConstantWaitTime
             |
             |All strategies also support the additional '$SendRetryFirstWaitKey' (default value: ${durationToCommandlineText(BftClientConfig.DefaultSendRetryStrategy.firstWaitTime)}) and '$SendRetriesKey' (default value: ${BftClientConfig.DefaultSendRetryStrategy.retries}) keys.""".stripMargin)
        sys.exit(0)
      })
      .text("Prints the BFT client send retry strategies and exits.")

    parser
      .opt[Unit]("bft-init-retry-strategies")
      .optional()
      .action((_, _) => {
        println(
          s"""BFT init retry strategies determine how the Ledger API Server retries initializing the BFT client, if not yet available, at bootstrap time. They can be selected through the '$InitRetryStrategyKey' key that supports the following values:
             |
             | * $ExponentialBackOff (default; the multiplier is fixed to ${RetryStrategy.ExponentialBackoffMultiplier})
             | * $ConstantWaitTime
             |
             |All strategies also support the additional '$InitRetryFirstWaitKey' (default value: ${durationToCommandlineText(BftClientConfig.DefaultInitRetryStrategy.firstWaitTime)}) and '$InitRetriesKey' (default value: ${BftClientConfig.DefaultInitRetryStrategy.retries}) keys.""".stripMargin)
        sys.exit(0)
      })
      .text("Prints the BFT client init retry strategies and exits.")

    addDeprecatedBftClientCommandLineArguments(parser)
  }

  private def addDeprecatedBftClientCommandLineArguments(parser: OptionParser[Config[ExtraConfig]]) = {
    parser
      .opt[Unit]("use-bft-client")
      .optional()
      .action { (_, config) => config }
      .text("Deprecated parameter -- BFT client is enabled by default.")
    parser
      .opt[Path]("bft-client-config-path")
      .optional()
      .action { (path, config) =>
        config.withBftClientConfig(configPath = path)
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
      configPath: Path = config.extra.bftClient.configPath,
      requestTimeoutComputation: RequestTimeoutStrategy = config.extra.bftClient.requestTimeoutStrategy,
    ): Config[ExtraConfig] =
      config.copy(
        extra = config.extra.copy(bftClient = config.extra.bftClient.copy(configPath = configPath, requestTimeoutStrategy = requestTimeoutComputation)))

    def withBftClientConfig(bftClientConfig: BftClientConfig): Config[ExtraConfig] =
      config.copy(
        extra = config.extra.copy(bftClient = bftClientConfig))
  }

  private implicit val pathReader: Read[Path] =
    Read.reads(Paths.get(_))

  private val EnableKey = "enable"
  private val ConfigPathKey = "config-path"
  private val SendRetryStrategyKey = "send-retry-strategy"
  private val SendRetriesKey = "send-retries"
  private val SendRetryFirstWaitKey = "send-retry-first-wait"
  private val InitRetryStrategyKey = "init-retry-strategy"
  private val InitRetriesKey = "init-retries"
  private val InitRetryFirstWaitKey = "init-retry-first-wait"

  private val GeneralParametersKeySet =
    Set(EnableKey, ConfigPathKey, SendRetryStrategyKey, SendRetriesKey, SendRetryFirstWaitKey, InitRetryStrategyKey, InitRetriesKey, InitRetryFirstWaitKey)

  private val ConstantTimeoutKey = "constant-timeout"
  private val LinearTimeoutSlopeKey = "linear-timeout-slope"
  private val LinearTimeoutInterceptKey = "linear-timeout-intercept"
  private val LinearTimeoutDefaultKey = "linear-timeout-default"

  private val LinearTimeoutKeysSet = Set(LinearTimeoutSlopeKey, LinearTimeoutInterceptKey, LinearTimeoutDefaultKey)

  private val ConstantWaitTime = "constant-wait-time"
  private val ExponentialBackOff = "exponential-backoff"

  private implicit val bftClientConfigReader: Read[BftClientConfig] =
    Read.mapRead[String, String].map { keyValuePairs =>
      verifyBftClientConfigurationKeysOrThrow(keyValuePairs)
      readBftClientTimeoutKeys(
        keyValuePairs,
        readOtherParameters(
          keyValuePairs,
          ExtraConfig.ReasonableDefault.bftClient,
        ),
      )
    }

  private def readBftClientTimeoutKeys(keyValuePairs: Map[String, String], config: BftClientConfig): BftClientConfig = {
    val configKeys = keyValuePairs.keySet
    if (configKeys.intersect(LinearTimeoutKeysSet).nonEmpty) {
      val linearTimeoutStrategy = keyValuePairs.filterKeys(LinearTimeoutKeysSet.contains)
        .foldLeft(LinearAffineInterpretationTimeTransform.ReasonableDefault) {
          case (strategy, (LinearTimeoutSlopeKey, value)) =>
            strategy.copy(slope = Read.doubleRead.reads(value))
          case (strategy, (LinearTimeoutInterceptKey, value)) =>
            strategy.copy(intercept = Read.durationRead.reads(value))
          case (strategy, (LinearTimeoutDefaultKey, value)) =>
            strategy.copy(defaultTimeout = Read.durationRead.reads(value))
        }
      config.copy(requestTimeoutStrategy = linearTimeoutStrategy)
    } else if (configKeys.contains(ConstantTimeoutKey)) {
      val constantTimeoutStrategy = keyValuePairs.get(ConstantTimeoutKey).map { value =>
        ConstantRequestTimeout(Read.durationRead.reads(value))
      }.getOrElse(ConstantRequestTimeout.ReasonableDefault)
      config.copy(requestTimeoutStrategy = constantTimeoutStrategy)
    } else {
      config
    }
  }

  private def readOtherParameters(keyValuePairs: Map[String, String], config: BftClientConfig): BftClientConfig =
    keyValuePairs.filterKeys(GeneralParametersKeySet.contains)
      .foldLeft(config) {
        case (config, (EnableKey, _)) =>
          println(s"'$EnableKey' key for --bft-client parameter is deprecated as BFT client is enabled by default.")
          config
        case (config, (ConfigPathKey, value)) =>
          config.copy(configPath = pathReader.reads(value))
        case (config, (SendRetriesKey, value)) =>
          val retries = Read.intRead.reads(value)
          config.copy(sendRetryStrategy = updateRetryStrategy(config.sendRetryStrategy, retries = Some(retries)))
        case (config, (SendRetryFirstWaitKey, value)) =>
          val waitTime = Read.durationRead.reads(value)
          config.copy(sendRetryStrategy = updateRetryStrategy(config.sendRetryStrategy, firstWaitTime = Some(waitTime)))
        case (config, (SendRetryStrategyKey, value)) =>
          config.copy(sendRetryStrategy = updateRetryStrategy(config.sendRetryStrategy, progression = Some(parseProgression(value))))
        case (config, (InitRetriesKey, value)) =>
          val retries = Read.intRead.reads(value)
          config.copy(initRetryStrategy = updateRetryStrategy(config.initRetryStrategy, retries = Some(retries)))
        case (config, (InitRetryFirstWaitKey, value)) =>
          val waitTime = Read.durationRead.reads(value)
          config.copy(initRetryStrategy = updateRetryStrategy(config.initRetryStrategy, firstWaitTime = Some(waitTime)))
        case (config, (InitRetryStrategyKey, value)) =>
          config.copy(initRetryStrategy = updateRetryStrategy(config.initRetryStrategy, progression = Some(parseProgression(value))))
      }

  private def parseProgression(value: String): Duration => Duration =
    value match {
      case ConstantWaitTime => identity
      case ExponentialBackOff => RetryStrategy.exponentialBackoffProgression
    }

  private def updateRetryStrategy(
    retryStrategy: RetryStrategy,
    retries: Option[Int] = None,
    firstWaitTime: Option[Duration] = None,
    progression: Option[Duration => Duration] = None,
  ): RetryStrategy = {
    val updatedProgression = progression.getOrElse(retryStrategy.progression)
    val updatedRetries = retries.getOrElse(retryStrategy.retries)
    val updatedFirstWaitTime = firstWaitTime.getOrElse(retryStrategy.firstWaitTime)
    retryStrategy.copy(
      retries = updatedRetries,
      firstWaitTime = updatedFirstWaitTime,
      progression = updatedProgression,
      waitTimeCap = if (RetryStrategy.isProgressionConstant(updatedProgression))
        updatedFirstWaitTime
      else
        RetryStrategy.exponentialBackoffWaitTimeCap(updatedRetries, updatedFirstWaitTime)
    )
  }

  private def verifyBftClientConfigurationKeysOrThrow(keyValuePairs: Map[String, String]): Unit = {
    val configKeys = keyValuePairs.keySet
    if (configKeys.intersect(LinearTimeoutKeysSet).nonEmpty && configKeys.contains(ConstantTimeoutKey)) {
      throwRequestStrategyKeysCannotBeMixed()
    }
    val invalidKeys = configKeys -- GeneralParametersKeySet -- LinearTimeoutKeysSet -- Set(ConstantTimeoutKey)
    if (invalidKeys.nonEmpty) {
      throw new IllegalArgumentException(s"Unsupported BFT client configuration key(s): ${invalidKeys.mkString(", ")}.")
    }
  }

  private def durationToCommandlineText(defaultTimeout: Duration) = s"${defaultTimeout.toMillis}ms"

  def throwRequestStrategyKeysCannotBeMixed(): Nothing =
    throw new IllegalArgumentException("Request timeout strategy keys cannot be mixed.")
}
