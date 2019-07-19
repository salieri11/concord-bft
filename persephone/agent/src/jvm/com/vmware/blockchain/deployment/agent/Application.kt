/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerClient
import com.vmware.blockchain.deployment.agent.docker.DockerOrchestrator
import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.logging.warn
import com.vmware.blockchain.deployment.model.ConcordAgentConfiguration
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.model.ConcordComponent
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.model.Credential
import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.FleetControlServiceStub
import com.vmware.blockchain.deployment.model.FleetMessage
import com.vmware.blockchain.deployment.model.InstanceMessage
import com.vmware.blockchain.deployment.model.MessageHeader
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import io.grpc.ManagedChannelBuilder
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDateTime
import java.time.ZoneOffset
import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.selects.select
import kotlinx.coroutines.supervisorScope
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.EmptyModule

/** Default server port number.  */
private val CONCORD_MODEL_URI = URI.create("file:/config/config.json")

/** Default [ConcordAgentConfiguration] model. */
private val DEFAULT_CONCORD_AGENT_CONFIGURATION by lazy {
    val model = ConcordModelSpecification(
            "version",
            "template",
            listOf(
                    ConcordComponent(
                            ConcordComponent.Type.CONTAINER_IMAGE,
                            ConcordComponent.ServiceType.CONCORD,
                            "registry-1.docker.io/vmwblockchain/concord-core:latest"
                    ),
                    ConcordComponent(
                            ConcordComponent.Type.CONTAINER_IMAGE,
                            ConcordComponent.ServiceType.ETHEREUM_API,
                            "registry-1.docker.io/vmwblockchain/ethrpc:latest"
                    )
            )
    )
    val registryEndpoint = DockerClient.DEFAULT_CONTAINER_REGISTRY
    val fleetEndpoint = Endpoint("localhost:9004", Credential())

    ConcordAgentConfiguration(
            model,
            registryEndpoint,
            fleetEndpoint,
            ConcordClusterIdentifier.defaultValue,
            ConcordNodeIdentifier.defaultValue
    )
}

/** Default STATUS message sending interval. */
private const val DEFAULT_HEARTBEAT_PERIOD_MS = 1000L

/**
 * Implementation of Concord Agent.
 */
class Application(private val configuration: ConcordAgentConfiguration) : CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = Dispatchers.Default + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = Job()

    /** [StatusReporter] instance for all deployed components associated with this agent. */
    private val statusCollector: StatusReporter = StatusReporter()

    /** [DockerOrchestrator] instance to be used for all container orchestration actions. */
    private val orchestrator: DockerOrchestrator = DockerOrchestrator(
            docker = DockerClient(
                    DockerClient.Context(
                            DockerClient.DEFAULT_DOCKER_ENGINE,
                            configuration.containerRegistry
                    )
            )
    )

    /**
     * Start the Concord agent.
     */
    fun start() {
        launch(coroutineContext) {
            // Each iteration of an established session does not propagate failure to parent.
            supervisorScope {
                val controllers = configuration.model.components
                        .groupBy { it.serviceType }
                        .mapValues { ServiceController(orchestrator, it.value) }

                while (isActive) {
                    // Start a new session and wait for the session coroutine to close.
                    establishNewSession()

                    // Note: Alternative is yield() without delay. The point is to let other
                    // tasks run before jumping into the next cycle, as well as giving this main
                    // coroutine loop a chance to catch cancellation signal.
                    // Introducing a delay also lets the fleet service to not be overwhelmed in
                    // pathological cases where an agent is retrying over and over without pause.
                    delay(1000)
                }
            }
        }
    }

    /**
     * Suspend the caller until this instance is completely shutdown with all child tasks exited.
     */
    suspend fun awaitShutdown() {
        job.join()
    }

    /**
     * Establish a new monitored session with the fleet service.
     */
    private suspend fun establishNewSession() {
        coroutineScope {
            val sessionId = "session-${LocalDateTime.now().toEpochSecond(ZoneOffset.UTC)}"

            // Continuously establish session with fleet unless told to go away.
            val channel = ManagedChannelBuilder
                    .forTarget(configuration.fleetService.address)
                    .usePlaintext()
                    .build()

            // Create a stub with a channel and call to create a new managed session.
            val fleetService = FleetControlServiceStub(channel)
            val receiver = ChannelStreamObserver<FleetMessage>()
            val receiverChannel = receiver.asReceiveChannel()
            val observer = fleetService.createManagedSession(receiver)

            // Send the session request.
            val createSessionRequest = InstanceMessage(
                    header = MessageHeader(id = sessionId),
                    type = InstanceMessage.Type.CREATE_SESSION_REQUEST,
                    sessionRequest = InstanceMessage.CreateSessionRequest(
                            instanceId = "instance-id",
                            cluster = configuration.cluster,
                            node = configuration.node
                    )
            )
            observer.onNext(createSessionRequest)

            // Launch periodic status sender.
            val statusChannel = Channel<InstanceMessage>()
            val heartBeatJob = launch {
                while (true) {
                    val timestamp = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC).toString()
                    val currentStatus = InstanceMessage(
                            header = MessageHeader(id = timestamp),
                            type = InstanceMessage.Type.STATUS,
                            status = statusCollector.currentStatus()
                    )

                    // Send on the internal channel to queue up for outbound message.
                    statusChannel.send(currentStatus)

                    // Send on 1-second interval.
                    delay(DEFAULT_HEARTBEAT_PERIOD_MS)
                }
            }.apply {
                invokeOnCompletion {
                    log.info { "Session($sessionId} heart beat stopped" }
                }
            }

            try {
                // Start pulling from receiver for fleet's messages and self-
                while (true) {
                    // Since gRPC's StreamObserver is not thread-safe, serialize among the actions
                    // that may cause outbound messages using a select() construct.
                    select<Unit> {
                        receiverChannel.onReceive { message ->
                            log.info { "Received ${message.type}" }

                            when (message.type) {
                                FleetMessage.Type.CREATE_SESSION_RESPONSE -> {}
                                FleetMessage.Type.GET_CONFIGURATION -> {}
                                FleetMessage.Type.GET_STATUS -> {}
                                FleetMessage.Type.UPDATE_SERVICE_STATE_REQUEST -> {
                                }
                                else -> {
                                    log.warn { "Unexpected message type(${message.type})" }
                                }
                            }
                        }
                        statusChannel.onReceive { observer.onNext(it) }
                    }
                }
            } catch (error: Throwable) {
                // Do not let any errors escape, stays up until explicit shutdown.
                log.error { "Session ended with error, message(${error.message})" }
            } finally {
                heartBeatJob.cancelAndJoin()
            }
        }
    }
}

/**
 * Main entry point for the application instance.
 *
 * @param[args]
 *   server startup arguments from command-line.
 */
fun main(args: Array<String>) {
    // Obtain a logging instance.
    val log = logger(Application::class)

    // Construct server configuration from input parameters.
    val json = Json(JsonConfiguration.Stable.copy(encodeDefaults = false), EmptyModule)

    val configuration = when {
        (args.size == 1 && Files.exists(Path.of(args[0]))) -> {
            // Expect first parameter to be the URL path for configuration settings.
            val configJson = Path.of(args[0]).toUri().toURL().readText()
            json.parse(ConcordAgentConfiguration.getSerializer(), configJson)
        }
        (Files.exists(Path.of(CONCORD_MODEL_URI))) -> {
            val configJson = Path.of(CONCORD_MODEL_URI).toUri().toURL().readText()
            json.parse(ConcordAgentConfiguration.getSerializer(), configJson)
        }
        else -> DEFAULT_CONCORD_AGENT_CONFIGURATION
    }

    val application = Application(configuration)
    try {
        log.info { "Starting Concord Agent" }
        application.start()
        Runtime.getRuntime().addShutdownHook(Thread {
            // Use stderr here since the logger may have been reset by its JVM shutdown hook.
            println("Shutting down Concord Agent since JVM is shutting down")
            application.cancel(CancellationException("Process is shutting down"))
        })
    } catch (error: Throwable) {
        log.error { "Error encountered, message(${error.message})" }
        application.cancel(CancellationException("Error encountered", error))
    } finally {
        runBlocking {
            application.awaitShutdown()
        }
    }
}
