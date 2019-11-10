/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerHttpClient
import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.logging.warn
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.service.grpc.support.newClientRpcChannel
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.v1.ConcordComponent
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification
import com.vmware.blockchain.deployment.v1.ConfigurationServiceStub
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier
import com.vmware.blockchain.deployment.v1.Credential
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.FleetControlServiceStub
import com.vmware.blockchain.deployment.v1.FleetMessage
import com.vmware.blockchain.deployment.v1.InstanceMessage
import com.vmware.blockchain.deployment.v1.MessageHeader
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDateTime
import java.time.ZoneOffset
import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.awaitAll
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

/** Default configuration file path.  */
private val DEFAULT_APPLICATION_CONFIG = URI.create("file:/config/agent/config.json")

/** Default component artifact target mount path. */
private val DEFAULT_COMPONENT_CONFIGURATION_MOUNT_PATH = URI.create("file:$CONFIGURATION_MOUNT_PATH")

/** Default [ConcordAgentConfiguration] model. */
private val DEFAULT_CONCORD_AGENT_CONFIGURATION by lazy {
    val model = ConcordModelSpecification(
            "version",
            "template",
            listOf(
                    ConcordComponent(
                            ConcordComponent.Type.CONTAINER_IMAGE,
                            ConcordComponent.ServiceType.CONCORD,
                            "vmwblockchain/concord-core:latest"
                    ),
                    ConcordComponent(
                            ConcordComponent.Type.CONTAINER_IMAGE,
                            ConcordComponent.ServiceType.ETHEREUM_API,
                            "vmwblockchain/ethrpc:latest"
                    )
            )
    )
    val registryEndpoint = DockerHttpClient.DEFAULT_CONTAINER_REGISTRY
    val fleetServiceEndpoint = Endpoint("localhost:9004", Credential())
    val configServiceEndpoint = Endpoint("localhost:9003", Credential())

    ConcordAgentConfiguration(
            model = model,
            containerRegistry = registryEndpoint,
            fleetService = fleetServiceEndpoint,
            cluster = ConcordClusterIdentifier.defaultValue,
            node = 0,
            configService = configServiceEndpoint,
            configurationSession = ConfigurationSessionIdentifier.defaultValue
    )
}

/** Default STATUS message sending interval. */
private const val DEFAULT_HEARTBEAT_PERIOD_MS = 1000L

/**
 * Implementation of Persephone Agent.
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

    /**
     * Start the Persephone agent.
     */
    fun start() {
        launch(coroutineContext) {
            // Each iteration of an established session does not propagate failure to parent.
            supervisorScope {
                // One-time fetch this node's configuration.
                prepareNodeConfiguration()

                val services = configuration.model.components
                        .groupBy { it.serviceType }
                        .mapValues {
                            when (it.key) {
                                // Note: GENERIC controller is currently used for controlling agent.
                                ConcordComponent.ServiceType.GENERIC -> AgentServiceController
                                else -> DefaultServiceController(
                                        it.key,
                                        it.value,
                                        configuration.containerRegistry
                                )
                            }
                        }

                // Initialize agent service first and wait for initialization completion.
                services[ConcordComponent.ServiceType.GENERIC]?.apply { initializeAsync().await() }

                // Initialize all services and wait for all to complete.
                services.filterNot { it.key == ConcordComponent.ServiceType.GENERIC }
                        .values
                        .map { it.initializeAsync() }
                        .awaitAll()

                // Start each service iteratively.
                services.forEach { it.value.start() }

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
            val channel = configuration.fleetService.newClientRpcChannel()

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

    /**
     * Retrieve the configuration artifacts for the local service components that this agent
     * instance is responsible for controlling.
     */
    private suspend fun prepareNodeConfiguration() {
        // Fetch the configuration.
        val channel = configuration.configService.newClientRpcChannel()
        val configService = ConfigurationServiceStub(channel)
        val receiver = ChannelStreamObserver<NodeConfigurationResponse>()
        val receiverChannel = receiver.asReceiveChannel()
        configService.getNodeConfiguration(
                request = NodeConfigurationRequest(
                        identifier = configuration.configurationSession,
                        node = configuration.node
                ),
                responseObserver = receiver
        )
        val nodeConfiguration = receiverChannel.receive()
        for (component in nodeConfiguration.configurationComponent) {
            val url = URI.create(component.componentUrl)

            // Prepare component according to scheme (scheme-less is treated as file:///).
            when (url.scheme) {
                "file", null -> {
                    val path = Path.of(DEFAULT_COMPONENT_CONFIGURATION_MOUNT_PATH.path, url.path)

                    // Ensure the prefix path.
                    path.parent.toFile().mkdirs()

                    // Write the artifact to path.
                    path.toFile().writeText(component.component)

                    log.info { "Component prepared at path($path), service(${component.type})" }
                }
                else -> {
                    log.info { "Component has non-local target($url), service(${component.type})" }
                }
            }
        }

        // Config service channel is single-use, so tear it down when done.
        channel.shutdown()
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
        (Files.exists(Path.of(DEFAULT_APPLICATION_CONFIG))) -> {
            val configJson = Path.of(DEFAULT_APPLICATION_CONFIG).toUri().toURL().readText()
            json.parse(ConcordAgentConfiguration.getSerializer(), configJson)
        }
        else -> DEFAULT_CONCORD_AGENT_CONFIGURATION
    }

    val application = Application(configuration)
    try {
        log.info { "Starting Persephone Agent" }
        application.start()
        Runtime.getRuntime().addShutdownHook(Thread {
            // Use stderr here since the logger may have been reset by its JVM shutdown hook.
            println("Shutting down Persephone Agent since JVM is shutting down")
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
