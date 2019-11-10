/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerHttpClient
import com.vmware.blockchain.deployment.agent.docker.DockerClient
import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.v1.ConcordComponent
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.ServiceState
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.launch
import kotlin.coroutines.CoroutineContext

/** Name of the default container network that all components attach to. */
const val DEFAULT_CONTAINER_NETWORK_NAME = "blockchain-replica"

/**
 * Component that controls lifecycle actions for a set of [ConcordComponent]s of a particular
 * [ConcordComponent.ServiceType].
 */
interface ServiceController {
    /**
     * Initialize the controller instance.
     *
     * Note: Caller should not invoke initialize() more than once on a given controller instance.
     */
    fun initializeAsync(): Deferred<Boolean>

    /**
     * Send a service start command to the [ServiceController] for processing.
     *
     * @return
     *   `true` if service was put into a starting/started desired state, `false` otherwise.
     */
    suspend fun start(): Boolean

    /**
     * Send a service stop command to the [ServiceController] for processing.
     *
     * @return
     *   `true` if service was put into a stopping/stopped desired state, `false` otherwise.
     */
    suspend fun stop(): Boolean

    suspend fun awaitInitializeCompletion(): Boolean
}

/**
 * Singleton implementation of [ServiceController] that represents a specialized service controller
 * for the agent component itself.
 */
object AgentServiceController : DefaultServiceController(ConcordComponent.ServiceType.GENERIC) {

    /** Logging instance. */
    private val log by logger()

    override fun initializeAsync(): Deferred<Boolean> {
        val network = async {
            val name = DEFAULT_CONTAINER_NETWORK_NAME
            orchestrator.getNetworks(name).firstOrNull() ?: (orchestrator.createNetwork(name) ?: "")
        }

        log.info { "Container network($network) is setup"}

        // Request handler that stays active until the controller's coroutine scope is cancelled.
        launch {
            // Wait on container network initialization.
            network.await()

            // Create a new channel to receive incoming requests for service controller.
            requests = Channel()

            // Set late-init fields to mark the completion of initialization.
            containers = emptyMap()

            log.info { "Service(agent) request handler started" }

            // Initialization did not encounter any error.
            initialized.complete(true)
            for (message in requests) {
                try {
                    log.info { "Service(agent) received message($message)" }

                    val response = processMessage(message)
                    message.response.complete(response)
                } catch (error: Throwable) {
                    log.error { "Service(agent) message($message) with error(${error.message}" }

                    message.response.completeExceptionally(error)
                }
            }
        }.invokeOnCompletion { error ->
            log.info { "Service(agent) request handler terminated, error(${error?.message})" }

            // Note: If channel is already closed due to error, this call has no effect.
            requests.close(error)
        }

        return initialized
    }

    override suspend fun processMessage(message: Request): Response {
        return when (message) {
            is Request.Start -> Response.Received(message) // No-op.
            is Request.Stop -> Response.Received(message) // No-op.
            is Request.Status -> Response.Status(ServiceState.State.ACTIVE)
        }
    }
}

/**
 * Default implementation of [ServiceController].
 *
 * @property[service]
 *   type of Concord service this controller instance is managing.
 * @property[components]
 *   list of components belonging to the [ConcordComponent.ServiceType] managed by this controller.
 * @property[containerRegistry]
 *   container registry to retrieve container image artifacts.
 */
open class DefaultServiceController(
    private val service: ConcordComponent.ServiceType,
    private val components: List<ConcordComponent> = emptyList(),
    private val containerRegistry: Endpoint = DockerHttpClient.DEFAULT_CONTAINER_REGISTRY
) : CoroutineScope, ServiceController {

    /**
     * Requests that can be sent to a [ServiceController]'s message handler.
     */
    sealed class Request(open val response: CompletableDeferred<Response>) {
        data class Start(override val response: CompletableDeferred<Response>) : Request(response)
        data class Stop(override val response: CompletableDeferred<Response>) : Request(response)
        data class Status(override val response: CompletableDeferred<Response>) : Request(response)
    }

    /**
     * Responses sent from a [ServiceController]'s message handler.
     */
    sealed class Response {
        data class Received(val request: Request) : Response()
        data class Status(val state: ServiceState.State) : Response()
        data class Error(val value: Throwable) : Response()
    }

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = Dispatchers.Default + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    /** Channel to receive [Request] messages. */
    protected lateinit var requests: Channel<Request>

    /** Mappings of [ConcordComponent] managed this service controller to the container ID. */
    protected lateinit var containers: Map<ConcordComponent, String>

    /** Indicator that initialization has completed and all lateinit properties have been set. */
    protected val initialized: CompletableDeferred<Boolean> = CompletableDeferred()

    /** [DockerClient] instance to be used for all container orchestration actions. */
    protected val orchestrator: DockerClient = DockerClient(
            // Note: Current assumption is that all components can be fetched from the same
            // DockerClient instance (which is targeting only 1 container registry).
            docker = DockerHttpClient(DockerHttpClient.Context(registryEndpoint = containerRegistry))
    )

    /**
     * Initialize the controller instance by ensuring the existence of underlying containers based
     * on the input [ConcordComponent] list.
     */
    override fun initializeAsync(): Deferred<Boolean> {
        // For each component, ensure the existence of a container matching its specification.
        val containerTasks = components.map { component ->
            async {
                val registry = DockerHttpClient.resolveRegistryAddress(containerRegistry)
                val model = ContainerImage.newContainerImage("$registry/${component.name}")
                val settings = model.resolve()

                // Pick the first one returned from the sorted result.
                val network = orchestrator.getNetworks(DEFAULT_CONTAINER_NETWORK_NAME).firstOrNull()
                        ?: ""
                log.info { "Network($network) obtained"}

                // Create / pull the image from container registry, if needed.
                val created = orchestrator.createImage(model.repository, model.tag)
                log.info { "Service($service) container image($model) created, result($created)" }

                // Resolve the image ID and create the container.
                val container = orchestrator.getImageIdentifier(model.repository, model.tag)
                        ?.let { image ->
                            /* Check if there is an existing container already.*/
                            val containers = orchestrator.getContainers(image)

                            // Highlander: There can be only one!
                            check(containers.size <= 1) { "There can be only one!" }

                            // Take the container ID if exists, otherwise spawn one.
                            // Note: There is no system-level mutual exclusion here, so this action
                            // is clearly not process or thread-safe!
                            containers.firstOrNull()
                                    ?: orchestrator.createContainer(
                                            settings.containerName?: basename(model.repository),
                                            image = image,
                                            portBindings = settings.portBindings,
                                            volumeBindings = settings.volumeBindings,
                                            environment = settings.environment,
                                            network = network
                                    )

                            // Obtain the EARLIEST container again in case there was a race. Since
                            // sort is by creation time, the retrieval is idempotent across
                            // concurrently created containers.
                            orchestrator.getContainers(image).firstOrNull()
                        }
                        ?: throw IllegalStateException("Container cannot be created, image($model)")

                log.info { "Service($service) container($container) created, image($model)" }

                // Return the name to container ID as a mapping.
                component to container
            }
        }

        // Request handler that stays active until the controller's coroutine scope is cancelled.
        launch {
            // Create a new channel to receive incoming requests for service controller.
            requests = Channel()

            // Wait on all the container setup to be complete and then join/prepare the result.
            containers = containerTasks.awaitAll().toMap()

            log.info { "Service($service) request handler started" }

            // Initialization did not encounter any error.
            initialized.complete(true)
            for (message in requests) {
                try {
                    log.info { "Service($service) received message($message)" }

                    val response = processMessage(message)
                    message.response.complete(response)
                } catch (error: Throwable) {
                    log.error { "Service($service) message($message) with error(${error.message}" }

                    message.response.completeExceptionally(error)
                }
            }
        }.invokeOnCompletion { error ->
            log.info { "Service($service) request handler terminated, error(${error?.message})" }

            // Note: If channel is already closed due to error, this call has no effect.
            requests.close(error)
        }

        return initialized
    }

    /**
     * Send a service start command to the [ServiceController] for processing.
     *
     * @return
     *   `true` if service was put into a starting/started desired state, `false` otherwise.
     */
    override suspend fun start(): Boolean {
        if (!initialized.await()) {
            return false
        }

        val message = Request.Start(CompletableDeferred())
        requests.send(message)

        return when (message.response.await()) {
            is Response.Received -> true
            else -> false
        }
    }

    /**
     * Send a service stop command to the [ServiceController] for processing.
     *
     * @return
     *   `true` if service was put into a stopping/stopped desired state, `false` otherwise.
     */
    override suspend fun stop(): Boolean {
        if (!initialized.await()) {
            return false
        }

        val message = Request.Stop(CompletableDeferred())
        requests.send(message)

        return when (message.response.await()) {
            is Response.Received -> true
            else -> false
        }
    }

    override suspend fun awaitInitializeCompletion(): Boolean = initialized.await()

    /**
     * Handler for [Request] messages sent to the service controller instance.
     *
     * @param[message]
     *   incoming request messages.
     *
     * @return
     *   [Response] message as a result of processing the request message.
     */
    open suspend fun processMessage(message: Request): Response {
        return when (message) {
            is Request.Start -> {
                // Perform action against all components managed by this controller.
                for (container in containers.values) {
                    orchestrator.startContainer(container)
                }

                Response.Received(message)
            }
            is Request.Stop -> {
                // Perform action against all components managed by this controller.
                for (container in containers.values) {
                    orchestrator.stopContainer(container)
                }

                Response.Received(message)
            }
            is Request.Status -> {
                Response.Status(ServiceState.State.ACTIVE)
            }
        }
    }

    /**
     * Resolve the [ContainerSettings] to use for this [ContainerImage].
     *
     * @return
     *   a suitable / matched [ContainerSettings] instance, or [ContainerSettings.DEFAULT] if there
     *   is no match.
     */
    private fun ContainerImage.resolve(): ContainerSettings {
        return when (repository) {
            ContainerSettings.CONCORD_CORE_TLS.imageRepository -> ContainerSettings.CONCORD_CORE_TLS
            ContainerSettings.ETHEREUM_RPC.imageRepository -> ContainerSettings.ETHEREUM_RPC
            else -> ContainerSettings.DEFAULT
        }
    }

    /**
     * Obtain the basename portion (last segment) of a container image repository name.
     *
     * @param[repository]
     *   container image repository name.
     *
     * @return
     *   basename segment, as a [String].
     */
    private fun basename(repository: String) = repository.split('/').last()
}
