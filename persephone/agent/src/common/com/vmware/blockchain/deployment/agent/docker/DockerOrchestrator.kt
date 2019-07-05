/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker

import com.vmware.blockchain.deployment.agent.docker.api.v1_38.Endpoints
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerCreateResponse
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.ContainerSummary
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.HostConfig
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.Image
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.PortBinding

/**
 * An orchestrator acting as a client of a Docker Engine API endpoint to conduct orchestration
 * actions on the host of the Docker Engine API endpoint.
 *
 * @property[docker]
 *   an instance of [DockerClient] set up to call APIs of a specific Docker Engine API endpoint.
 */
class DockerOrchestrator(private val docker: DockerClient) {

    /**
     * Specification of a container port binding on the host.
     *
     * @property[ip]
     *   IP published on the host.
     * @property[port]
     *   port published on the host.
     */
    data class HostPortBinding(val ip: String = "", val port: Int = 0)

    /**
     * Specification of a container port binding on the container.
     *
     * @property[port]
     *   port exposed in the container.
     * @property[protocol]
     *   protocol for the port binding.
     */
    data class ContainerPortBinding(val port: Int, val protocol: Protocol = Protocol.TCP) {
        enum class Protocol {
            UDP,
            TCP;

            override fun toString(): String {
                return when (this) {
                    UDP -> "udp"
                    TCP -> "tcp"
                }
            }
        }
    }

    /**
     * Create an image locally by pulling an upstream repository from the container registry.
     *
     * @param[repository]
     *   image repository to pull from.
     * @param[tag]
     *   tag of the image to pull.
     *
     * @return
     *   `true` if image was successfully created locally, `false` otherwise.
     */
    suspend fun createImage(repository: String, tag: String): Boolean {
        val response = docker
                .post<Unit, String>(
                        path = Endpoints.IMAGES_CREATE.interpolate(
                                parameters = listOf("fromImage" to "$repository:$tag")
                        ),
                        contentType = "application/json",
                        headers = listOf("X-Registry-Auth" to docker.registryAuthenticationHeader),
                        body = null
                )

        return when (response.statusCode()) {
            200 -> true
            404, 500 -> false // Possible errors. (Not handling for now)
            else -> false
        }
    }

    /**
     * Create a container according to the given parameters.
     *
     * @param[portBindings]
     *   a mapping of host to container port bindings.
     * @param[volumeBindings]
     *   a mapping of host path or container volume name to container path.
     * @param[environment]
     *   a mapping of environment variables set in the container, `null` value removes the key
     *   from the container.
     * @param[network]
     *   network to attach the container to, standard values are "none", "bridge"
     *   (Docker default bridge network), "host" (host network), or "<container-name/ID>", any other
     *   values are treated as Docker network ID/names.
     */
    suspend fun createContainer(
        name: String,
        image: String,
        portBindings: Map<ContainerPortBinding, List<HostPortBinding>?> = emptyMap(),
        volumeBindings: Map<String, String> = emptyMap(),
        environment: Map<String, String?> = emptyMap(),
        network: String = ""
    ): String? {
        val response = docker.post<ContainerConfig, ContainerCreateResponse>(
                path = Endpoints.CONTAINERS_CREATE.interpolate(parameters = listOf("name" to name)),
                contentType = "application/json",
                headers = emptyList(),
                body = ContainerConfig(
                        Image = image,
                        Env = environment.map { entry ->
                            entry.value?.let { "${entry.key}=$it" } ?: entry.key
                        },
                        HostConfig = HostConfig(
                                Binds = volumeBindings.map { "${it.key}:${it.value}" },
                                PortBindings = portBindings.asIterable().associate {
                                    val key = "${it.key.port}/${it.key.protocol}"
                                    val value = it.value?.map { binding ->
                                        PortBinding(binding.ip, binding.port.toString())
                                    }

                                    // Container port (String) to list of Host ports (List<Port>).
                                    key to value
                                },
                                NetworkMode = network
                        )
                )
        )

        return when (response.statusCode()) {
            201 -> response.body()?.Id
            400, 404, 409, 500 -> null // Possible errors. (Not handling for now)
            else -> null
        }
    }

    /**
     * Look up the identifier of a container image specified by the given name.
     *
     * @param[name]
     *   name of the image to look up.
     *
     * @return
     *   identifier of the container image if found, `null` otherwise.
     */
    suspend fun getImageIdentifier(name: String): String? {
        val response = docker.get<Image>(
                            path = Endpoints.IMAGES_INSPECT
                                    .interpolate(pathVariables = listOf("{name}" to name)),
                            contentType = "application/json",
                            headers = emptyList()
        )

        return when (response.statusCode()) {
            200 -> {
                response.body()?.Id
            }
            400, 500 -> null // Possible errors. (Not handling for now)
            else -> null
        }
    }

    /**
     * Find all containers that is created from a given image specified by the given identifier.
     *
     * @param[imageIdentifier]
     *   identifier of the container image.
     *
     * @return
     *   a [List] of identifiers of containers created using the specified image identifier.
     */
    suspend fun getContainers(imageIdentifier: String): List<String> {
        val response = docker.get<ContainerSummary>(
                path = Endpoints.CONTAINERS_LIST.interpolate(parameters = listOf("all" to "true")),
                contentType = "application/json",
                headers = emptyList(),
                arrayResponse = true
        )

        return when (response.statusCode()) {
            200 -> {
                response.body().orEmpty().asSequence()
                        .filter { container -> container.ImageID == imageIdentifier }
                        .map { it.Id }
                        .toList()
            }
            400, 500 -> emptyList() // Possible errors. (Not handling for now)
            else -> emptyList()
        }
    }

    /**
     * Start a container specified by the given container identifier.
     *
     * @param[container]
     *   identifier of the container to start.
     *
     * @return
     *   `true` if container is started or already in started state, `false` otherwise.
     */
    suspend fun startContainer(container: String): Boolean {
        val response = docker.post<Unit, String>(
                path = Endpoints.CONTAINERS_START.interpolate(
                        pathVariables = listOf("{id}" to container)
                ),
                contentType = "application/json",
                headers = emptyList(),
                body = null
        )

        return when (response.statusCode()) {
            204, 304 -> true // Indicates that container is now/already in desired state.
            404, 500 -> false // Possible errors. (Not handling for now)
            else -> false
        }
    }

    /**
     * Stop a container specified by the given container identifier.
     *
     * @param[container]
     *   identifier of the container to stop.
     *
     * @return
     *   `true` if container is stopped or already in stopped state, `false` otherwise.
     */
    suspend fun stopContainer(container: String, waitBeforeStop: Int = 0): Boolean {
        val response = docker.post<Unit, String>(
                path = Endpoints.CONTAINERS_STOP.interpolate(
                        parameters = listOf("t" to waitBeforeStop.toString()),
                        pathVariables = listOf("{id}" to container)
                ),
                contentType = "application/json",
                headers = emptyList(),
                body = null
        )

        return when (response.statusCode()) {
            204, 304 -> true // Indicates that container is now/already in desired state.
            404, 500 -> false // Possible errors. (Not handling for now)
            else -> false
        }
    }

    /**
     * Delete a container specified by the given container identifier.
     *
     * @param[container]
     *   identifier of the container to delete.
     *
     * @return
     *   `true` if container was deleted or did not exist, `false` otherwise.
     */
    suspend fun deleteContainer(
        container: String,
        removeVolume: Boolean = false,
        force: Boolean = false
    ): Boolean {
        val response = docker.delete<Unit>(
                path = Endpoints.CONTAINERS_DELETE.interpolate(
                        parameters = listOf(
                                "v" to removeVolume.toString(),
                                "force" to force.toString()
                        ),
                        pathVariables = listOf("{id}" to container)
                ),
                contentType = "application/json",
                headers = emptyList()
        )

        return when (response.statusCode()) {
            204, 404 -> true
            400, 409, 500 -> false // Possible errors. (Not handling for now)
            else -> false
        }
    }
}
