/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker

import com.vmware.blockchain.deployment.agent.docker.api.v1_38.DockerModelSerializer
import com.vmware.blockchain.deployment.agent.docker.api.v1_38.model.AuthConfig
import com.vmware.blockchain.deployment.http.AccessTokenAwareHttpClient
import com.vmware.blockchain.deployment.http.HttpResponse
import com.vmware.blockchain.deployment.model.Credential
import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.core.Credential as CoreCredential
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.encodeBase64

/**
 * An HTTP REST client for issuing API to a Docker Engine endpoint.
 *
 * @property[client]
 *   underlying HTTP client to use for communication.
 */
class DockerClient(
    private val context: Context
) : AccessTokenAwareHttpClient(URI.create(context.endpoint.address), DockerModelSerializer) {

    companion object {
        /** Default container registry endpoint. */
        @JvmStatic
        val DEFAULT_CONTAINER_REGISTRY: Endpoint = Endpoint("https://registry-1.docker.io/v2")

        /** Default Docker Engine API endpoint. */
        @JvmStatic
        val DEFAULT_DOCKER_ENGINE: Endpoint = Endpoint("http://localhost:2375")
    }

    /**
     * Context parameters for [DockerClient].
     *
     * @param[endpoint]
     *   endpoint for the targeted Docker Engine API endpoint.
     * @param[registryEndpoint]
     *   endpoint for the container registry to instruct the Docker Engine to pull images from.
     */
    data class Context(
        val endpoint: Endpoint,
        val registryEndpoint: Endpoint = DEFAULT_CONTAINER_REGISTRY
    )

    /** HTTP Authentication header value for authenticating with the container registry. */
    val registryAuthenticationHeader: String by lazy {
        val config = AuthConfig(
                username = context.registryEndpoint.credential.passwordCredential.username,
                password = context.registryEndpoint.credential.passwordCredential.password,
                serveraddress = registryAddress
        )

        // Base64-encoded JSON.
        val data = ByteString.of(*DockerModelSerializer.toJson(config).toByteArray()).encodeBase64()

        // Represent the data in UTF-8 encoded String.
        String(data.toByteArray(), Charsets.UTF_8)
    }

    /** DNS-name of the container registry that images should be created from. */
    val registryAddress: String by lazy {
        val url = URI.create(context.registryEndpoint.address)

        url.host + (url.port.takeIf { it != -1 }?.let { ":$it" } ?: "")
    }

    /** Specify whether HTTP requests should specify access token in HTTP header. */
    override val useAccessToken: Boolean = false

    /**
     * Obtain the API session token from a given session response.
     *
     * @return
     *   API session token as a [String].
     */
    override fun retrieveAccessToken(sessionResponse: HttpResponse<String>): String = ""

    /**
     * Retrieve the HTTP header name corresponding to the access token.
     *
     * @return
     *   the HTTP header name for access token.
     */
    override fun accessTokenHeader(): String = ""

    /**
     * Retrieve the session creation URI associated with this client instance.
     *
     * @return
     *   the session-creation [URI] value.
     */
    override fun session(): URI = URI.create(context.endpoint.address)

    /**
     * Obtain the authentication credential associated with this client instance.
     *
     * @return
     *   the [Credential] instance.
     */
    override fun credential(): CoreCredential = CoreCredential()
}
