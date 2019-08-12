/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.http.AccessTokenAwareHttpClient
import com.vmware.blockchain.deployment.http.HttpResponse
import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.core.BearerTokenCredential
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.vmc.VmcAuthenticationResponse

/**
 * An HTTP REST client for issuing API to a VMware Cloud endpoint.
 *
 * @param[serializer]
 *   HTTP response serializer.
 * @property[client]
 *   underlying HTTP client to use for communication.
 */
class VmcHttpClient(
    val context: Context,
    private val serializer: JsonSerializer
): AccessTokenAwareHttpClient(context.endpoint, serializer) {

    /**
     * Context parameters for [VmcHttpClient].
     *
     * @param[endpoint]
     *   URI for the targeted VMware Cloud API endpoint.
     * @param[authenticationEndpoint]
     *   URI for the targeted VMware Cloud API endpoint's authentication endpoint.
     * @param[refreshToken]
     *   API refresh token to use to obtain access token.
     * @param[organization]
     *   organization identifier.
     * @param[datacenter]
     *   data center identifier.
     */
    data class Context(
        val endpoint: URI,
        val authenticationEndpoint: URI,
        val refreshToken: String,
        val organization: String,
        val datacenter: String,
        val enableVerboseLogging: Boolean = true
    )

    /**
     * Obtain the API session token from a given session response.
     *
     * @return
     *   API session token as a [String].
     */
    override fun retrieveAccessToken(sessionResponse: HttpResponse<String>): String {
        return serializer
                .fromJson<VmcAuthenticationResponse>(sessionResponse.body())
                .access_token
    }

    /**
     * Retrieve the HTTP header name corresponding to the access token.
     *
     * @return
     *   the HTTP header name for access token.
     */
    override fun accessTokenHeader(): String = "csp-auth-token"

    /**
     * Retrieve the session creation URI associated with this client instance.
     *
     * @return
     *   the session-creation [URI] value.
     */
    override fun session(): URI {
        return Endpoints.VMC_AUTHENTICATION
                .resolve(context.authenticationEndpoint,
                         listOf(Pair("refresh_token", context.refreshToken)))
    }

    /**
     * Obtain the authentication credential associated with this client instance.
     *
     * @return
     *   the [Credential] instance.
     */
    override fun credential(): Credential {
        return Credential(
                Credential.Type.BEARER,
                tokenCredential = BearerTokenCredential(context.refreshToken)
        )
    }
}