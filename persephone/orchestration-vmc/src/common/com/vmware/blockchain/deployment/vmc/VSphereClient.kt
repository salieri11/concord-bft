/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.http.AccessTokenAwareHttpClient
import com.vmware.blockchain.deployment.http.HttpResponse
import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.PasswordCredential
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.vsphere.VSphereAuthenticationResponse

/**
 * An HTTP REST client for issuing API to a vSphere endpoint.
 *
 * @param[context]
 *   context for the targeted vSphere deployment.
 * @param[serializer]
 *   HTTP response serializer.
 * @property[client]
 *   underlying HTTP client to use for communication.
 */
class VSphereClient(
    val context: Context,
    private val serializer: JsonSerializer
): AccessTokenAwareHttpClient(context.endpoint, serializer) {

    /**
     * Context parameters for [VSphereClient].
     *
     * @param[endpoint]
     *   context for the targeted vSphere endpoint.
     * @param[username]
     *   username to use to authenticate for bearer token.
     * @param[password]
     *   password to use to authenticate for bearer token.
     */
    data class Context(val endpoint: URI, val username: String, val password: String)

    /**
     * Obtain the API session token from a given session response.
     *
     * @return
     *   API session token as a [String].
     */
    override fun retrieveAccessToken(sessionResponse: HttpResponse<String>): String {
        return serializer
                .fromJson<VSphereAuthenticationResponse>(sessionResponse.body())
                .value
    }

    /**
     * Retrieve the HTTP header name corresponding to the access token.
     *
     * @return
     *   the HTTP header name for access token.
     */
    override fun accessTokenHeader(): String = "vmware-api-session-id"

    /**
     * Retrieve the session creation URI associated with this client instance.
     *
     * @return
     *   the session-creation [URI] value.
     */
    override fun session(): URI = Endpoints.VSPHERE_AUTHENTICATION.resolve(context.endpoint)

    /**
     * Obtain the authentication credential associated with this client instance.
     *
     * @return
     *   the [Credential] instance.
     */
    override fun credential(): Credential {
        return Credential(
                Credential.Type.PASSWORD,
                passwordCredential = PasswordCredential(context.username, context.password)
        )
    }
}
