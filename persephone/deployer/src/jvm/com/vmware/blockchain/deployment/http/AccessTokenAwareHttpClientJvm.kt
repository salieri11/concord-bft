/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.http

import com.vmware.blockchain.deployment.logging.Logger
import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.serialization.JsonSerializer
import kotlinx.coroutines.future.await
import java.io.IOException
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.net.http.HttpClient
import java.net.http.HttpRequest as JdkHttpRequest
import java.net.http.HttpResponse as JdkHttpResponse
import java.nio.charset.StandardCharsets

/**
 * An abstract contract implementation of an HTTP client that is access-token aware.
 *
 * @param[serializer]
 *   HTTP response serializer.
 * @property[accessToken]
 *   last-known API access token value.
 * @property[client]
 *   underlying HTTP client to use for communication.
 */
actual abstract class AccessTokenAwareHttpClient(
    private val serviceEndpoint: URI,
    private val serializer: JsonSerializer
) {
    /**
     * Adapter implementation converting [JdkHttpResponse] to [HttpResponse].
     *
     * @param[response]
     *   JDK HTTP response instance.
     */
    private class ResponseAdapter<T>(private val response: JdkHttpResponse<T>) : HttpResponse<T> {
        override fun statusCode(): Int = response.statusCode()
        override fun body(): T = response.body()
    }

    /** Logging instance. */
    private val log: Logger by logger()

    /** Underlying HTTP client instance. */
    private val client: HttpClient by newClient()

    /** Last-known API access token value. */
    @Volatile private var accessToken: String? = null

    /**
     * Retrieve the API session token from a given session response.
     *
     * @return
     *   API session token as a [String].
     */
    protected actual abstract fun retrieveAccessToken(sessionResponse: HttpResponse<String>): String

    /**
     * Retrieve the HTTP header name corresponding to the access token.
     *
     * @return
     *   the HTTP header name for access token.
     */
    protected actual abstract fun accessTokenHeader(): String

    /**
     * Obtain the session creation URI associated with this client instance.
     *
     * @return
     *   the session-creation [URI] value.
     */
    protected actual abstract fun session(): URI

    /**
     * Obtain the authentication credential associated with this client instance.
     *
     * @return
     *   the [Credential] instance.
     */
    protected actual abstract fun credential(): Credential

    /**
     * Create a new [HttpClient] for this API client instance.
     *
     * @return
     *   a new [HttpClient] instance.
     */
    private fun newClient(): Lazy<HttpClient> = lazy {
        val credential = credential()
        when (credential.type) {
            Credential.Type.PASSWORD -> {
                val authenticator = object : Authenticator() {
                    val passwordCredential = requireNotNull(credential.passwordCredential)
                    override fun getPasswordAuthentication(): PasswordAuthentication =
                            PasswordAuthentication(
                                    passwordCredential.username,
                                    passwordCredential.password.toCharArray()
                            )
                }
                HttpClient.newBuilder().authenticator(authenticator).build()
            }
            else -> HttpClient.newHttpClient()
        }
    }

    /**
     * POST to the session endpoint associated with this client instance in order to establish an
     * open session.
     *
     * @return
     *   the [HttpResponse] from a successful POST.
     */
    private suspend fun connect(): JdkHttpResponse<String> {
        val request = JdkHttpRequest.newBuilder()
                .uri(session())
                .POST(JdkHttpRequest.BodyPublishers.noBody())
                .build()
                .also { log.info { "API: $it" } }

        return client
                .sendAsync(request, JdkHttpResponse.BodyHandlers.ofString())
                .await()
    }

    /**
     * Convert a [JdkHttpResponse] instance to a [HttpResponse] instance.
     *
     * @return
     *   an adapted [HttpResponse] instance.
     */
    private fun <T> JdkHttpResponse<T>.toHttpResponse(): HttpResponse<T> {
        return ResponseAdapter(this)
    }

    /**
     * Obtain an API session token either from cache or through [connect].
     *
     * @return
     *   API session token as a [String].
     */
    internal suspend fun token(): String {
        return accessToken
                ?.let { it } ?: retrieveAccessToken(connect().toHttpResponse())
                .also { accessToken = it }
    }

    /**
     * Send a HTTP GET with content specified by parameter and return the response with response
     * body mapped to a typed instance if request was successful.
     *
     * @param[path]
     *   path to be resolved against the base [serviceEndpoint].
     * @param[contentType]
     *   HTTP content type.
     * @param[headers]
     *   list of HTTP headers to be set for the request.
     *
     * @return
     *   the response of the request as a parameterized (data-bound) [HttpResponse] instance.
     */
    internal actual suspend inline fun <reified T> get(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>
    ): HttpResponse<T?> {
        val httpRequest = JdkHttpRequest.newBuilder()
                .GET()
                .uri(serviceEndpoint.resolve(path))
                .header("Content-Type", contentType)
                .header(accessTokenHeader(), token())
                .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                .build()
        return request(httpRequest)
    }

    /**
     * Send a HTTP PATCH with content specified by parameter and return the response with response
     * body mapped to a typed instance if request was successful.
     *
     * @param[path]
     *   path to be resolved against the base [serviceEndpoint].
     * @param[contentType]
     *   HTTP content type.
     * @param[headers]
     *   list of HTTP headers to be set for the request.
     * @param[body]
     *   request body.
     *
     * @return
     *   the response of the request as a parameterized (data-bound) [HttpResponse] instance.
     */
    internal actual suspend inline fun <reified R, reified T> patch(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>,
        body: R?
    ): HttpResponse<T?> {
        val bodyPublisher = body
                // Note: Serializer may throw exception, which is not going to be caught here.
                ?.let { serializer.toJson(body) }
                ?.let { JdkHttpRequest.BodyPublishers.ofString(it) }
                ?: JdkHttpRequest.BodyPublishers.noBody()
        val httpRequest = JdkHttpRequest.newBuilder()
                .method("PATCH", bodyPublisher)
                .uri(serviceEndpoint.resolve(path))
                .header("Content-Type", contentType)
                .header(accessTokenHeader(), token())
                .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                .build()
        return request(httpRequest)
    }

    /**
     * Send a HTTP POST with content specified by parameter and return the response with response
     * body mapped to a typed instance if request was successful.
     *
     * @param[path]
     *   path to be resolved against the base [serviceEndpoint].
     * @param[contentType]
     *   HTTP content type.
     * @param[headers]
     *   list of HTTP headers to be set for the request.
     * @param[body]
     *   request body.
     *
     * @return
     *   the response of the request as a parameterized (data-bound) [HttpResponse] instance.
     */
    internal actual suspend inline fun <reified R, reified T> post(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>,
        body: R?
    ): HttpResponse<T?> {
        val bodyPublisher = body
                // Note: Serializer may throw exception, which is not going to be caught here.
                ?.let { serializer.toJson(body) }
                ?.let { JdkHttpRequest.BodyPublishers.ofString(it) }
                ?: JdkHttpRequest.BodyPublishers.noBody()
        val httpRequest = JdkHttpRequest.newBuilder()
                .POST(bodyPublisher)
                .uri(serviceEndpoint.resolve(path))
                .header("Content-Type", contentType)
                .header(accessTokenHeader(), token())
                .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                .build()
        return request(httpRequest)
    }

    /**
     * Send a HTTP DELETE with content specified by parameter and return the response with response
     * body mapped to a typed instance if request was successful.
     *
     * @param[path]
     *   path to be resolved against the base [serviceEndpoint].
     * @param[contentType]
     *   HTTP content type.
     * @param[headers]
     *   list of HTTP headers to be set for the request.
     * @return
     *   the response of the request as a parameterized (data-bound) [HttpResponse] instance.
     */
    internal actual suspend inline fun <reified T> delete(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>
    ): HttpResponse<T?> {
        val httpRequest = JdkHttpRequest.newBuilder()
                .DELETE()
                .uri(serviceEndpoint.resolve(path))
                .header("Content-Type", contentType)
                .header(accessTokenHeader(), token())
                .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                .build()
        return request(httpRequest)
    }

    /**
     * Send a HTTP request with content specified by parameter and return the response with response
     * body mapped to a typed instance if request was successful.
     *
     * @param[httpRequest]
     *   HTTP request instance.
     *
     * @return
     *   the response of the request as a parameterized (data-bound) [HttpResponse] instance.
     */
    private suspend inline fun <reified T> request(httpRequest: JdkHttpRequest): HttpResponse<T?> {
        val upstream = JdkHttpResponse.BodySubscribers.ofString(StandardCharsets.UTF_8)
        val bindToResult = JdkHttpResponse.BodySubscribers.mapping(upstream) { value ->
            try {
                log.info { "API: response body($value)" }
                value?.takeIf { it.isNotEmpty() }
                        ?.let { serializer.fromJson<T>(it) }
            } catch (error: Exception) {
                log.error { "Exception while parsing API response, error(${error.message})" }

                // Since response cannot be deserialized, return no value.
                null
            }
        }
        val logging = JdkHttpResponse.BodySubscribers.mapping<String, T?>(upstream) { value ->
            log.info { "API: raw response body($value)"}
            null
        }

        val handler: JdkHttpResponse.BodyHandler<T?> = JdkHttpResponse.BodyHandler { response ->
            when (response.statusCode()) {
                200 -> bindToResult
                201 -> bindToResult
                202 -> bindToResult
                203 -> bindToResult
                else -> logging // no logging => JdkHttpResponse.BodySubscribers.replacing(null)
            }
        }

        return try {
            httpRequest
                    .apply { log.info { "API: $this" } }
                    .let { client.sendAsync(it, handler).await() }
                    .also { response ->
                        log.info { "API: status(${response.statusCode()} ${response.request()}" }
                    }
                    .let { response -> response.toHttpResponse() }
        } catch (error: IOException) {
            // Straight-up body-handling without examining header.
            log.error { "Exception encountered from API request, error(${error.message})" }

            // Try again re-requesting with a new token.
            //
            // Note: The body handler logic is setup to capture failures WITHIN the body handling.
            // Unfortunately vSphere's CIS authentication session is not according to spec since in
            // does not send WWW-Authenticate in the header response of status 401. So HttpClient
            // will blow up WHILE processing the response header.
            //
            // try-catch with IOException is somewhat hacky. When vSphere's CIS auth session is
            // fixed, this workaround can be removed.
            httpRequest
                    .apply { log.info { "API: $this" } }
                    .let { client.sendAsync(it, handler).await() }
                    .also { response ->
                        log.info { "API response(${response.statusCode()}): ${response.request()}" }
                    }
                    .let { response -> response.toHttpResponse() }
        }
    }
}
