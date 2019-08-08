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
import kotlinx.coroutines.future.await
import java.io.IOException
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.net.http.HttpClient
import java.net.http.HttpRequest as JdkHttpRequest
import java.net.http.HttpResponse as JdkHttpResponse
import java.nio.charset.StandardCharsets
import java.security.SecureRandom
import javax.net.ssl.SSLContext

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
    @PublishedApi internal val serviceEndpoint: URI,
    @PublishedApi internal val serializer: JsonSerializer
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
    @PublishedApi
    internal val log: Logger by logger()

    /** Underlying HTTP client instance. */
    @PublishedApi
    internal val client: HttpClient by newClient()

    /** Last-known API access token value. */
    @Volatile private var accessToken: String? = null

    /** Specify whether HTTP request should specify access token in HTTP header. */
    actual open val useAccessToken: Boolean = true

    /** Specify whether insecure HTTP connections are allowed. */
    actual open val allowInsecureConnection: Boolean = false

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

                // Create the SSL context based on configuration settings.
                val sslContext = if (allowInsecureConnection) {
                    SSLContext.getInstance("TLS").apply {
                        init(null, arrayOf(InsecureX509TrustManager), SecureRandom())
                    }
                } else {
                    SSLContext.getDefault()
                }

                HttpClient.newBuilder()
                        .sslContext(sslContext)
                        .authenticator(authenticator)
                        .build()
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
    @PublishedApi internal fun <T> JdkHttpResponse<T>.toHttpResponse(): HttpResponse<T> {
        return ResponseAdapter(this)
    }

    /**
     * Obtain an API session token either from cache or through [connect].
     *
     * @param[useCache]
     *   use the currently-cached token if available.
     *
     * @return
     *   API session token as a [String].
     */
    @PublishedApi internal suspend fun token(useCache: Boolean = true): String {
        return accessToken?.takeIf { useCache }
                ?: (if (useAccessToken) retrieveAccessToken(connect().toHttpResponse()) else "")
                        .also { accessToken = it }
    }

    /**
     * Internal access wrapper to [accessTokenHeader].
     *
     * Note: Access wrapper is required to access protected inline API methods. Wrapper is declared
     * as internal to prevent unexpected callers external to this module.
     *
     * @return
     *   the HTTP header name for access token.
     */
    @PublishedApi
    internal fun internalAccessTokenHeader() = accessTokenHeader()

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
    actual suspend inline fun <reified T : Any> get(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>
    ): HttpResponse<T?> {
        @Suppress("DuplicatedCode")
        val response = requestWith { token ->
            JdkHttpRequest.newBuilder()
                    .GET()
                    .uri(serviceEndpoint.resolve(path))
                    .header(HTTP_HEADER_CONTENT_TYPE, contentType)
                    .apply {
                        takeIf { useAccessToken }?.header(internalAccessTokenHeader(), token)
                    }
                    .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                    .build()
        }

        return object : HttpResponse<T?> {
            override fun statusCode(): Int = response.statusCode()
            override fun body(): T? = response.body().toTypedInstance()
        }
    }

    /**
     * Send a HTTP GET with content specified by parameter and return the response with response
     * body mapped to a typed instance if request was successful.
     *
     * @param[path]
     *   path to be resolved against the base service endpoint.
     * @param[contentType]
     *   HTTP content type.
     * @param[headers]
     *   list of HTTP headers to be set for the request.
     * @param[arrayResponse]
     *   specify whether to expect the HTTP response body as an array of elements.
     *
     * @return
     *   the response of the request as a parameterized (data-bound) [HttpResponse] instance.
     */
    actual suspend inline fun <reified T : Any> get(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>,
        arrayResponse: Boolean
    ): HttpResponse<List<T>?> {
        @Suppress("DuplicatedCode")
        val response = requestWith { token ->
            JdkHttpRequest.newBuilder()
                    .GET()
                    .uri(serviceEndpoint.resolve(path))
                    .header(HTTP_HEADER_CONTENT_TYPE, contentType)
                    .apply {
                        takeIf { useAccessToken }?.header(internalAccessTokenHeader(), token)
                    }
                    .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                    .build()
        }

        return object : HttpResponse<List<T>?> {
            override fun statusCode(): Int = response.statusCode()
            override fun body(): List<T>? {
                return if (arrayResponse) {
                    response.body().toTypedInstanceList()
                } else {
                    response.body().toTypedInstance<T>()?.let { listOf(it) }
                }
            }
        }
    }

    /**
     * Send a HTTP PUT with content specified by parameter and return the response with response
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
    actual suspend inline fun <reified R : Any, reified T : Any> put(
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

        val response = requestWith { token ->
            JdkHttpRequest.newBuilder()
                    .PUT(bodyPublisher)
                    .uri(serviceEndpoint.resolve(path))
                    .header(HTTP_HEADER_CONTENT_TYPE, contentType)
                    .apply {
                        takeIf { useAccessToken }?.header(internalAccessTokenHeader(), token)
                    }
                    .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                    .build()
        }

        return object : HttpResponse<T?> {
            override fun statusCode(): Int = response.statusCode()
            override fun body(): T? = response.body().toTypedInstance()
        }
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
    actual suspend inline fun <reified R : Any, reified T : Any> patch(
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

        val response = requestWith { token ->
            JdkHttpRequest.newBuilder()
                    .method("PATCH", bodyPublisher)
                    .uri(serviceEndpoint.resolve(path))
                    .header(HTTP_HEADER_CONTENT_TYPE, contentType)
                    .apply {
                        takeIf { useAccessToken }?.header(internalAccessTokenHeader(), token)
                    }
                    .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                    .build()
        }

        return object : HttpResponse<T?> {
            override fun statusCode(): Int = response.statusCode()
            override fun body(): T? = response.body().toTypedInstance()
        }
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
    actual suspend inline fun <reified R : Any, reified T : Any> post(
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

        val response = requestWith { token ->
            JdkHttpRequest.newBuilder()
                    .POST(bodyPublisher)
                    .uri(serviceEndpoint.resolve(path))
                    .header(HTTP_HEADER_CONTENT_TYPE, contentType)
                    .apply {
                        takeIf { useAccessToken }?.header(internalAccessTokenHeader(), token)
                    }
                    .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                    .build()
        }

        return object : HttpResponse<T?> {
            override fun statusCode(): Int = response.statusCode()
            override fun body(): T? = response.body().toTypedInstance()
        }
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
    actual suspend inline fun <reified T : Any> delete(
        path: String,
        contentType: String,
        headers: List<Pair<String, String>>
    ): HttpResponse<T?> {
        val response = requestWith { token ->
            JdkHttpRequest.newBuilder()
                    .DELETE()
                    .uri(serviceEndpoint.resolve(path))
                    .header(HTTP_HEADER_CONTENT_TYPE, contentType)
                    .apply {
                        takeIf { useAccessToken }?.header(internalAccessTokenHeader(), token)
                    }
                    .also { builder -> headers.forEach { builder.header(it.first, it.second) } }
                    .build()
        }

        return object : HttpResponse<T?> {
            override fun statusCode(): Int = response.statusCode()
            override fun body(): T? = response.body().toTypedInstance()
        }
    }

    /**
     * Convenient extension function on [String] to deserialize to an instance of type [T].
     *
     * @param[T]
     *   type of instance to deserialize to.
     *
     * @return
     *   an instance of [T] if string is not empty and successfully deserialized, `null` otherwise.
     */
    @PublishedApi
    internal inline fun <reified T : Any> String.toTypedInstance(): T? {
        return try {
            takeIf { it.isNotEmpty() }?.let { serializer.fromJson<T>(it) }
        } catch (error: Exception) {
            log.error { "Exception while parsing string, error(${error.message})" }

            // Since string cannot be deserialized, return no value.
            null
        }
    }

    /**
     * Convenient extension function on [String] to deserialize to an instance of type [T].
     *
     * @param[T]
     *   type of instance to deserialize to.
     *
     * @return
     *   a list of instances of type [T] if string is not empty and successfully deserialized,
     *   `null` otherwise.
     */
    @PublishedApi
    internal inline fun <reified T : Any> String.toTypedInstanceList(): List<T>? {
        return try {
            takeIf { it.isNotEmpty() }?.let { serializer.fromJsonArray(it) }
        } catch (error: Exception) {
            log.error { "Exception while parsing string, error(${error.message})" }

            // Since string cannot be deserialized, return no value.
            null
        }
    }

    /**
     * Pipeline the HTTP request with a request factory-supplier lambda such that when called with
     * an access token, creates an HTTP request that is then used for outbound HTTP call.
     *
     * Note: The function takes care of retrying the request in the event of status 401 response
     * upon first attempt.
     *
     * @param[supplier]
     *   supplier function to create the HTTP request payload when given an access token.
     *
     * @return
     *   the [HttpResponse] as a result of the HTTP request.
     */
    @PublishedApi
    internal suspend fun requestWith(supplier: (String) -> JdkHttpRequest): HttpResponse<String> {
        val message = supplier(token())
        val response = request(message)
        return if (response.statusCode() == 401) {
            // Request failed due to authorization. Force-refresh access token and try again.
            val retryMessage = supplier(token(useCache = false))
            request(retryMessage)
        } else {
            response
        }
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
    private suspend fun request(httpRequest: JdkHttpRequest): HttpResponse<String> {
        val upstream = JdkHttpResponse.BodySubscribers.ofString(StandardCharsets.UTF_8)
        val success = JdkHttpResponse.BodySubscribers.mapping<String, String>(upstream) { value ->
            log.info { "API: response body($value)" }
            value
        }
        val error = JdkHttpResponse.BodySubscribers.mapping<String, String>(upstream) { value ->
            log.info { "API: response body($value)" }
            ""
        }
        val handler: JdkHttpResponse.BodyHandler<String> = JdkHttpResponse.BodyHandler { response ->
            when (response.statusCode()) {
                200, 201, 202, 203 -> success
                else -> error // no logging => JdkHttpResponse.BodySubscribers.replacing("")
            }
        }

        return try {
            httpRequest
                    .apply { log.info { "API: $this" } }
                    .let { client.sendAsync(it, handler).await() }
                    .also { response ->
                        log.info { "API: status(${response.statusCode()}) ${response.request()}" }
                    }
                    .let { response -> response.toHttpResponse() }
        } catch (error: IOException) {
            // Straight-up body-handling without examining header.
            log.error { "Exception encountered from API request, error(${error.message})" }

            // Interpret as status 401.
            //
            // Note: The body handler logic is setup to capture failures WITHIN the body handling.
            // Unfortunately vSphere's CIS authentication session is not according to spec since in
            // does not send WWW-Authenticate in the header response of status 401. So HttpClient
            // will blow up WHILE processing the response header.
            //
            // try-catch with IOException is somewhat hacky. When vSphere's CIS auth session is
            // fixed, this workaround can be removed.
            //
            // Reference: https://jira.eng.vmware.com/browse/VAPI-1195
            object : HttpResponse<String> {
                override fun statusCode(): Int = 401
                override fun body(): String = ""
            }
        }
    }
}
