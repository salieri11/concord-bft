/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.http

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.matching.EqualToPattern
import com.github.tomakehurst.wiremock.stubbing.Scenario
import com.vmware.blockchain.deployment.model.core.Credential
import com.vmware.blockchain.deployment.model.core.URI
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.modules.EmptyModule
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Basic semantics testing of [AccessTokenAwareHttpClient] operations.
 */
class AccessTokenAwareHttpClientTest {

    /**
     * A basic mock HTTP test server that provides bearer-token access semantics.
     *
     * @param[port]
     *   port number to use to start the server on localhost.
     * @param[scenario]
     *   name of the scenario to use as an isolated namespace (useful for logging).
     */
    class TestServer(port: Int, scenario: String = "DefaultScenario") {

        companion object {
            // Mock server's properties constants.
            const val MOCK_SERVER_TOKEN_HEADER = "Authentication"

            // Mock server's path-related constants.
            const val MOCK_SERVER_EXPIRE_SESSION_PATH = "/api/session/expire"
            const val MOCK_SERVER_RESOURCE_PATH = "/api/resource"
            const val MOCK_SERVER_SESSION_PATH = "/api/session"

            // Scenario state related constants.
            const val SCENARIO_EXPIRE_ACCESS_TOKEN = "access-token"
            const val SCENARIO_STATE_ACCESS_GRANTED = "Access Granted"
            const val SCENARIO_STATE_ACCESS_EXPIRED = "Access Expired"
        }

        /**
         * A [TestServer] client that implements [AccessTokenAwareHttpClient].
         */
        class TestClient(
            private val sessionEndpoint: URI,
            serviceEndpoint: URI,
            serializer: JsonSerializer = JsonSerializer(EmptyModule)
        ) : AccessTokenAwareHttpClient(serviceEndpoint, serializer) {

            /** Track the number of times [retrieveAccessToken] has been invoked. */
            val accessTokenRequestCount = atomic(0)

            override fun retrieveAccessToken(sessionResponse: HttpResponse<String>): String {
                accessTokenRequestCount.incrementAndGet()
                return sessionResponse.body()
            }

            override fun accessTokenHeader(): String = MOCK_SERVER_TOKEN_HEADER

            override fun session(): URI = sessionEndpoint

            override fun credential(): Credential {
                return Credential()
            }
        }

        /** Base test server endpoint as an URL. */
        private val baseEndpoint = URI.create("http://localhost:$port")

        /** Internal WireMock server that performs the request/response mocking. */
        private val server = WireMockServer(port)

        /** Path to create a login/authorized session using a bearer token encoded in the URL. */
        private val sessionPath = MOCK_SERVER_SESSION_PATH

        /** Path to expire a logged in session. */
        val sessionExpirePath = MOCK_SERVER_EXPIRE_SESSION_PATH

        /** Path to a mocked resource served by this test server. */
        val resourcePath = MOCK_SERVER_RESOURCE_PATH

        init {
            // Add session-related mapping handlers that also perform scenario state transitions.
            server.stubFor(
                    WireMock.post(WireMock.urlPathEqualTo(sessionPath))
                            .inScenario(scenario)
                            .whenScenarioStateIs(Scenario.STARTED)
                            .withQueryParam("refresh_token", EqualToPattern("bearer"))
                            .willReturn(
                                    WireMock.aResponse()
                                            .withHeader(HTTP_HEADER_CONTENT_TYPE, "text/plain")
                                            .withStatus(200)
                                            .withBody(SCENARIO_EXPIRE_ACCESS_TOKEN)
                            )
                            .willSetStateTo(SCENARIO_STATE_ACCESS_GRANTED)
            )
            server.stubFor(
                    WireMock.post(WireMock.urlPathEqualTo(sessionPath))
                            .inScenario(scenario)
                            .whenScenarioStateIs(SCENARIO_STATE_ACCESS_EXPIRED)
                            .withQueryParam("refresh_token", EqualToPattern("bearer"))
                            .willReturn(
                                    WireMock.aResponse()
                                            .withHeader(HTTP_HEADER_CONTENT_TYPE, "text/plain")
                                            .withStatus(200)
                                            .withBody(SCENARIO_EXPIRE_ACCESS_TOKEN)
                            )
                            .willSetStateTo(SCENARIO_STATE_ACCESS_GRANTED)
            )
            server.stubFor(
                    WireMock.post(WireMock.urlEqualTo(sessionExpirePath))
                            .inScenario(scenario)
                            .whenScenarioStateIs(SCENARIO_STATE_ACCESS_GRANTED)
                            .willReturn(WireMock.aResponse().withStatus(200))
                            .willSetStateTo(SCENARIO_STATE_ACCESS_EXPIRED)
            )

            server.stubFor(
                    WireMock.get(WireMock.urlEqualTo(MOCK_SERVER_RESOURCE_PATH))
                            .inScenario(scenario)
                            .whenScenarioStateIs(Scenario.STARTED)
                            .willReturn(WireMock.aResponse().withStatus(401))
            )
            server.stubFor(
                    WireMock.get(WireMock.urlEqualTo(MOCK_SERVER_RESOURCE_PATH))
                            .inScenario(scenario)
                            .whenScenarioStateIs(SCENARIO_STATE_ACCESS_EXPIRED)
                            .willReturn(WireMock.aResponse().withStatus(401))
            )
            server.stubFor(
                    WireMock.get(WireMock.urlEqualTo(MOCK_SERVER_RESOURCE_PATH))
                            .inScenario(scenario)
                            .whenScenarioStateIs(SCENARIO_STATE_ACCESS_GRANTED)
                            .withHeader(
                                    MOCK_SERVER_TOKEN_HEADER,
                                    EqualToPattern(SCENARIO_EXPIRE_ACCESS_TOKEN)
                            )
                            .willReturn(WireMock.aResponse().withStatus(200))
            )

            // Start the server.
            server.start()
        }

        /**
         * Create a new [AccessTokenAwareHttpClient] that connects to this server instance.
         *
         * @return
         *   a new [TestClient] instance that is a subtype of [AccessTokenAwareHttpClient].
         */
        fun newClient(): TestClient {
            val sessionUrlString = "$sessionPath?refresh_token=bearer"
            return TestClient(baseEndpoint.resolve(sessionUrlString), baseEndpoint)
        }

        fun stop() = server.stop()
    }

    /**
     * Test the automatic re-authentication upon a status 401 failure.
     */
    //@Test
    fun accessTokenExpired() {
        // Setup.
        val server = TestServer(18800, "Scenario-Expire-Access")
        val client = server.newClient()

        // HttpClient operations use suspendable functions, for simplicity of test setup, run them
        // on default/main thread in blocking fashion relative to this test method.
        runBlocking {
            // Verify that typical resource retrieval is ok.
            val getResult = client.get<String>(
                    path = server.resourcePath,
                    contentType = "text/plain",
                    headers = emptyList()
            )
            Assertions.assertThat(getResult.statusCode()).isEqualTo(200)
            Assertions.assertThat(client.accessTokenRequestCount.value).isEqualTo(1)

            // Post to a special URL that forces the test server to no longer have an authenticated
            // session for any client.
            val expireResult = client.post<Unit, Unit>(
                    path = server.sessionExpirePath,
                    contentType = "text/plain",
                    headers = emptyList(),
                    body = null
            )
            Assertions.assertThat(expireResult.statusCode()).isEqualTo(200)

            // Verify that after a test client has an existing auth token, but for whatever reason
            // the server will no longer treat the token as valid, the client would reauthenticate
            // and retrieve the result.
            val getAgainResult = client.get<String>(
                    path = server.resourcePath,
                    contentType = "text/plain",
                    headers = emptyList()
            )
            Assertions.assertThat(getAgainResult.statusCode()).isEqualTo(200)
            Assertions.assertThat(client.accessTokenRequestCount.value).isEqualTo(2)
        }

        // Teardown.
        server.stop()
    }
}
