/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.util.UriComponentsBuilder;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.stubbing.Scenario;
import com.vmware.blockchain.common.restclient.RestClientException;

/**
 * test csp calls.
 */
@ExtendWith(SpringExtension.class)
public class CspAuthenticationHelperTest {

    private static WireMockServer server;

    private static String cspUrl;

    private String goodResponse = "{\r\n"
            + "  \"access_token\": \"access-token\",\r\n"
            + "  \"expires_in\": 3000,\r\n"
            + "  \"id_token\": \"id-token\",\r\n"
            + "  \"refresh_token\": \"refresh-token\",\r\n"
            + "  \"scope\": \"scope\",\r\n"
            + "  \"token_type\": \"bearer\"\r\n" + "}";

    @BeforeAll
    static void initWiremock() {
        server = new WireMockServer(options().dynamicPort());
        server.start();
        cspUrl =
                UriComponentsBuilder.newInstance().host("localhost").port(server.port()).scheme("http").toUriString();
    }

    @AfterAll
    static void shutdownWiremock() {
        server.stop();
    }


    @Test
    void testAuthtoken() {
        server.resetAll();
        server.stubFor(post(urlEqualTo(CspConstants.CSP_LOGIN_REFRESH_TOKEN))
                .withRequestBody(equalTo("refresh_token=refresh-token"))
                .willReturn(
                        aResponse().withHeader("Content-Type", "application/json")
                            .withBody(goodResponse).withStatus(200)));
        CspAuthenticationHelper helper = new CspAuthenticationHelper(cspUrl);
        String token = helper.fetchAuthTokenFromRefreshToken("refresh-token");
        Assertions.assertEquals("access-token", token);
    }

    @Test
    void testClientCredentialGrant() {
        //todo: use formdata for good and bad requests
        server.resetAll();
        server.stubFor(post(urlEqualTo(CspConstants.CSP_OAUTH_TOKEN))
                .willReturn(
                        aResponse().withHeader("Content-Type", "application/json")
                                .withBody(goodResponse).withStatus(200)));
        CspAuthenticationHelper helper = new CspAuthenticationHelper(cspUrl);
        String token = helper.getClientCredentialsGrant("client_id", "client_secret", "org_id");
        Assertions.assertEquals("access-token", token);
    }

    @Test
    void testFail() {
        // always fail
        server.resetAll();
        server.stubFor(post(urlEqualTo(CspConstants.CSP_LOGIN_REFRESH_TOKEN))
                .withRequestBody(equalTo("refresh_token=refresh-token"))
                .willReturn(
                        aResponse().withStatus(502)));
        CspAuthenticationHelper helper = new CspAuthenticationHelper(cspUrl);
        Assertions.assertThrows(RestClientException.class,
            () -> helper.fetchAuthTokenFromRefreshToken("refresh-token"));
        // Should have tried three times
        server.verify(3, postRequestedFor(urlEqualTo(CspConstants.CSP_LOGIN_REFRESH_TOKEN)));
    }

    @Test
    void testRetry() {
        server.resetAll();
        // First call will fail with 502
        server.stubFor(post(urlEqualTo(CspConstants.CSP_LOGIN_REFRESH_TOKEN))
                .inScenario("retry test")
                .whenScenarioStateIs(Scenario.STARTED)
                .withRequestBody(equalTo("refresh_token=refresh-token"))
                .willReturn(
                        aResponse().withStatus(502)).willSetStateTo("failed one"));

        // Second call will work
        server.stubFor(post(urlEqualTo(CspConstants.CSP_LOGIN_REFRESH_TOKEN))
                .inScenario("retry test")
                .whenScenarioStateIs("failed one")
                .withRequestBody(equalTo("refresh_token=refresh-token"))
                .willReturn(
                        aResponse().withHeader("Content-Type", "application/json")
                            .withBody(goodResponse).withStatus(200)));

        CspAuthenticationHelper helper = new CspAuthenticationHelper(cspUrl);
        String token = helper.fetchAuthTokenFromRefreshToken("refresh-token");
        Assertions.assertEquals("access-token", token);
        // Make sure we hit the post endpoint twice
        server.verify(2, postRequestedFor(urlEqualTo(CspConstants.CSP_LOGIN_REFRESH_TOKEN)));
    }
}
