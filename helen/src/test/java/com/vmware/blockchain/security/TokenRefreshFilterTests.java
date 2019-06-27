/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.containing;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

import java.util.concurrent.TimeUnit;

import javax.servlet.FilterChain;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.http.Fault;
import com.github.tomakehurst.wiremock.stubbing.Scenario;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspConfig;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.restclient.RestClientException;
import com.vmware.blockchain.utils.ControllerTestConfig;

/**
 * Unit tests for TokenRefreshFilter.
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {MvcConfig.class, ControllerTestConfig.class})
public class TokenRefreshFilterTests {

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private HttpServletRequest request = mock(HttpServletRequest.class);
    private HttpServletResponse response = mock(HttpServletResponse.class);
    private FilterChain chain = mock(FilterChain.class);
    private CspConfig cspConfig = mock(CspConfig.class);
    HttpSession session = mock(HttpSession.class);

    private TokenRefreshFilter refreshFilter = new TokenRefreshFilter(cspConfig, "ss-client", "ss-secret");

    private String refreshToken = "validRefreshToken";
    private String clientId = "ss-client";
    private String clientSecret = "ss-secret";
    private String params = "grant_type=refresh_token&refresh_token=validRefreshToken";

    private static WireMockServer server;
    private static String cspUrl;

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

    // These are various Wiremock return mappings

    // Return OK, with a valid token
    MappingBuilder cspAuthGoodReturn() {
        CspCommon.CspAuthorizeResponse auth = new CspCommon.CspAuthorizeResponse();
        auth.setCspAuthToken("verylongvalidtoken");
        auth.setCspRefreshToken("validRefreshToken");
        auth.setIdToken("anOpenIdToken");
        auth.setExpiresIn(5);
        String body = "";
        try {
            body = jacksonBuilder.build().writeValueAsString(auth);
        } catch (JsonProcessingException e) {
            // Nothing we can do.
        }

        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN))
                .withHeader("Content-Type", containing("application/x-www-form-urlencoded"))
                .withBasicAuth(clientId, clientSecret)
                .withRequestBody(equalTo(params))
                .willReturn(aResponse()
                        .withStatus(200)
                        .withHeader("Content-Type", "application/json")
                        .withBody(body));
        return r;
    }

    // Return unauthorized
    MappingBuilder cspAuthUnauthorized() {
        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN))
                .willReturn(aResponse().withStatus(403));
        return r;
    }

    // Simulate gateway error
    MappingBuilder cspAuthGatewayError() {
        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN))
                .willReturn(aResponse().withStatus(503));
        return r;
    }

    // simulate connection closed
    MappingBuilder cspAuthFault() {
        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN))
                .willReturn(aResponse().withFault(Fault.RANDOM_DATA_THEN_CLOSE));
        return r;
    }

    Long validTime() {
        return Long.valueOf(System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(10));
    }

    Long expiredTime() {
        return Long.valueOf(System.currentTimeMillis() - TimeUnit.SECONDS.toMillis(10));
    }

    /**
     * set up cspConfig.
     */
    @BeforeEach
    public void setup() {
        server.resetAll();
        String cspUrl = UriComponentsBuilder.newInstance().host("localhost").port(server.port()).scheme("http")
                .toUriString();
        when(cspConfig.getCspUrl()).thenReturn(cspUrl);
        ReflectionTestUtils.setField(refreshFilter, "clientId", clientId);
        ReflectionTestUtils.setField(refreshFilter, "clientSecret", clientSecret);
    }

    /**
     * Test no HttpSession.
     * 0 calls to csp, 1 call to chain.doFilter
     */
    @Test
    public void noSession() throws Exception {
        server.stubFor(cspAuthGoodReturn());
        when(request.getSession(false)).thenReturn(null);
        refreshFilter.doFilter(request, response, chain);
        server.verify(0, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(1)).doFilter(request, response);
    }

    /**
     * Test when session is still valid.
     * 0 call to csp, 1 call to chain.doFilter
     */
    @Test
    public void tokenStillValid() throws Exception {
        server.stubFor(cspAuthGoodReturn());
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.TOKEN_EXPIRES_AT)).thenReturn(validTime());
        refreshFilter.doFilter(request, response, chain);
        server.verify(0, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(1)).doFilter(request, response);
    }

    /**
     * Test refresh with normal exchange with oauthForm enalbed
     * 1 call to csp, 1 call to chain.doFilter, 1 call to session.setAttribute
     */
    @Test
    public void tokenNeedsRefreshForm() throws Exception {
        server.stubFor(cspAuthGoodReturn());
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.TOKEN_EXPIRES_AT)).thenReturn(expiredTime());
        when(session.getAttribute(Constants.TOKEN_REFRESH)).thenReturn(refreshToken);
        refreshFilter.doFilter(request, response, chain);
        server.verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(1)).doFilter(request, response);
        Mockito.verify(session, times(1)).setAttribute(Constants.AUTH_HEADER_NAME, "verylongvalidtoken");
        Mockito.verify(session, times(1)).setAttribute(Constants.TOKEN_ID, "anOpenIdToken");
    }

    /**
     * test refresh fails.  Filter continues chain at this point.
     * 1 call to csp, 1 call to chain.doFilter, 0 calls to session.setAttribute
     */
    @Test
    public void tokenRefreshFails() throws Exception {
        server.stubFor(cspAuthUnauthorized());
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.TOKEN_EXPIRES_AT)).thenReturn(expiredTime());
        refreshFilter.doFilter(request, response, chain);
        server.verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(1)).doFilter(request, response);
        Mockito.verify(session, times(0)).setAttribute(anyString(), anyString());
    }

    /**
     * Simulate csp fault (closed connection).
     * Skyscraper exception thrown
     * 3 calls to csp (from retries), 0 calls to chain.doFilter, 0 calls to session.setAttribute
     */
    @Test
    public void tokenRefreshFaults() throws Exception {
        server.stubFor(cspAuthFault());
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.TOKEN_EXPIRES_AT)).thenReturn(expiredTime());
        try {
            refreshFilter.doFilter(request, response, chain);
        } catch (RestClientException e) {
            // This is what we expect.  Catch it to be sure that counts are right
        }
        server.verify(3, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(0)).doFilter(request, response);
        Mockito.verify(session, times(0)).setAttribute(anyString(), anyString());
    }

    /**
     * Simulate csp gateway error (pretty much the same as fault).
     * Skyscraper exception thrown
     * 3 calls to csp (from retries), 0 calls to chain.doFilter, 0 calls to session.setAttribute
     */
    @Test
    public void tokenGatewayError() throws Exception {
        server.stubFor(cspAuthGatewayError());
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.TOKEN_EXPIRES_AT)).thenReturn(expiredTime());
        try {
            refreshFilter.doFilter(request, response, chain);
        } catch (RestClientException e) {
            // This is what we expect.  Catch it to be sure that counts are right
        }
        server.verify(3, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(0)).doFilter(request, response);
        Mockito.verify(session, times(0)).setAttribute(anyString(), anyString());
    }

    /**
     * Simulate one failure, followed by a good response.
     * 2 calls to csp, 1 call to chain.doFilter, 1 call session.setAttribute
     */
    @Test
    public void tokenOneFail() throws Exception {
        server.stubFor(cspAuthFault().inScenario("fail one")
                .whenScenarioStateIs(Scenario.STARTED)
                .willSetStateTo("OneFail"));
        server.stubFor(cspAuthGoodReturn().inScenario("fail one")
                .whenScenarioStateIs("OneFail"));

        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(Constants.TOKEN_EXPIRES_AT)).thenReturn(expiredTime());
        when(session.getAttribute(Constants.TOKEN_REFRESH)).thenReturn(refreshToken);
        refreshFilter.doFilter(request, response, chain);
        server.verify(2, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
        Mockito.verify(chain, times(1)).doFilter(request, response);
        Mockito.verify(session, times(1)).setAttribute(Constants.AUTH_HEADER_NAME, "verylongvalidtoken");
        Mockito.verify(session, times(1)).setAttribute(Constants.TOKEN_ID, "anOpenIdToken");
    }

}
