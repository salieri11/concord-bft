/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.containing;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.verify;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrlPattern;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.http.Fault;
import com.github.tomakehurst.wiremock.stubbing.Scenario;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspConfig;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * Unit tests for Oauth2Controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = { Oauth2Controller.class })
@TestPropertySource(locations = "classpath:test.properties")
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class })
@ComponentScan(basePackageClasses = { Oauth2ControllerTests.class, HelenExceptionHandler.class })
public class Oauth2ControllerTests {
    private MockMvc mockMvc;

    @Autowired
    private WebApplicationContext webApplicationContext;

    @Autowired
    private Oauth2Controller oauth2Controller;

    @Autowired
    private AuthHelper authHelper;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    DefaultProfiles defaultProfiles;

    private CspConfig cspConfig;

    @Value("${vmbc.url.public}")
    private String vmbcUrl;

    @Value(Constants.OAUTH_CALLBACK)
    private String vmbcOauthCallback;

    @Value(Constants.AUTH_LOGIN)
    private String vmbcAuthLogin;

    @Value(Constants.AUTH_LOGOUT)
    private String vmbcAuthLogout;

    @Value(Constants.API_AUTH_TOKEN)
    private String vmbcAuthToken;


    private String oauthCallbackUrl;

    private String targetUri;

    private String invitationUri;

    private String logoutRedirectUri;

    private String logoutCspDiscovery;

    private WireMockServer server;

    // Random UUID for state.
    private String state = "07bf2452-8f19-4c44-80c5-9e801279f6d3";

    private String clientId = "vmbc-client";
    private String clientSecret = "vmbc-secret";
    private String code = "returncode";
    private String params =
            "grant_type=authorization_code" + "&code=returncode" + "&state=" + state
                + "&redirect_uri=http://localhost:9090/sample/auth/oauth";
    private MockHttpSession session;
    private ObjectMapper objectMapper;

    private AuthenticationContext authctx;


    // These are various Wiremock return mappings

    // Post Logout request. Return OK with logout redirect.
    MappingBuilder cspPostLogout() {
        String body = String.format("{\"url\":\"%s\"}", logoutRedirectUri);
        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_LOGOUT))
                .withHeader("Content-Type", containing("application/json"))
                .withRequestBody(equalTo("{\"idToken\":\"atoken\"}"))
                .willReturn(aResponse()
                        .withStatus(200)
                        .withHeader("Content-Type", "application/json")
                        .withBody(body));
        return r;
    }

    // Post Logout request, but give a bad gateway error
    MappingBuilder cspPostBadLogout() {
        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_LOGOUT))
                .withHeader("Content-Type", containing("application/json"))
                .withRequestBody(equalTo("{\"idToken\":\"atoken\"}"))
                .willReturn(aResponse()
                        .withStatus(502));
        return r;
    }

    // Return OK, with a valid token when using Forms
    MappingBuilder cspAuthGoodReturn() {
        CspCommon.CspAuthorizeResponse auth = new CspCommon.CspAuthorizeResponse();
        auth.setCspAuthToken("verylongvalidtoken");
        auth.setCspRefreshToken("validRefreshToken");
        auth.setExpiresIn(5);
        String body = "";
        try {
            body = objectMapper.writeValueAsString(auth);
        } catch (JsonProcessingException e) {
            // Nothing we can do.
        }

        MappingBuilder r =
                post(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN))
                .withBasicAuth(clientId, clientSecret)
                .withHeader("Content-Type", containing("application/x-www-form-urlencoded"))
                //.withRequestBody(equalTo(params))
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

    /**
     * Create mocks.
     */
    @BeforeEach
    public void setup() throws Exception {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(webApplicationContext)
                .apply(springSecurity())
                .build();
        oauthCallbackUrl =
                UriComponentsBuilder.fromUriString(vmbcUrl).path(vmbcOauthCallback).build().toUriString();
        targetUri = UriComponentsBuilder.fromUriString(Constants.AUTH_LOGIN_RETURN).build().toUriString();
        invitationUri = UriComponentsBuilder.fromUriString(Constants.AUTH_INVITATION).build().toUriString();
        cspConfig = mock(CspConfig.class);
        server = new WireMockServer(options().dynamicPort());
        server.start();
        WireMock.configureFor(server.port());
        String cspUrl = UriComponentsBuilder.newInstance().host("localhost").port(server.port()).scheme("http")
                .toUriString();
        when(cspConfig.getCspUrl()).thenReturn(cspUrl);
        logoutRedirectUri = UriComponentsBuilder.fromUriString(cspUrl).path(CspConstants.CSP_LOGOUT)
                .build().toUriString();
        logoutCspDiscovery = UriComponentsBuilder.fromUriString(cspUrl).path(CspConstants.CSP_DISCOVERY_PAGE)
                .build().toUriString();
        authctx = createContext("user@domain", UUID.randomUUID(),
                                              ImmutableList.of(VmbcRoles.ORG_USER),
                                              Collections.emptyList(), Collections.emptyList(),
                                              "atoken");
        session = new MockHttpSession();
        session.setAttribute("oauth-state", state);
        CacheManager cacheManger = mock(CacheManager.class);
        when(cacheManger.getCache(anyString())).thenReturn(mock(Cache.class));
        ReflectionTestUtils.setField(oauth2Controller, "cspConfig", cspConfig);
        ReflectionTestUtils.setField(oauth2Controller, "clientId", clientId);
        ReflectionTestUtils.setField(oauth2Controller, "clientSecret", clientSecret);
        ReflectionTestUtils.setField(oauth2Controller, "cacheManager", cacheManger);
        objectMapper = jacksonBuilder.build();
    }

    @Test
    public void testLoginNoTargetpath() throws Exception {
        // setup auth with no authToken.
        AuthenticationContext authctx =
                new AuthenticationContext(UUID.randomUUID(), UUID.randomUUID(), "user@domain",
                                          "atoken", Arrays.asList(VmbcRoles.ORG_USER));
        authHelper.setAuthenticationContext(authctx);
        SecurityContextHolder.getContext().setAuthentication(authctx);
        String redirectUri = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl())
                .path(CspConstants.CSP_DISCOVERY_PAGE).queryParam("client_id", "vmbc-client")
                .queryParam("redirect_uri", oauthCallbackUrl).queryParam("state", "*").build().toUriString();
        mockMvc.perform(get(vmbcAuthLogin)).andExpect(redirectedUrlPattern(redirectUri));
    }

    @Test
    public void testLoginWithSession() throws Exception {
        String redirectUri = UriComponentsBuilder.fromUriString(vmbcAuthLogin)
                .queryParam("session_cleaned", true)
                .build().toUriString();
        mockMvc.perform(get(vmbcAuthLogin)
                    .sessionAttr("csp-auth-token", "atoken"))
                    .andExpect(redirectedUrlPattern(redirectUri));
    }

    @Test
    public void testLoginOrg() throws Exception {
        String orgId = CspConstants.CSP_ORG_API + "/" + UUID.randomUUID().toString();
        String redirectUri = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl())
                .path(CspConstants.CSP_DISCOVERY_PAGE).queryParam("client_id", "vmbc-client")
                .queryParam("redirect_uri", oauthCallbackUrl).queryParam("state", "*")
                .queryParam("orgLink", orgId).build().toUriString();

        String getUri = String.format("%s?orgLink=%s", vmbcAuthLogin, orgId);

        mockMvc.perform(get(getUri)).andDo(print()).andExpect(redirectedUrlPattern(redirectUri));
    }

    @Test
    public void testDiscoveryWithOrg() throws Exception {
        String orgLink = CspConstants.CSP_ORG_API + "/" + UUID.randomUUID().toString();
        String redirectUri = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl())
                .path(CspConstants.CSP_DISCOVERY_PAGE).queryParam("client_id", "vmbc-client")
                .queryParam("redirect_uri", oauthCallbackUrl).queryParam("state", "*")
                .queryParam("orgLink", orgLink).build().toUriString();

        String getUri = String.format("%s?orgLink=%s&session_cleaned=true", vmbcAuthLogin, orgLink);

        mockMvc.perform(get(getUri)).andExpect(redirectedUrlPattern(redirectUri));
    }

    @Test
    public void testDiscoveryWithUser() throws Exception {
        String orgLink = CspConstants.CSP_ORG_API + "/" + UUID.randomUUID().toString();
        MockHttpSession session = new MockHttpSession();
        String redirectUri = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl())
                .path(CspConstants.CSP_DISCOVERY_PAGE).queryParam("client_id", "vmbc-client")
                .queryParam("redirect_uri", oauthCallbackUrl).queryParam("state", "*")
                .queryParam("orgLink", orgLink).build().toUriString();

        String getUri = String.format("%s?orgLink=%s&session_cleaned=true&user=new", vmbcAuthLogin, orgLink);

        mockMvc.perform(get(getUri).session((session))).andExpect(redirectedUrlPattern(redirectUri));
        Assertions.assertEquals("new", session.getAttribute("user"));
    }

    @Test
    public void testDiscoveryWithInvitation() throws Exception {
        String orgLink = CspConstants.CSP_ORG_API + "/" + UUID.randomUUID().toString();
        MockHttpSession session = new MockHttpSession();
        String redirectUri = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl())
                .path(CspConstants.CSP_DISCOVERY_PAGE).queryParam("client_id", "vmbc-client")
                .queryParam("redirect_uri", oauthCallbackUrl).queryParam("state", "*")
                .queryParam("orgLink", orgLink).build().toUriString();

        String getUri = String.format("%s?orgLink=%s&session_cleaned=true&serviceInvitationLink=service-invitation",
                                      vmbcAuthLogin, orgLink);

        mockMvc.perform(get(getUri).session((session))).andExpect(redirectedUrlPattern(redirectUri));
        Assertions.assertEquals("service-invitation", session.getAttribute("serviceInvitationLink"));
    }

    @Test
    public void testLogout() throws Exception {
        server.stubFor(cspPostLogout());
        mockMvc.perform(get(vmbcAuthLogout).with(authentication(authctx))
                .sessionAttr("csp-auth-token", "atoken")
                .sessionAttr("token-id", "atoken"))
                .andExpect(redirectedUrl(logoutRedirectUri));
        server.verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_LOGOUT)));
    }

    @Test
    public void testLogoutNoSession() throws Exception {
        WireMock.reset();
        stubFor(cspPostLogout());
        mockMvc.perform(get(vmbcAuthLogout).with(authentication(authctx)))
                .andExpect(redirectedUrl(logoutCspDiscovery));
        verify(0, postRequestedFor(urlPathEqualTo(CspConstants.CSP_LOGOUT)));
    }

    @Test
    public void testBadLogout() throws Exception {
        WireMock.reset();
        stubFor(cspPostBadLogout());
        mockMvc.perform(get(vmbcAuthLogout).with(authentication(authctx))
                .sessionAttr("csp-auth-token", "atoken")
                .sessionAttr("token-id", "atoken"))
                .andExpect(status().is5xxServerError());
    }

    @Test
    public void testCallback() throws Exception {
        WireMock.reset();
        stubFor(cspAuthGoodReturn());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);

        mockMvc.perform(get(getUri).session(session)).andExpect(redirectedUrl(targetUri));

        verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackUserFlag() throws Exception {
        WireMock.reset();
        stubFor(cspAuthGoodReturn());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);

        session.setAttribute(Constants.NEW_USER_PARAM, "new");
        targetUri =
                UriComponentsBuilder.fromUriString(targetUri).queryParam(Constants.NEW_USER_PARAM, "new")
                        .build().toUriString();
        mockMvc.perform(get(getUri).session(session)).andExpect(redirectedUrl(targetUri));

        verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testInvitationCallback() throws Exception {
        WireMock.reset();
        stubFor(cspAuthGoodReturn());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);
        session.setAttribute(Constants.CSP_INVITATION_LINK, "invitation");

        mockMvc.perform(get(getUri).session(session)).andExpect(redirectedUrl(invitationUri));

        verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackWithSingleFail() throws Exception {
        WireMock.reset();
        stubFor(cspAuthFault().inScenario("fail one").whenScenarioStateIs(Scenario.STARTED).willSetStateTo("OneFail"));
        stubFor(cspAuthGoodReturn().inScenario("fail one").whenScenarioStateIs("OneFail"));

        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);
        String redirect = UriComponentsBuilder.fromUriString(targetUri).build().toUriString();
        mockMvc.perform(get(getUri).session(session)).andExpect(redirectedUrl(redirect));
        verify(2, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackBadState() throws Exception {
        WireMock.reset();
        stubFor(cspAuthGoodReturn());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, "badState");
        mockMvc.perform(get(getUri).session(session)).andExpect(status().is5xxServerError());
        verify(0, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackNoSession() throws Exception {
        WireMock.reset();
        stubFor(cspAuthGoodReturn());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, "badState");
        mockMvc.perform(get(getUri)).andExpect(status().is5xxServerError());
        verify(0, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackGatewayError() throws Exception {
        WireMock.reset();
        stubFor(cspAuthGatewayError());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);
        mockMvc.perform(get(getUri).session(session)).andExpect(status().is5xxServerError());
        verify(3, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackFault() throws Exception {
        WireMock.reset();
        stubFor(cspAuthFault());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);
        mockMvc.perform(get(getUri).session(session)).andExpect(status().is5xxServerError());
        verify(3, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testCallbackUnauthorizedAccess() throws Exception {
        WireMock.reset();
        stubFor(cspAuthUnauthorized());
        String getUri = String.format("%s?code=%s&state=%s", vmbcOauthCallback, code, state);
        mockMvc.perform(get(getUri).session(session)).andExpect(status().is5xxServerError());
        verify(1, postRequestedFor(urlPathEqualTo(CspConstants.CSP_OAUTH_TOKEN)));
    }

    @Test
    public void testGetAuthToken() throws Exception {
        String body = mockMvc.perform(get(vmbcAuthToken).sessionAttr("token-id", "id-token")
                                              .with(authentication(authctx)))
                .andExpect(status().is2xxSuccessful()).andReturn().getResponse()
                .getContentAsString();
        Assertions.assertEquals(
                "{\"auth_token\":\"atoken\",\"id_token\":\"id-token\",\"email\":\"user@domain\",\"last_login\":0}",
                body);
    }
}
