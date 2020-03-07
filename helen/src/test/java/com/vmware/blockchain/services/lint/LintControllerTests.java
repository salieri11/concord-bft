/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.lint;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.NestedServletException;

import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.csp.CspAuthenticationHelper;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;

/**
 * Tests for the LintProxyController.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:lint-test.properties")
@WebMvcTest(secure = false, controllers = LintProxyController.class)
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { LintControllerTests.class })
public class LintControllerTests {
    private static final String CONSORTIUM_ID = "0d83f75d-edad-4c54-8dce-64d5b98df2b7";
    private static final String REPLICA_ID = "542b9855-443b-4dd8-bde0-29bfc717acaa";
    private static final String BLOCKCHAIN_ID = "7f94bbe9-fa8d-4143-9759-8912461d09a3";

    @Autowired
    MockMvc mockMvc;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    CspAuthenticationHelper cspAuthHelper;

    @MockBean
    ReplicaService replicaService;

    @Mock
    RestTemplate restTemplate;

    @Autowired
    LintProxyController lintProxyController;

    @Captor
    ArgumentCaptor<String> uriCaptcha;

    @Captor
    ArgumentCaptor<HttpEntity<String>> httpCaptcha;

    @Captor
    ArgumentCaptor<HttpMethod> methodCaptcha;

    @Captor
    ArgumentCaptor<Class<?>> classCaptcha;


    @BeforeEach
    void init() {
        Replica replica = new Replica();
        replica.setId(UUID.fromString(REPLICA_ID));
        replica.setBlockchainId(UUID.fromString(BLOCKCHAIN_ID));
        when(replicaService.get(UUID.fromString(REPLICA_ID))).thenReturn(replica);
        when(authHelper.canAccessChain(UUID.fromString(BLOCKCHAIN_ID))).thenReturn(true);
        when(cspAuthHelper.getClientCredentialsGrant(anyString(), anyString(), anyString())).thenReturn("anAuthToken");
        when(authHelper.getOrganizationId()).thenReturn(UUID.fromString(CONSORTIUM_ID));
        when(restTemplate.exchange(any(URI.class), any(HttpMethod.class), any(HttpEntity.class), any(Class.class)))
            .thenReturn(new ResponseEntity<String>("the answer", HttpStatus.OK));
        ReflectionTestUtils.setField(lintProxyController, "restTemplate", restTemplate);
        ReflectionTestUtils.setField(lintProxyController, "cspAuthHelper", cspAuthHelper);
    }

    @Test
    void testCspUrl() throws Exception {
        // Make sure default cspUrl is pointing to production
        String cspUrl = (String) ReflectionTestUtils.getField(lintProxyController, "cspUrl");
        Assertions.assertEquals("https://console.cloud.vmware.com", cspUrl);
    }

    @Test
    void simpleTest() throws Exception {
        // Check the following in the proxy call
        //  - URI value
        //  - HTTP Method
        //  - Body proxied
        //  - Authorization header

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content("{\"one\": \"two\"}"))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals("{\"one\": \"two\"}", entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void missingReplicaIdTest() {

        NestedServletException ex =
                Assertions.assertThrows(NestedServletException.class, () ->
                        mockMvc.perform(post("/api/lint/log")
                                .contentType(MediaType.APPLICATION_JSON).content("{\"one\": \"two\"}"))
                                .andExpect(status().isOk()));

        Assertions.assertTrue(ex.getCause() instanceof BadRequestException);
        Assertions.assertEquals(ErrorCodeType.BAD_REQUEST_PARAM.getErrorCodeTypeValue(), ex.getCause().getMessage());
    }

    @Test
    void replicaAuthTest() {

        Replica replica = new Replica();
        UUID replicaId = UUID.randomUUID();
        replica.setId(replicaId);
        replica.setBlockchainId(UUID.randomUUID());
        when(replicaService.get(replicaId)).thenReturn(replica);
        when(authHelper.canAccessChain(any(UUID.class))).thenReturn(false);

        NestedServletException ex =
            Assertions.assertThrows(NestedServletException.class, () ->
                mockMvc.perform(post("/api/lint/log?blockchain_id=" + replicaId)
                    .contentType(MediaType.APPLICATION_JSON).content("{\"one\": \"two\"}"))
                    .andExpect(status().isOk()));

        Assertions.assertTrue(ex.getCause() instanceof BadRequestException);
        Assertions.assertEquals(ErrorCodeType.CANNOT_ACCESS_BLOCKCHAIN.getErrorCodeTypeValue(),
                ex.getCause().getMessage());
    }


    @Test
    void sqlTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE blockchain_id = '" + BLOCKCHAIN_ID
                + "' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void queryParamTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE blockchain_id = '" + BLOCKCHAIN_ID
                + "' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?odata=*&blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log?odata=*", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void whereTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs WHERE user = 'userId' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE (blockchain_id = '" + BLOCKCHAIN_ID + "') "
                + "AND (user = 'userId' ) ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void whereTestTwoConditions() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs WHERE service_name = 'concord' AND level = 'INFO' "
                        + "ORDER BY ingest_timestamp DESC\","
                        + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE (blockchain_id = '" + BLOCKCHAIN_ID + "') "
                        + "AND (service_name = 'concord' AND level = 'INFO' ) "
                        + "ORDER BY ingest_timestamp DESC\","
                        + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void whereTestMultipleVerbosity() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs WHERE (level = 'INFO' OR level = 'ERROR') "
                        + "ORDER BY ingest_timestamp DESC\","
                        + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE (blockchain_id = '" + BLOCKCHAIN_ID + "') "
                        + "AND ((level = 'INFO' OR level = 'ERROR') ) "
                        + "ORDER BY ingest_timestamp DESC\","
                        + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void whereTestMultipleConditionsAndVerbosity() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs WHERE service_name = 'concord' AND "
                        + "(level = 'INFO' OR level = 'ERROR') ORDER BY ingest_timestamp DESC\","
                        + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE (blockchain_id = '" + BLOCKCHAIN_ID + "') "
                        + "AND (service_name = 'concord' AND (level = 'INFO' OR level = 'ERROR') ) "
                        + "ORDER BY ingest_timestamp DESC\","
                        + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void missingSqlTest() throws Exception {
        // If there's no log query field, this
        final String query =
                "{\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }

    @Test
    void extraFieldTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20,\"color\":\"red\"}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE blockchain_id = '" + BLOCKCHAIN_ID + "' "
                + "ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20,\"color\":\"red\"}";

        mockMvc.perform(post("/api/lint/log?blockchain_id=" + BLOCKCHAIN_ID)
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCaptcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());
    }


    @Test
    void getTest() throws Exception {
        mockMvc.perform(get("/api/lint/log/12e41ed8a9a77c7559748e221ba72"))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCaptcha.capture(), methodCaptcha.capture(), httpCaptcha.capture(),
                classCaptcha.capture());

        final HttpEntity<String> entity = httpCaptcha.getValue();

        Assertions.assertEquals("/log/12e41ed8a9a77c7559748e221ba72", uriCaptcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.GET, methodCaptcha.getValue());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCaptcha.getValue());

    }
}
