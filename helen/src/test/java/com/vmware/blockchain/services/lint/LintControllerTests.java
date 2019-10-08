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

import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.csp.CspAuthenticationHelper;

/**
 * Tests for the LintProxyController.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:lint-test.properties")
@WebMvcTest(secure = false, controllers = LintProxyController.class)
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { LintControllerTests.class })
public class LintControllerTests {
    private static final String CON_ID = "0d83f75d-edad-4c54-8dce-64d5b98df2b7";

    @Autowired
    MockMvc mockMvc;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    CspAuthenticationHelper cspAuthHelper;

    @Mock
    RestTemplate restTemplate;

    @Autowired
    LintProxyController lintProxyController;

    @Captor
    ArgumentCaptor<String> uriCapcha;

    @Captor
    ArgumentCaptor<HttpEntity<String>> httpCapcha;

    @Captor
    ArgumentCaptor<HttpMethod> methodCapcha;

    @Captor
    ArgumentCaptor<Class<?>> classCapcha;


    @BeforeEach
    void init() {
        when(cspAuthHelper.fetchAuthTokenFromRefreshToken(anyString())).thenReturn("anAuthToken");
        when(authHelper.getOrganizationId()).thenReturn(UUID.fromString(CON_ID));
        when(restTemplate.exchange(any(URI.class), any(HttpMethod.class), any(HttpEntity.class), any(Class.class)))
            .thenReturn(new ResponseEntity<String>("the answer", HttpStatus.OK));
        ReflectionTestUtils.setField(lintProxyController, "restTemplate", restTemplate);
        ReflectionTestUtils.setField(lintProxyController, "cspAuthHelper", cspAuthHelper);
    }

    @Test
    void testCspUrl() throws Exception {
        // Make sure defaut cspUrl is pointing to production
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

        mockMvc.perform(post("/api/lint/log")
                .contentType(MediaType.APPLICATION_JSON).content("{\"one\": \"two\"}"))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals("{\"one\": \"two\"}", entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }


    @Test
    void sqlTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE consortium_id = '0d83f75d-edad-4c54-8dce-64d5b98df2b7' "
                + "AND function = 'ethLogger' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }

    @Test
    void functionTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE consortium_id = '0d83f75d-edad-4c54-8dce-64d5b98df2b7' "
                + "AND function = 'logger' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?function=logger")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }

    @Test
    void queryParamTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE consortium_id = '0d83f75d-edad-4c54-8dce-64d5b98df2b7' "
                + "AND function = 'ethLogger' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?odata=*")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log?odata=*", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }

    @Test
    void functionAndQueryparamTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE consortium_id = '0d83f75d-edad-4c54-8dce-64d5b98df2b7' "
                + "AND function = 'logger' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log?odate=*&function=logger")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log?odate=*", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }

    @Test
    void whereTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs WHERE user = 'userId' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE (consortium_id = '0d83f75d-edad-4c54-8dce-64d5b98df2b7' "
                + "AND function = 'ethLogger') AND (user = 'userId' ) ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }

    @Test
    void missingSqlTest() throws Exception {
        // If there's no log query field, this
        final String query =
                "{\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";
        final String response =
                "{\"start\":1548092484611,\"end\":1548697284611,\"rows\":20}";

        mockMvc.perform(post("/api/lint/log")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }

    @Test
    void extraFieldTest() throws Exception {
        final String query =
                "{\"logQuery\":\"SELECT * FROM logs ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20,\"color\":\"red\"}";
        final String response =
                "{\"logQuery\":\"SELECT * FROM logs WHERE consortium_id = '0d83f75d-edad-4c54-8dce-64d5b98df2b7' "
                + "AND function = 'ethLogger' ORDER BY ingest_timestamp DESC\","
                + "\"start\":1548092484611,\"end\":1548697284611,\"rows\":20,\"color\":\"red\"}";

        mockMvc.perform(post("/api/lint/log")
                .contentType(MediaType.APPLICATION_JSON).content(query))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.POST, methodCapcha.getValue());
        Assertions.assertEquals(response, entity.getBody());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());
    }


    @Test
    void getTest() throws Exception {
        mockMvc.perform(get("/api/lint/log"))
                .andExpect(status().isOk());
        // capture the values handed in to restTemplate.exchange
        verify(restTemplate).exchange(uriCapcha.capture(), methodCapcha.capture(), httpCapcha.capture(),
                classCapcha.capture());

        final HttpEntity<String> entity = httpCapcha.getValue();

        Assertions.assertEquals("/log", uriCapcha.getValue().toString());
        Assertions.assertEquals(HttpMethod.GET, methodCapcha.getValue());
        Assertions.assertEquals("Bearer anAuthToken", entity.getHeaders().getFirst("Authorization"));
        Assertions.assertEquals(String.class, classCapcha.getValue());

    }
}
