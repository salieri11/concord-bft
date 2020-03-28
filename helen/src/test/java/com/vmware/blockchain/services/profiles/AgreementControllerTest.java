/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.databind.ObjectMapper;

import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.ManagedChannel;

/**
 * Tests for AgreementController.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(secure = false, controllers = { AgreementController.class })
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { AgreementController.class, HelenExceptionHandler.class})
public class AgreementControllerTest {
    private static final Logger logger = LogManager.getLogger(AgreementController.class);

    private AuthenticationContext adminAuth;

    static final UUID ORG_1 = UUID.fromString("fdd53f14-acaa-4c55-a41c-cad76154cd1f");

    static final String firstName = "Blob";
    static final String lastName = "Blobberson";
    static final String company = "BLOB LLC";

    @Autowired
    private WebApplicationContext context;

    @Autowired
    MockMvc mockMvc;

    @MockBean
    OperationContext operationContext;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    AgreementService agreementService;

    @MockBean
    private ProfilesService prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private ConcordProperties concordProperties;

    @MockBean
    private ConcordConnectionPool connectionPool;

    @MockBean
    private KeystoreService keystoreService;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    private BlockchainService blockchainService;

    @MockBean
    private ConsortiumService consortiumService;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    GenericDao genericDao;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    TaskService taskService;

    @Autowired
    DefaultProfiles defaultProfiles;

    @MockBean
    @Qualifier("provisioningServerChannel")
    ManagedChannel channel;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    ObjectMapper objectMapper;

    @BeforeEach
    void init() throws Exception {
        Agreement a1 = new Agreement();

        a1.setAccepted(true);
        a1.setFirstName(firstName);
        a1.setLastName(lastName);
        a1.setCompany(company);
        a1.setAcceptedOn(new Date());
        a1.setOrgId(ORG_1);

        List<Agreement> agreementList =
                Arrays.asList(a1);
        Mockito.when(organizationService.getAgreements(ORG_1)).thenReturn(agreementList);
        Mockito.when(authHelper.getOrganizationId()).thenReturn(ORG_1);
        objectMapper = jacksonBuilder.build();
    }

    //    Some tests are commented out as we don't have org id in auth helper right now

    //    @Test
    //    void getAgreementFromId() throws Exception {
    //        MvcResult mvcResult = mockMvc.perform(get("/api/organizations/" + ORG_1.toString() + "/agreements")
    //                .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    //
    //        String body = mvcResult.getResponse().getContentAsString();
    //
    //        body = body.substring(1, body.length() - 1);
    //
    //        Agreement agreement = objectMapper.readValue(body, Agreement.class);
    //
    //        Assertions.assertTrue(agreement.isAccepted());
    //        Assertions.assertEquals(firstName, agreement.getFirstName());
    //        Assertions.assertEquals(lastName, agreement.getLastName());
    //        Assertions.assertEquals(company, agreement.getCompany());
    //    }

    @Test
    void getAgreementWithoutId() throws Exception {
        MvcResult mvcResult = mockMvc.perform(get("/api/organizations/agreements")
                .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        String body = mvcResult.getResponse().getContentAsString();

        body = body.substring(1, body.length() - 1);

        Agreement agreement = objectMapper.readValue(body, Agreement.class);

        Assertions.assertTrue(agreement.isAccepted());
        Assertions.assertEquals(firstName, agreement.getFirstName());
        Assertions.assertEquals(lastName, agreement.getLastName());
        Assertions.assertEquals(company, agreement.getCompany());
    }

    //    @Test
    //    void doPost() throws Exception {
    //        String content = String.format("    {"
    //                + "        \"accepted\": \"true\","
    //                + "        \"first_name\": \"John\","
    //                + "        \"last_name\": \"Smith\","
    //                + "        \"company\": \"Glob\""
    //                + "    }");
    //
    //        mockMvc.perform(post("/api/organizations/" + ORG_1.toString() + "/agreements")
    //                .with(authentication(adminAuth))
    //                .contentType(MediaType.APPLICATION_JSON)
    //                .content(content))
    //                .andExpect(status().isOk()).andReturn();
    //    }

    @Test
    void doPostWithoutId() throws Exception {
        String content = String.format("    {"
                + "        \"accepted\": \"true\","
                + "        \"first_name\": \"John\","
                + "        \"last_name\": \"Smith\","
                + "        \"company\": \"Glob\""
                + "    }");

        mockMvc.perform(post("/api/organizations/agreements")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(content))
                .andExpect(status().isOk()).andReturn();
    }

    //    @Test
    //    void doPostWithoutAccepting() throws Exception {
    //        String content = String.format("    {"
    //                + "        \"accepted\": \"false\","
    //                + "        \"first_name\": \"John\","
    //                + "        \"last_name\": \"Smith\","
    //                + "        \"company\": \"Glob\""
    //                + "    }");
    //
    //        mockMvc.perform(post("/api/organizations/" + ORG_1.toString() + "/agreements")
    //                .with(authentication(adminAuth))
    //                .contentType(MediaType.APPLICATION_JSON)
    //                .content(content))
    //                .andExpect(status().isBadRequest());
    //    }

    @Test
    void doPostWithoutIdWithoutAccepting() throws Exception {
        String content = String.format("    {"
                + "        \"accepted\": \"false\","
                + "        \"first_name\": \"John\","
                + "        \"last_name\": \"Smith\","
                + "        \"company\": \"Glob\""
                + "    }");

        mockMvc.perform(post("/api/organizations/agreements")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(content))
                .andExpect(status().isBadRequest());
    }

    //    @Test
    //    void doPostWithoutFirstName() throws Exception {
    //        String content = String.format("    {"
    //                + "        \"accepted\": \"true\","
    //                + "        \"first_name\": \"\","
    //                + "        \"last_name\": \"Smith\","
    //                + "        \"company\": \"Glob\""
    //                + "    }");
    //
    //        mockMvc.perform(post("/api/organizations/" + ORG_1.toString() + "/agreements")
    //                .with(authentication(adminAuth))
    //                .contentType(MediaType.APPLICATION_JSON)
    //                .content(content))
    //                .andExpect(status().isBadRequest());
    //    }

    @Test
    void doPostWithoutIdWithoutFirstName() throws Exception {
        String content = String.format("    {"
                + "        \"accepted\": \"true\","
                + "        \"first_name\": \"\","
                + "        \"last_name\": \"Smith\","
                + "        \"company\": \"Glob\""
                + "    }");

        mockMvc.perform(post("/api/organizations/agreements")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(content))
                .andExpect(status().isBadRequest());
    }

    //    @Test
    //    void doPostWithoutLastName() throws Exception {
    //        String content = String.format("    {"
    //                + "        \"accepted\": \"true\","
    //                + "        \"first_name\": \"John\","
    //                + "        \"last_name\": \"\","
    //                + "        \"company\": \"Glob\""
    //                + "    }");
    //
    //        mockMvc.perform(post("/api/organizations/" + ORG_1.toString() + "/agreements")
    //                .with(authentication(adminAuth))
    //                .contentType(MediaType.APPLICATION_JSON)
    //                .content(content))
    //                .andExpect(status().isBadRequest());
    //    }

    @Test
    void doPostWithoutIdWithoutLastName() throws Exception {
        String content = String.format("    {"
                + "        \"accepted\": \"true\","
                + "        \"first_name\": \"John\","
                + "        \"last_name\": \"\","
                + "        \"company\": \"Glob\""
                + "    }");

        mockMvc.perform(post("/api/organizations/agreements")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(content))
                .andExpect(status().isBadRequest());
    }

    //    @Test
    //    void doPostWithoutCompany() throws Exception {
    //        String content = String.format("    {"
    //                + "        \"accepted\": \"true\","
    //                + "        \"first_name\": \"John\","
    //                + "        \"last_name\": \"Smith\","
    //                + "        \"company\": \"\""
    //                + "    }");
    //
    //        mockMvc.perform(post("/api/organizations/" + ORG_1.toString() + "/agreements")
    //                .with(authentication(adminAuth))
    //                .contentType(MediaType.APPLICATION_JSON)
    //                .content(content))
    //                .andExpect(status().isBadRequest());
    //    }

    @Test
    void doPostWithoutIdWithoutCompany() throws Exception {
        String content = String.format("    {"
                + "        \"accepted\": \"true\","
                + "        \"first_name\": \"John\","
                + "        \"last_name\": \"Smith\","
                + "        \"company\": \"\""
                + "    }");

        mockMvc.perform(post("/api/organizations/agreements")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(content))
                .andExpect(status().isBadRequest());
    }
}
