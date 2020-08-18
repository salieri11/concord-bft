/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.nodesizing;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * Tests for the NodeSizeTemplateController.
 */
@ExtendWith({SpringExtension.class})
@WebMvcTest(controllers = {NodeSizeTemplateController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = {NodeSizeTemplateController.class, HelenExceptionHandler.class})
class NodeSizeTemplateControllerTest {
    private static final UUID DEFAULT_TEMPLATE_ID = UUID.fromString("dab730c9-c82d-436a-8b71-38076f061093");
    private static final UUID ORG_ID = UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902");
    private static final String DEFAULT_TEMPLATE_NAME = "Default Template";

    @Autowired
    WebApplicationContext context;

    private MockMvc mockMvc;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private AuthenticationContext userAuth;
    private AuthenticationContext adminAuth;

    private ObjectMapper objectMapper;

    @MockBean
    NodeSizeTemplateService nodeSizeTemplateService;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();


        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER),
                                 Collections.emptyList(),
                                 Collections.emptyList(), "");

        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(VmbcRoles.INFRA_ADMIN, VmbcRoles.SYSTEM_ADMIN,
                                                   VmbcRoles.CONSORTIUM_ADMIN),
                                  Collections.emptyList(),
                                  Collections.emptyList(), "");

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        NodeSizeTemplate nst = NodeSizeTemplateUtil.createNodeSizeTemplate(DEFAULT_TEMPLATE_ID, DEFAULT_TEMPLATE_NAME);

        when(nodeSizeTemplateService.getTemplate()).thenReturn(nst);
    }

    @Test
    void testGet() throws Exception {
        MvcResult result =  mockMvc.perform(get("/api/blockchains/nodesizetemplate")
                                                    .with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        NodeSizeTemplateController.NodeSizeTemplateResponse nst =
                objectMapper.readValue(body, NodeSizeTemplateController.NodeSizeTemplateResponse.class);
        Assertions.assertEquals(DEFAULT_TEMPLATE_NAME, nst.getName());
        Assertions.assertNotNull(nst.getTemplates());
        Assertions.assertNotNull(nst.getRange());
        Assertions.assertEquals(3, nst.getRange().size());
        Assertions.assertEquals(3, nst.getTemplates().size());
    }

    @Test
    void testGetTemplateBadAction() throws Exception {
        mockMvc.perform(get("/api/blockchains/nodesizetemplate").with(authentication(userAuth)))
                .andExpect(status().isForbidden());
    }

}
