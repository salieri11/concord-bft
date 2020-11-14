/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.tasks.TaskService;

/**
 * Test Organization Controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = {OrganizationContoller.class})
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = {ProfileController.class, HelenExceptionHandler.class})
@TestPropertySource(properties = "spring.main.allow-bean-definition-overriding=true")
public class ProfileControllerTest {
    static final UUID USER_1 = UUID.fromString("fdd53f14-acaa-4c55-a41c-cad76154cd1f");
    static final UUID USER_2 = UUID.fromString("ec50d405-f8b6-4e27-9394-3e73af2e0805");
    static final UUID ORG_1 = UUID.fromString("4a87e52b-967d-48af-bd7d-f77fcc21e29f");
    static final UUID CONSORTIUM = UUID.fromString("13c0829e-3105-4163-b961-4f575f43a0f4");

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    DefaultProfiles defaultProfiles;

    @MockBean
    UserService userService;

    @MockBean
    ProfilesService profilesService;

    @MockBean(name = "genericDao")
    GenericDao genericDao;

    @MockBean(name = "serviceContext")
    ServiceContext serviceContext;

    @MockBean(name = "taskService")
    TaskService taskService;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;


    VmbcRoles vmbcRoles = new VmbcRoles();


    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;

    @BeforeEach
    void init() throws Exception {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();

        User u1 = new User();
        u1.setEmail("user1@test");
        u1.setName("user1@test");
        u1.setId(USER_1);

        User u2 = new User();
        u2.setEmail("user2@test");
        u2.setName("user2@test");
        u2.setId(USER_2);

        when(userService.list()).thenReturn(List.of(u1, u2));
        when(profilesService.getUserWithId(USER_1.toString())).thenReturn(u1);

        adminAuth = createContext("operator", ORG_1,
                ImmutableList.of(vmbcRoles.SYSTEM_ADMIN, vmbcRoles.ORG_USER),
                ImmutableList.of(CONSORTIUM),
                Collections.emptyList(), "");

        objectMapper = jacksonBuilder.build();
    }

    @Test
    void listUsers() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/users")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<UsersGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(2, res.size());
    }


}