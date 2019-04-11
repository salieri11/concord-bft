/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.security.AuthHelper;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.SecurityTestUtils;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.BlockchainController.BlockchainTaskResponse;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.KeystoreService;
import com.vmware.blockchain.services.profiles.ProfilesService;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskController;
import com.vmware.blockchain.services.tasks.TaskService;

/**
 * Tests for the blockchain controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(secure = false, controllers = { BlockchainController.class, TaskController.class })
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { BlockchainControllerTest.class, HelenExceptionHandler.class,
        TaskController.class })
public class BlockchainControllerTest {
    static final UUID BC_ID = UUID.fromString("437d97b2-76df-4596-b0d8-3d8a9412ff2f");
    static final UUID BC2_ID = UUID.fromString("7324cb8f-0ffc-4311-b57e-4c3e1e10a3aa");
    static final UUID BC_NEW = UUID.fromString("4b8a5ec6-91ad-437d-b574-45f5b7345b96");
    static final UUID C2_ID = UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3");
    static final UUID C3_ID = UUID.fromString("a4b8f7ed-00b3-451e-97bc-4aa51a211288");
    static final UUID TASK_ID = UUID.fromString("c23ed97d-f29c-472e-9f63-cc6be883a5f5");

    // use consortium c2 in this.
    static final String POST_BODY = "{"
            + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
            + "    \"f_count\": 1,"
            + "    \"c_count\": 0,"
            + "    \"deployment_type\": \"UNSPECIFIED\"" + "}";

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserService userService;

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

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private String token;
    private User user;
    private Consortium consortium;
    private ObjectMapper objectMapper;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        user = SecurityTestUtils.getUser();
        consortium = SecurityTestUtils.getConsortium();
        Consortium c2 = new Consortium();
        c2.setId(C2_ID);
        Consortium c3 = new Consortium();
        c3.setId(C3_ID);
        UUID c1Id = consortium.getId();
        when(userService.getByEmail(user.getEmail())).thenReturn(user);
        when(consortiumService.get(c1Id)).thenReturn(consortium);
        when(consortiumService.get(C2_ID)).thenReturn(c2);
        when(consortiumService.get(C3_ID)).thenReturn(c3);

        final Blockchain b = Blockchain.builder()
                .consortium(consortium.getId())
                .nodeList(Stream.of("1", "2", "3")
                        .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "", "", ""))
                        .collect(Collectors.toList()))
                .build();
        final Blockchain b2 = Blockchain.builder()
                .consortium(c2.getId())
                .nodeList(Stream.of("4", "5", "6")
                        .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "", "", ""))
                        .collect(Collectors.toList()))
                .build();
        final Blockchain bn = Blockchain.builder()
                .consortium(UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3"))
                .nodeList(Stream.of("one", "two", "three")
                        .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, "http://".concat(s), "cert-".concat(s), ""))
                        .collect(Collectors.toList()))
                .build();
        bn.setId(BC_NEW);
        when(blockchainService.listByConsortium(consortium)).thenReturn(Collections.singletonList(b));
        when(blockchainService.listByConsortium(c3)).thenReturn(Collections.emptyList());
        when(blockchainService.list()).thenReturn(ImmutableList.of(b, b2));
        when(blockchainService.get(BC_ID)).thenReturn(b);
        when(blockchainService.get(BC2_ID)).thenReturn(b2);
        when(blockchainService.get(BC_NEW)).thenReturn(bn);
        when(blockchainService.get(C2_ID)).thenThrow(new NotFoundException("Not found"));
        when(defaultProfiles.getBlockchain()).thenReturn(bn);
        Task t = new Task();
        t.setId(TASK_ID);
        t.setState(State.SUCCEEDED);
        t.setMessage("Done");
        t.setResourceId(BC_NEW);
        when(taskService.put(any())).thenReturn(t);
        when(taskService.get(TASK_ID)).thenReturn(t);
        token = "token";
        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

    }

    @Test
    void getBlockchainOperatorList() throws Exception {
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        MvcResult result = mockMvc.perform(get("/api/blockchains/")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainController.BlockchainGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(2, res.size());
    }

    @Test
    void getBlockchainUserList() throws Exception {
        UUID cid = consortium.getId();
        when(authHelper.getConsortiumId()).thenReturn(cid);
        MvcResult result = mockMvc.perform(get("/api/blockchains/")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainController.BlockchainGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As a user in this consortium, we should only see one blockchain
        Assertions.assertEquals(1, res.size());
    }

    @Test
    void getBlockchainUser2List() throws Exception {
        when(authHelper.getConsortiumId()).thenReturn(C3_ID);
        MvcResult result = mockMvc.perform(get("/api/blockchains/")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainController.BlockchainGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As a user in this consortium, we should only see one blockchain
        Assertions.assertEquals(0, res.size());
    }

    @Test
    void getBlockchainOperator() throws Exception {
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        mockMvc.perform(get("/api/blockchains/" + BC_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void getBlockchainOperatorNotFound() throws Exception {
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        // There is no such blockchain.
        mockMvc.perform(get("/api/blockchains/" + C2_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void getBlockchainUser() throws Exception {
        when(authHelper.getPermittedChains()).thenReturn(Collections.singletonList(BC_ID));
        mockMvc.perform(get("/api/blockchains/" + BC_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void getBlockchainOperatorBc2() throws Exception {
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        mockMvc.perform(get("/api/blockchains/" + BC2_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void getBlockchainUserBc2() throws Exception {
        when(authHelper.getPermittedChains()).thenReturn(Collections.singletonList(BC_ID));
        mockMvc.perform(get("/api/blockchains/" + BC2_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isForbidden());
    }

    @Test
    void getBlockchainNoAccess() throws Exception {
        when(authHelper.getPermittedChains()).thenReturn(Collections.emptyList());
        mockMvc.perform(get("/api/blockchains/" + BC_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isForbidden());
    }

    @Test
    void postUserAccess() throws Exception {
        mockMvc.perform(post("/api/blockchains")
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_BODY)
                .characterEncoding("utf-8"))
            .andExpect(status().isForbidden());
    }

    @Test
    void postOperAccess() throws Exception {
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        MvcResult result = mockMvc.perform(post("/api/blockchains")
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_BODY).characterEncoding("utf-8"))
            .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();

        BlockchainController.BlockchainTaskResponse t = objectMapper.readValue(body, BlockchainTaskResponse.class);
        Assertions.assertEquals(TASK_ID, t.getTaskId());

        result = mockMvc.perform(get("/api/tasks/" + TASK_ID.toString())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();

        body = result.getResponse().getContentAsString();
        Map<String, String> r = objectMapper.readValue(body, new TypeReference<Map<String, String>>() {});
        Assertions.assertEquals(TASK_ID.toString(), r.get("task_id"));
        Assertions.assertEquals("SUCCEEDED", r.get("state"));
        Assertions.assertEquals("Done", r.get("message"));
        Assertions.assertEquals(BC_NEW.toString(), r.get("resource_id"));

    }

}
