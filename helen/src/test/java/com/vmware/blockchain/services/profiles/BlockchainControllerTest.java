/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

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
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.security.AuthHelper;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.SecurityTestUtils;
import com.vmware.blockchain.services.profiles.BlockchainController.BlockchainResponse;

/**
 * Tests for the blockchain controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(secure = false, controllers = BlockchainController.class)
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { BlockchainControllerTest.class, HelenExceptionHandler.class })
public class BlockchainControllerTest {
    static final UUID BC_ID = UUID.fromString("437d97b2-76df-4596-b0d8-3d8a9412ff2f");
    static final UUID BC2_ID = UUID.fromString("7324cb8f-0ffc-4311-b57e-4c3e1e10a3aa");
    static final UUID BC_NEW = UUID.fromString("4b8a5ec6-91ad-437d-b574-45f5b7345b96");
    static final UUID C2_ID = UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3");
    static final UUID C3_ID = UUID.fromString("a4b8f7ed-00b3-451e-97bc-4aa51a211288");

    // use consortium c2 in this.
    static final String POST_BODY = "{"
            + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
            + "    \"ip_list\": \"one,two,three\","
            + "    \"rpc_urls\": \"http:/one,http:/two,http:/three\","
            + "    \"rpc_certs\": \"certs\"" + "}";

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private ProfilesRegistryManager prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private ConcordProperties concordProperties;

    @MockBean
    private ConcordConnectionPool connectionPool;

    @MockBean
    private AgreementRepository agreementRepository;

    @MockBean
    private KeystoreRepository keystoreRepository;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    private BlockchainManager blockchainManager;

    @MockBean
    private ConsortiumRepository consortiumRepository;

    @MockBean
    AuthHelper authHelper;

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
        user = SecurityTestUtils.createMockUser();
        consortium = user.getConsortium();
        Consortium c2 = new Consortium();
        c2.setConsortiumId(C2_ID);
        Consortium c3 = new Consortium();
        c3.setConsortiumId(C3_ID);
        UUID c1Id = consortium.getConsortiumId();
        when(userRepository.findUserByEmail(user.getEmail())).thenReturn(Optional.of(user));
        when(consortiumRepository.findById(c1Id)).thenReturn(Optional.of(consortium));
        when(consortiumRepository.findById(C2_ID)).thenReturn(Optional.of(c2));
        when(consortiumRepository.findById(C3_ID)).thenReturn(Optional.of(c3));
        Blockchain b = new Blockchain(BC_ID, consortium, "1,2,3",
                "https://one.https://two,https://three", "");
        Blockchain b2 = new Blockchain(BC2_ID, c2, "4,5,6", "", "");
        when(blockchainManager.listByConsortium(consortium)).thenReturn(Collections.singletonList(b));
        when(blockchainManager.listByConsortium(c3)).thenReturn(Collections.emptyList());
        when(blockchainManager.list()).thenReturn(ImmutableList.of(b, b2));
        when(blockchainManager.get(BC_ID)).thenReturn(Optional.of(b));
        when(blockchainManager.get(BC2_ID)).thenReturn(Optional.of(b2));
        when(blockchainManager.create(any(Consortium.class), anyString(), anyString(), anyString())).thenAnswer(
            i -> new Blockchain(BC_NEW, i.getArgument(0), i.getArgument(1), i.getArgument(2), i.getArgument(3)));
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
        List<BlockchainController.BlockchainResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(2, res.size());
    }

    @Test
    void getBlockchainUserList() throws Exception {
        String id = consortium.getConsortiumId().toString();
        when(authHelper.getConsortiumId()).thenReturn(id);
        MvcResult result = mockMvc.perform(get("/api/blockchains/")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainController.BlockchainResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As a user in this consortium, we should only see one blockchain
        Assertions.assertEquals(1, res.size());
    }

    @Test
    void getBlockchainUser2List() throws Exception {
        when(authHelper.getConsortiumId()).thenReturn(C3_ID.toString());
        MvcResult result = mockMvc.perform(get("/api/blockchains/")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainController.BlockchainResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
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
    void postOperAccessNoCon() throws Exception {
        when(consortiumRepository.findById(any(UUID.class))).thenReturn(Optional.empty());
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        mockMvc.perform(post("/api/blockchains")
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_BODY).characterEncoding("utf-8"))
            .andExpect(status().isNotFound());
    }

    @Test
    void postOperAccess() throws Exception {
        when(authHelper.hasAnyAuthority(Roles.operatorRoles())).thenReturn(true);
        MvcResult result = mockMvc.perform(post("/api/blockchains")
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_BODY).characterEncoding("utf-8"))
            .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        BlockchainController.BlockchainResponse b = objectMapper.readValue(body, BlockchainResponse.class);
        Assertions.assertEquals(BC_NEW, b.getId());
        Assertions.assertEquals(C2_ID, b.getConsortiumId());
        Assertions.assertEquals("one,two,three", b.getIpList());
        Assertions.assertEquals("http:/one,http:/two,http:/three", b.getRpcUrls());
        Assertions.assertEquals("certs", b.getRpcCerts());
    }

}
