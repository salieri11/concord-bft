/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import com.vmware.blockchain.common.ConflictException;
import com.vmware.blockchain.common.ForbiddenException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.utils.ControllerTestConfig;

/**
 * Tests for the ContractsController.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(secure = false, controllers = {ContractsController.class})
@ContextConfiguration(classes = {ControllerTestConfig.class, MvcConfig.class })
@ComponentScan(basePackageClasses = {ContractsControllerTest.class, HelenExceptionHandler.class })
@TestPropertySource(properties = "spring.main.allow-bean-definition-overriding=true")
public class ContractsControllerTest {

    static final UUID BC_ID = UUID.fromString("81024ec4-83fe-4893-a870-311ad4716743");
    static final UUID DEFAULT_BC = UUID.fromString("ff30a99d-a2eb-4180-9fba-2001c3f9dd83");

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    DefaultProfiles defaultProfiles;

    @Autowired
    AuthHelper authHelper;

    @Autowired
    ContractService contractService;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    ObjectMapper objectMapper;
    private Object when;

    @BeforeEach
    void setUp() {
        objectMapper = jacksonBuilder.build();
        when(authHelper.hasAnyAuthority(Roles.systemAdmin())).thenReturn(true);
        // need this for a default method.
        Blockchain bc = new Blockchain();
        bc.setId(DEFAULT_BC);
        when(defaultProfiles.getBlockchain()).thenReturn(bc);


        Contract c1 = Contract.builder()
                .name("My Test Contract")
                .versionName("1.0")
                .owner("I. R. Owner")
                .metadata("{ \"key\": \"value\" }")
                .blockchainId(BC_ID)
                .build();
        Contract c2 = Contract.builder()
                .name("My Test Contract")
                .versionName("2.0")
                .owner("I. R. Owner")
                .metadata("{ \"key\": \"value\" }")
                .blockchainId(BC_ID)
                .build();
        Contract c3 = Contract.builder()
                .name("My Test Contract")
                .versionName("2.0")
                .owner("P. Kachu")
                .metadata("{ \"key\": \"value\" }")
                .blockchainId(BC_ID)
                .build();
        List<Contract> cList = ImmutableList.of(c1, c2);
        List<Contract> cList2 = ImmutableList.of(c3);
        when(contractService.list(BC_ID)).thenReturn(cList);
        when(contractService.listByName("My Test Contract", BC_ID)).thenReturn(cList);
        when(contractService.listByName("ID", BC_ID)).thenReturn(cList);
        Compiler.Result result = new Compiler.Result();
        result.setSuccess(true);
    }

    @Test
    void testGet() throws  Exception {
        String url = String.format("/api/blockchains/%s/concord/contracts", BC_ID.toString());
        var r = mockMvc.perform(get(url)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        List<BriefContractInfo> infoList = objectMapper.readValue(r.getResponse().getContentAsString(),
                                                                  new TypeReference<List<BriefContractInfo>>() {});
        Assertions.assertEquals(2, infoList.size());
        BriefContractInfo info = infoList.get(0);
        Assertions.assertEquals("My Test Contract", info.getContractId());
        Assertions.assertEquals("I. R. Owner", info.getOwner());
    }

    @Test
    void testGetNo() throws  Exception {
        String url = String.format("/api/blockchains/%s/concord/contracts", BC_ID.toString());
        var r = mockMvc.perform(get(url)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        List<BriefContractInfo> infoList = objectMapper.readValue(r.getResponse().getContentAsString(),
                new TypeReference<List<BriefContractInfo>>() {});
        Assertions.assertEquals(2, infoList.size());
        BriefContractInfo info = infoList.get(0);
        Assertions.assertEquals("My Test Contract", info.getContractId());
        Assertions.assertEquals("I. R. Owner", info.getOwner());
    }

    @Test
    void testGetByName() throws  Exception {
        String url = String.format("/api/blockchains/%s/concord/contracts/%s", BC_ID.toString(),
                                   "My Test Contract");
        var r = mockMvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        BriefContractInfo info  = objectMapper.readValue(r.getResponse().getContentAsString(), BriefContractInfo.class);
        Assertions.assertEquals("My Test Contract", info.getContractId());
        Assertions.assertEquals("I. R. Owner", info.getOwner());
        Assertions.assertEquals(2, info.getVersions().size());
        BriefVersionInfo bv = info.getVersions().get(0);
        Assertions.assertEquals("1.0", bv.getVersion());
        Assertions.assertEquals("value", ((Map) bv.getMetadata()).get("key"));
    }

    @Test
    void testPostBadParam() throws  Exception {
        String postBody = "{\n"
                          + "  \"version\": \"1.0\",\n"
                          + "  \"sourcecode\": \"source\",\n"
                          + "  \"contract_name\": \"what's in a name\",\n"
                          + "  \"compiler_version\": \"1.0\"\n"
                          + "}\n";

        String url = String.format("/api/blockchains/%s/concord/contracts", BC_ID.toString());
        var r = mockMvc.perform(post(url).contentType(MediaType.APPLICATION_JSON).content(postBody))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testPostSameVersion() throws  Exception {
        String postBody = "{\n"
                + "  \"from\": \"P. Kachu\",\n"
                + "  \"contract_id\": \"ID\",\n"
                + "  \"version\": \"1.0\",\n"
                + "  \"sourcecode\": \"//source\",\n"
                + "  \"contract_name\": \"My Test Contract\",\n"
                + "  \"compiler_version\": \"1.0\"\n"
                + "}\n";

        String url = String.format("/api/blockchains/%s/concord/contracts", BC_ID.toString());
        MvcResult mvcResult = mockMvc.perform(post(url).contentType(MediaType.APPLICATION_JSON).content(postBody))
                .andExpect(status().is4xxClientError())
                .andReturn();

        Optional<ConflictException> someException = Optional.ofNullable((ConflictException)
                mvcResult.getResolvedException());

        Assertions.assertEquals(409, someException.get().getHttpStatus().value());
    }

    @Test
    void testPostDifferentOwners() throws  Exception {
        String postBody = "{\n"
                + "  \"from\": \"P. Plup\",\n"
                + "  \"contract_id\": \"ID\",\n"
                + "  \"version\": \"3.0\",\n"
                + "  \"sourcecode\": \"//source\",\n"
                + "  \"contract_name\": \"My Test Contract\",\n"
                + "  \"compiler_version\": \"1.0\"\n"
                + "}\n";

        String url = String.format("/api/blockchains/%s/concord/contracts", BC_ID.toString());
        MvcResult mvcResult = mockMvc.perform(post(url).contentType(MediaType.APPLICATION_JSON).content(postBody))
                .andExpect(status().isForbidden())
                .andReturn();

        Optional<ForbiddenException> someException = Optional.ofNullable((ForbiddenException)
                mvcResult.getResolvedException());

        Assertions.assertEquals(403, someException.get().getHttpStatus().value());
    }
}