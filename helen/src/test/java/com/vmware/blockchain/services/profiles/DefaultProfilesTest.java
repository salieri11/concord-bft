/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

/**
 * Tests for the DefaultProfiles component.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:default-test.properties")
@ComponentScan(basePackageClasses = {DefaultProfilesTest.class})
class DefaultProfilesTest {
    // Just some random UUIDs
    private static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    private static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    private static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");

    @MockBean
    ConsortiumRepository consortiumRepository;

    @MockBean
    OrganizationRepository organizationRepository;

    @MockBean
    UserRepository userRepository;

    @MockBean
    PasswordEncoder passwordEncoder;

    @MockBean
    BlockchainManager blockchainManager;

    DefaultProfiles profiles;

    @Value("${ConcordAuthorities}")
    private String ipList;
    @Value("${ConcordRpcUrls}")
    private String rpcUrls;
    private Consortium consortium;
    private Organization organization;
    private User testUser;
    private Blockchain blockchain;
    private UUID uuid = UUID.fromString("c6b0f366-0884-4ab0-bedf-5fc1ed3c0d56");

    /**
     * initialize the various mocks.
     */
    @BeforeEach
    void init() {
        consortium = new Consortium();
        consortium.setConsortiumId(CONSORTIUM_ID);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");

        organization = new Organization();
        organization.setOrganizationId(ORG_ID);
        organization.setOrganizationName("Test Org");

        blockchain = new Blockchain();
        blockchain.setId(uuid);
        blockchain.setConsortium(consortium);
        blockchain.setIpList("1,2,3,4 ");
        blockchain.setRpcUrls("a=1,b=2,c=3,d=4");

        // our test user
        testUser = new User();
        testUser.setUserId(USER_ID);
        testUser.setEmail("user@test.com");
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword("1234");
        testUser.setConsortium(consortium);
        testUser.setOrganization(organization);
        testUser.setRole(Roles.SYSTEM_ADMIN);

        profiles = new DefaultProfiles(userRepository, organizationRepository, consortiumRepository, passwordEncoder,
                blockchainManager, ipList, rpcUrls);

        when(passwordEncoder.encode(anyString())).then(a -> a.getArguments().toString());
    }

    private void initProfilesExist() {
        when(consortiumRepository.findAll()).thenReturn(Arrays.asList(consortium));
        when(organizationRepository.findAll()).thenReturn(Arrays.asList(organization));
        when(blockchainManager.list()).thenReturn(Arrays.asList(blockchain));
        when(userRepository.findAll()).thenReturn(Arrays.asList(testUser));
        profiles.initialize();
    }

    private void initProfilesEmpty() {
        when(consortiumRepository.findAll()).thenReturn(Collections.emptyList());
        when(consortiumRepository.save(any(Consortium.class))).then(in -> {
            Consortium c = in.getArgument(0);
            c.setConsortiumId(CONSORTIUM_ID);
            return c;
        });
        when(organizationRepository.findAll()).thenReturn(Collections.emptyList());
        when(organizationRepository.save(any(Organization.class))).then(in -> {
            Organization o = in.getArgument(0);
            o.setOrganizationId(ORG_ID);
            return o;
        });
        when(blockchainManager.list()).thenReturn(Collections.emptyList());
        when(blockchainManager.create(any(Consortium.class), anyString(), anyString())).then(in -> {
            Blockchain b = new Blockchain();
            b.setId(uuid);
            b.setIpList(ipList);
            b.setRpcUrls(rpcUrls);
            return b;
        });
        when(userRepository.findAll()).thenReturn(Collections.emptyList());
        when(userRepository.save(any(User.class))).thenAnswer(in -> {
            User u = in.getArgument(0);
            u.setUserId(USER_ID);
            return u;
        });
        profiles.initialize();
    }

    @Test
    void testProfileExisting() {
        initProfilesExist();
        Map<String, String> ipMap = new ImmutableMap.Builder<String, String>()
                .put("a", "1")
                .put("b", "2")
                .put("c", "3")
                .put("d", "4").build();
        List<String> ipList = new ImmutableList.Builder<String>().add("1", "2", "3", "4").build();
        Assertions.assertEquals(organization, profiles.getOrganization());
        Assertions.assertEquals("Test Org", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium, profiles.getConsortium());
        Assertions.assertEquals("Consortium Test", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain, profiles.getBlockchain());
        Assertions.assertEquals(ipList, profiles.getBlockchain().getIpAsList());
        Assertions.assertEquals(ipMap, profiles.getBlockchain().getUrlsAsMap());
        Assertions.assertEquals(testUser, profiles.getUser());
        Assertions.assertEquals("user@test.com", profiles.getUser().getEmail());
    }

    @Test
    void testProfileEmpty() {
        // This will use the system generated profiles, rather than the ones we created.
        initProfilesEmpty();
        Map<String, String> ipMap = new ImmutableMap.Builder<String, String>()
                .put("replica0", "localhost:5458")
                .put("replica1", "localhost:5459")
                .put("replica2", "localhost:5460")
                .put("replica3", "localhost:5461").build();
        Assertions.assertEquals(organization, profiles.getOrganization());
        Assertions.assertEquals("ADMIN", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium, profiles.getConsortium());
        Assertions.assertEquals("ADMIN", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain, profiles.getBlockchain());
        Assertions.assertEquals(ipMap, profiles.getBlockchain().getUrlsAsMap());
        Assertions.assertEquals("localhost:5458,localhost:5459,localhost:5460,localhost:5461",
                profiles.getBlockchain().getIpList());
        Assertions.assertEquals(testUser, profiles.getUser());
        Assertions.assertEquals("admin@blockchain.local", profiles.getUser().getEmail());
    }

}
