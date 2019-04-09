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
import com.vmware.blockchain.security.ServiceContext;

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
    ConsortiumService consortiumService;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    UserService userService;

    @MockBean
    PasswordEncoder passwordEncoder;

    @MockBean
    BlockchainService blockchainService;

    @MockBean
    AgreementService agreementService;

    @MockBean
    ServiceContext serviceContext;

    DefaultProfiles profiles;

    @Value("${ConcordAuthorities}")
    private String ipList;
    @Value("${ConcordRpcUrls}")
    private String rpcUrls;
    @Value("${ConcordRpcCerts}")
    private String rpcCerts;
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
        consortium.setId(CONSORTIUM_ID);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");

        organization = new Organization();
        organization.setId(ORG_ID);
        organization.setOrganizationName("Test Org");

        blockchain = new Blockchain.BlockchainBuilder()
                .consortium(consortium.getId())
                .ipList("1,2,3,4 ")
                .rpcUrls("a=1,b=2,c=3,d=4")
                .rpcCerts("a=c1,b=c2,c=c3,d=c4").build();
        blockchain.setId(uuid);

        // our test user
        testUser = new User();
        testUser.setId(USER_ID);
        testUser.setEmail("user@test.com");
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword("1234");
        testUser.setRoles(Collections.singletonList(Roles.SYSTEM_ADMIN));

        profiles = new DefaultProfiles(userService, organizationService, consortiumService, passwordEncoder,
                                       blockchainService, agreementService, serviceContext, ipList, rpcUrls, rpcCerts);

        when(passwordEncoder.encode(anyString())).then(a -> a.getArguments().toString());
    }

    private void initProfilesExist() {
        when(consortiumService.list()).thenReturn(Arrays.asList(consortium));
        when(organizationService.list()).thenReturn(Arrays.asList(organization));
        when(blockchainService.list()).thenReturn(Arrays.asList(blockchain));
        when(userService.list()).thenReturn(Arrays.asList(testUser));
        profiles.initialize();
    }

    private void initProfilesEmpty() {
        when(consortiumService.list()).thenReturn(Collections.emptyList());
        when(consortiumService.put(any(Consortium.class))).then(in -> {
            Consortium c = in.getArgument(0);
            c.setId(CONSORTIUM_ID);
            return c;
        });
        when(organizationService.list()).thenReturn(Collections.emptyList());
        when(organizationService.put(any(Organization.class))).then(in -> {
            Organization o = in.getArgument(0);
            o.setId(ORG_ID);
            return o;
        });
        when(blockchainService.list()).thenReturn(Collections.emptyList());
        when(blockchainService.create(any(Consortium.class), anyString(), anyString(), anyString())).then(in -> {
            Blockchain b = new Blockchain.BlockchainBuilder()
                .consortium(((Consortium) in.getArgument(0)).getId())
                .ipList(in.getArgument(1))
                .rpcUrls(in.getArgument(2))
                .rpcCerts(in.getArgument(3)).build();
            b.setId(uuid);
            return b;
        });
        when(userService.list()).thenReturn(Collections.emptyList());
        when(userService.put(any(User.class))).thenAnswer(in -> {
            User u = in.getArgument(0);
            u.setId(USER_ID);
            return u;
        });
        profiles.initialize();
    }

    @Test
    void testProfileExisting() {
        initProfilesExist();
        Map<String, String> urlMap = new ImmutableMap.Builder<String, String>()
                .put("a", "1")
                .put("b", "2")
                .put("c", "3")
                .put("d", "4").build();
        Map<String, String> certMap = new ImmutableMap.Builder<String, String>()
                .put("a", "c1")
                .put("b", "c2")
                .put("c", "c3")
                .put("d", "c4").build();
        List<String> ipList = new ImmutableList.Builder<String>().add("1", "2", "3", "4").build();
        Assertions.assertEquals(organization.getId(), profiles.getOrganization().getId());
        Assertions.assertEquals("Test Org", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium.getId(), profiles.getConsortium().getId());
        Assertions.assertEquals("Consortium Test", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain.getId(), profiles.getBlockchain().getId());
        Assertions.assertEquals(ipList, profiles.getBlockchain().getIpAsList());
        Assertions.assertEquals(urlMap, profiles.getBlockchain().getUrlsAsMap());
        Assertions.assertEquals(certMap, profiles.getBlockchain().getCertsAsMap());
        Assertions.assertEquals(testUser.getId(), profiles.getUser().getId());
        Assertions.assertEquals("user@test.com", profiles.getUser().getEmail());
    }

    @Test
    void testProfileEmpty() {
        // This will use the system generated profiles, rather than the ones we created.
        initProfilesEmpty();
        Map<String, String> urlMap = new ImmutableMap.Builder<String, String>()
                .put("replica0", "https://127.0.0.1:8545/")
                .put("replica1", "https://127.0.0.1:8546/")
                .put("replica2", "https://127.0.0.1:8547/")
                .put("replica3", "https://127.0.0.1:8548/").build();
        Map<String, String> certSet = new ImmutableMap.Builder<String, String>()
                .put("replica0", "/config/replica0-cacert.pem").build();
        Assertions.assertEquals(organization.getId(), profiles.getOrganization().getId());
        Assertions.assertEquals("ADMIN", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium.getId(), profiles.getConsortium().getId());
        Assertions.assertEquals("ADMIN", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain.getId(), profiles.getBlockchain().getId());
        Assertions.assertEquals(urlMap, profiles.getBlockchain().getUrlsAsMap());
        Assertions.assertEquals(certSet, profiles.getBlockchain().getCertsAsMap());
        Assertions.assertEquals("localhost:5458,localhost:5459,localhost:5460,localhost:5461",
                profiles.getBlockchain().getIpList());
        Assertions.assertEquals(testUser.getId(), profiles.getUser().getId());
        Assertions.assertEquals("admin@blockchain.local", profiles.getUser().getEmail());
    }

}
