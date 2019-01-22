/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
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

/**
 * Tests for the DefaultProfiles component.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:test.properties")
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
    BlockchainRepository blockchainRepository;

    DefaultProfiles profiles;

    @Value("${ConcordAuthorities}")
    private String ipList;
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
        blockchain.setIpList("1,2,3,4");

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
                blockchainRepository, ipList);

        when(passwordEncoder.encode(anyString())).then(a -> a.getArguments().toString());
    }

    private void initProfilesExist() {
        when(consortiumRepository.findAll()).thenReturn(Arrays.asList(consortium));
        when(organizationRepository.findAll()).thenReturn(Arrays.asList(organization));
        when(blockchainRepository.findAll()).thenReturn(Arrays.asList(blockchain));
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
        when(blockchainRepository.findAll()).thenReturn(Collections.emptyList());
        when(blockchainRepository.save(any(Blockchain.class))).then(in -> {
            Blockchain b = in.getArgument(0);
            b.setId(uuid);
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
        Assertions.assertEquals(organization, profiles.getOrganization());
        Assertions.assertEquals("Test Org", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium, profiles.getConsortium());
        Assertions.assertEquals("Consortium Test", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain, profiles.getBlockchain());
        Assertions.assertEquals("1,2,3,4", profiles.getBlockchain().getIpList());
        Assertions.assertEquals(testUser, profiles.getUser());
        Assertions.assertEquals("user@test.com", profiles.getUser().getEmail());
    }

    @Test
    void testProfileEmpty() {
        // This will use the system generated profiles, rather than the ones we created.
        initProfilesEmpty();
        Assertions.assertEquals(organization, profiles.getOrganization());
        Assertions.assertEquals("ADMIN", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium, profiles.getConsortium());
        Assertions.assertEquals("ADMIN", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain, profiles.getBlockchain());
        Assertions.assertEquals(ipList, profiles.getBlockchain().getIpList());
        Assertions.assertEquals(testUser, profiles.getUser());
        Assertions.assertEquals("admin@blockchain.local", profiles.getUser().getEmail());
    }

}
