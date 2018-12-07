/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Tests for the DefaultProfiles component.
 */
@RunWith(SpringRunner.class)
@TestPropertySource(locations = "classpath:test.properties")
@ComponentScan(basePackageClasses = {DefaultProfilesTest.class})
public class DefaultProfilesTest {
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
    @Before
    public void init() {
        consortium = new Consortium();
        consortium.setConsortiumId(200L);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");

        organization = new Organization();
        organization.setOrganizationId(300L);
        organization.setOrganizationName("Test Org");

        blockchain = new Blockchain();
        blockchain.setId(uuid);
        blockchain.setConsortium(consortium);
        blockchain.setIpList("1,2,3,4");

        // our test user
        testUser = new User();
        testUser.setUserId(20L);
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
            c.setConsortiumId(200L);
            return c;
        });
        when(organizationRepository.findAll()).thenReturn(Collections.emptyList());
        when(organizationRepository.save(any(Organization.class))).then(in -> {
            Organization o = in.getArgument(0);
            o.setOrganizationId(300L);
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
            u.setUserId(20L);
            return u;
        });
        profiles.initialize();
    }

    @Test
    public void testProfileExisting() {
        initProfilesExist();
        Assert.assertEquals(organization, profiles.getOrganization());
        Assert.assertEquals("Test Org", profiles.getOrganization().getOrganizationName());
        Assert.assertEquals(consortium, profiles.getConsortium());
        Assert.assertEquals("Consortium Test", profiles.getConsortium().getConsortiumName());
        Assert.assertEquals(blockchain, profiles.getBlockchain());
        Assert.assertEquals("1,2,3,4", profiles.getBlockchain().getIpList());
        Assert.assertEquals(testUser, profiles.getUser());
        Assert.assertEquals("user@test.com", profiles.getUser().getEmail());
    }

    @Test
    public void testProfileEmpty() {
        // This will use the system generated profiles, rather than the ones we created.
        initProfilesEmpty();
        Assert.assertEquals(organization, profiles.getOrganization());
        Assert.assertEquals("ADMIN", profiles.getOrganization().getOrganizationName());
        Assert.assertEquals(consortium, profiles.getConsortium());
        Assert.assertEquals("ADMIN", profiles.getConsortium().getConsortiumName());
        Assert.assertEquals(blockchain, profiles.getBlockchain());
        Assert.assertEquals(ipList, profiles.getBlockchain().getIpList());
        Assert.assertEquals(testUser, profiles.getUser());
        Assert.assertEquals("admin@blockchain.local", profiles.getUser().getEmail());
    }

}
