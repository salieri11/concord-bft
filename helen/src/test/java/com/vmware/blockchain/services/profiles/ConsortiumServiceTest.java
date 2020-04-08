/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.dao.TestDaoConfig;
import com.vmware.blockchain.db.DbConfig;
import com.vmware.blockchain.db.TestMapper;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.ManagedChannel;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;

/**
 * Testing database access to consortium entities.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@ComponentScan(basePackageClasses = {GenericDao.class, ConsortiumService.class, OrganizationService.class})
@EnableAutoConfiguration
@AutoConfigureEmbeddedDatabase
@ContextConfiguration(classes = {MvcConfig.class, DbConfig.class, TestDaoConfig.class })
public class ConsortiumServiceTest {
    @MockBean
    private ProfilesService prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private ConcordConnectionPool connectionPool;

    @MockBean
    private KeystoreService keystoreService;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    private BlockchainService blockchainService;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    TaskService taskService;

    @MockBean
    @Qualifier("provisioningServerChannel")
    ManagedChannel channel;

    @MockBean
    EthDispatcher ethDispatcher;

    @MockBean
    ConnectionPoolManager connectionPoolManager;

    @MockBean
    BaseCacheHelper baseCacheHelper;

    @MockBean
    CacheManager cacheManager;

    @Autowired
    OrganizationService organizationService;

    @Autowired
    ConsortiumService consortiumService;

    @Autowired
    private TestMapper testMapper;


    private static UUID ORG_ID = UUID.fromString("b07c32a2-8630-4a32-b346-63e1b43aa6c4");
    private static UUID ORG2_ID = UUID.fromString("9f7f7c14-79c8-4d72-90df-498077d31247");
    private static UUID USER_ID = UUID.fromString("3d312c6c-2483-4e34-a4c2-c9267854e939");
    private static String EMAIL = "user@test.com";

    @BeforeEach
    void init() {
        when(authHelper.hasAnyAuthority(anyString())).thenReturn(true);
        when(authHelper.getOrganizationId()).thenReturn(ORG_ID);
        when(authHelper.getUserId()).thenReturn(USER_ID);
        when(authHelper.getEmail()).thenReturn(EMAIL);
    }

    @AfterEach
    void cleanup() {
        // Remove any entities we created
        testMapper.deleteEntity();
        testMapper.deleteEntityHistory();
        testMapper.deleteLink();
    }

    @Test
    void basicTest() throws Exception {
        Organization o = new Organization("test name");
        o.setId(ORG_ID);
        o = organizationService.put(o);
        Consortium c = new Consortium("Consort", "test", o.getId());
        c = consortiumService.put(c);
        List<Consortium> allc = organizationService.getConsortiums(ORG_ID);
        List<Organization> allo = consortiumService.getOrganizations(c.getId());
        Assertions.assertEquals(1, allc.size());
        Assertions.assertEquals(1, allo.size());
    }

    @Test
    void removeOwner() throws Exception {
        Organization o = organizationService.put(new Organization("test name"));
        Consortium c = consortiumService.put(new Consortium("Consort", "test", o.getId()));
        Assertions.assertThrows(IllegalArgumentException.class,
            () -> consortiumService.removeOrganization(c, o.getId()));
        // make sure nothing change
        List<Consortium> allc = organizationService.getConsortiums(o.getId());
        List<Organization> allo = consortiumService.getOrganizations(c.getId());
        Assertions.assertEquals(1, allc.size());
        Assertions.assertEquals(1, allo.size());
    }

    @Test
    void addRemoveOrg() throws Exception {
        Organization o = new Organization("test name");
        o.setId(ORG_ID);
        o = organizationService.put(o);
        Organization o2 = new Organization("test 2 name");
        o2.setId(ORG2_ID);
        o2 = organizationService.put(o2);
        Consortium c = new Consortium("Consort", "test", o.getId());
        c = consortiumService.put(c);
        consortiumService.addOrganization(c, o2.getId());
        // let's make sure the add worked
        List<Consortium> allc = organizationService.getConsortiums(ORG_ID);
        List<Consortium> allc2 = organizationService.getConsortiums(ORG2_ID);
        List<Organization> allo = consortiumService.getOrganizations(c.getId());
        Assertions.assertEquals(1, allc2.size());
        Assertions.assertEquals(1, allc.size());
        Assertions.assertEquals(2, allo.size());
        // Now remove org 2
        consortiumService.removeOrganization(c, ORG2_ID);
        allc = organizationService.getConsortiums(ORG_ID);
        allc2 = organizationService.getConsortiums(ORG2_ID);
        allo = consortiumService.getOrganizations(c.getId());
        Assertions.assertEquals(0, allc2.size());
        Assertions.assertEquals(1, allc.size());
        Assertions.assertEquals(1, allo.size());
    }

    @Test
    void moveConsortium() throws Exception {
        Organization o = new Organization("test name");
        o.setId(ORG_ID);
        o = organizationService.put(o);
        Organization o2 = new Organization("test 2 name");
        o2.setId(ORG2_ID);
        o2 = organizationService.put(o2);

        Consortium c = new Consortium("Consort", "test", o.getId());
        c = consortiumService.put(c);

        c.setOrganization(ORG2_ID);
        c = consortiumService.put(c);

        List<Consortium> allc = organizationService.getConsortiums(ORG_ID);
        List<Consortium> allc2 = organizationService.getConsortiums(ORG2_ID);
        List<Organization> allo = consortiumService.getOrganizations(c.getId());
        Assertions.assertEquals(1, allc2.size());
        Assertions.assertEquals(0, allc.size());
        Assertions.assertEquals(1, allo.size());
    }

}
