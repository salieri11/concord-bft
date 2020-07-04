/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
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

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.base.auth.Role;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.dao.TestDaoConfig;
import com.vmware.blockchain.db.DbConfig;
import com.vmware.blockchain.db.mapper.TestMapper;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.ethereum.EthDispatcher;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.ManagedChannel;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;

/**
 * Testing database access to User entities.  In particular, test that the conversion from the old style
 * roles to the new style rolse works correctly.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@ComponentScan(basePackageClasses = {GenericDao.class, UserService.class, OrganizationService.class,
                                     TestDaoConfig.class})
@EnableAutoConfiguration
@AutoConfigureEmbeddedDatabase
@ContextConfiguration(classes = {MvcConfig.class, DbConfig.class, TestDaoConfig.class })
class UserServiceTest {
    private static UUID ORG_ID = UUID.fromString("b07c32a2-8630-4a32-b346-63e1b43aa6c4");
    private static UUID USER_ID = UUID.fromString("3d312c6c-2483-4e34-a4c2-c9267854e939");
    private static String EMAIL = "user@test.com";

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

    @MockBean
    ConsortiumService consortiumService;

    @Autowired
    private TestMapper testMapper;

    @Autowired
    private UserService userService;

    @Autowired
    OrganizationService organizationService;

    @Autowired
    private GenericDao genericDao;

    @BeforeEach
    void init() {
        when(authHelper.hasAnyAuthority(anyString())).thenReturn(true);
        when(authHelper.getOrganizationId()).thenReturn(ORG_ID);
        when(authHelper.getUserId()).thenReturn(USER_ID);
        when(authHelper.getEmail()).thenReturn(EMAIL);
        Organization org = new Organization("OrgName");
        org.setId(ORG_ID);
        organizationService.put(org);
    }

    @AfterEach
    void cleanup() {
        // Remove any entities we created
        testMapper.deleteEntity();
        testMapper.deleteEntityHistory();
        testMapper.deleteLink();
    }

    @Test
    void testBasic() throws Exception {
        List<Role> roles = ImmutableList.of(VmbcRoles.ORG_USER, VmbcRoles.CONSORTIUM_ADMIN);
        User user = User.builder()
                .email("user2@test.com")
                .firstName("User")
                .lastName("2")
                .serviceRoles(roles)
                .organization(ORG_ID).build();
        user = userService.put(user);
        Assertions.assertNotNull(user.getId());
        Assertions.assertEquals(roles, user.getServiceRoles());
    }

    @Test
    void testGetOldRoles() throws Exception {
        List<Role> roles = ImmutableList.of(VmbcRoles.ORG_USER, VmbcRoles.CONSORTIUM_ADMIN);
        List<Roles> oldRoles = ImmutableList.of(Roles.ORG_USER, Roles.CONSORTIUM_ADMIN);
        User user = User.builder()
                .email("user2@test.com")
                .firstName("User")
                .lastName("2")
                .roles(oldRoles)
                .organization(ORG_ID).build();
        // bypass the logic in UserService to save old roles.
        user = genericDao.put(user, null);
        user = userService.get(user.getId());
        Assertions.assertNotNull(user.getId());
        Assertions.assertEquals(roles, user.getServiceRoles());
        Assertions.assertNull(user.getRoles());
    }

    @Test
    void testPutOldRoles() throws Exception {
        List<Role> roles = ImmutableList.of(VmbcRoles.ORG_USER, VmbcRoles.CONSORTIUM_ADMIN);
        List<Roles> oldRoles = ImmutableList.of(Roles.ORG_USER, Roles.CONSORTIUM_ADMIN);
        User user = User.builder()
                .email("user2@test.com")
                .firstName("User")
                .lastName("2")
                .roles(oldRoles)
                .organization(ORG_ID).build();
        // bypass the logic in UserService to save old roles.
        user = userService.put(user);
        Assertions.assertNotNull(user.getId());
        Assertions.assertEquals(roles, user.getServiceRoles());
        Assertions.assertNull(user.getRoles());
        user = userService.get(user.getId());
        Assertions.assertNotNull(user.getId());
        Assertions.assertEquals(roles, user.getServiceRoles());
        Assertions.assertNull(user.getRoles());
    }
}