/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.nodesizing;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

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
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.dao.TestDaoConfig;
import com.vmware.blockchain.db.DbConfig;
import com.vmware.blockchain.db.mapper.TestMapper;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.ProfilesService;

import io.grpc.ManagedChannel;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;

/**
 * Test the NodeSizeTemplateService.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@ComponentScan(basePackageClasses = {GenericDao.class, ConsortiumService.class, OrganizationService.class})
@EnableAutoConfiguration
@AutoConfigureEmbeddedDatabase
@ContextConfiguration(classes = {MvcConfig.class, DbConfig.class, TestDaoConfig.class })
public class NodeSizeTemplateServiceTest {
    private static final UUID USER_ID = UUID.fromString("5df113c4-6f7d-45a1-82f3-806dc6ec6a7e");
    private static final String USER_EMAIL = "test@foo.com";
    private static final UUID DEFAULT_TEMPLATE_ID = UUID.fromString("155de65a-da76-11ea-87d0-0242ac130003");
    private static final String DEFAULT_TEMPLATE_NAME = "Default Template";

    @Autowired
    GenericDao genericDao;

    @Autowired
    TestMapper testMapper;

    @MockBean
    private ProfilesService prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    @Qualifier("provisioningServerChannel")
    ManagedChannel channel;

    private NodeSizeTemplateService nodeSizeTemplateService;

    @BeforeEach
    void setUp() {
        when(authHelper.hasAnyAuthority(anyString())).thenReturn(true);
        //when(authHelper.getOrganizationId()).thenReturn(ONPREM_ORG);
        when(authHelper.getUserId()).thenReturn(USER_ID);
        when(authHelper.getEmail()).thenReturn(USER_EMAIL);

        NodeSizeTemplate nst = NodeSizeTemplateUtil.createNodeSizeTemplate(DEFAULT_TEMPLATE_ID, DEFAULT_TEMPLATE_NAME);

        nodeSizeTemplateService = new NodeSizeTemplateService(genericDao);

        genericDao.put(nst, null);
    }

    @AfterEach
    void cleanup() {
        // Remove any entities we created during setup.
        // If we don't cleanup, we will run into ConcurrentUpdateException, because
        // genericDao.put(..) method is saving the same entity object again and again.
        testMapper.deleteEntity();
        testMapper.deleteEntityHistory();
        testMapper.deleteLink();
    }

    @Test
    void testGetTemplate() {
        // This code assumes that we support default template only.
        // If we do introduce templates at different levels, then this code needs changing.
        NodeSizeTemplate nst = nodeSizeTemplateService.getTemplate();
        Assertions.assertNotNull(nst);
        Assertions.assertEquals(DEFAULT_TEMPLATE_ID, nst.getId());
    }


    @Test
    void testGetById() {
        NodeSizeTemplate nst = nodeSizeTemplateService.get(DEFAULT_TEMPLATE_ID);
        Assertions.assertNotNull(nst);
        Assertions.assertEquals(DEFAULT_TEMPLATE_ID, nst.getId());
    }

    @Test
    void testPut() {
        NodeSizeTemplate nst = nodeSizeTemplateService.get(DEFAULT_TEMPLATE_ID);
        // change some values to save.
        String name = "Org level template";
        nst.setName(name);
        NodeSizeTemplate result = nodeSizeTemplateService.put(nst);
        Assertions.assertNotNull(result);
        Assertions.assertEquals(DEFAULT_TEMPLATE_ID, result.getId());
        Assertions.assertEquals(name, result.getName());
    }

    @Test
    void delete() {
        nodeSizeTemplateService.delete(DEFAULT_TEMPLATE_ID);
        // This get call should throw NotFoundException. If it did not, then that's a problem.
        try {
            NodeSizeTemplate nst = nodeSizeTemplateService.get(DEFAULT_TEMPLATE_ID);
            Assertions.assertNull(nst);
        } catch (NotFoundException nfe) {
            Assertions.assertTrue(true);
        }
    }
}
