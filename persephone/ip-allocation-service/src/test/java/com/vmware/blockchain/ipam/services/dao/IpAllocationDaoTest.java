/*
 * Copyright (c) 2016 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services.dao;

import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.protobuf.ByteString;
import com.vmware.blockchain.base.auth.BaseAuthHelper;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.dao.TestDaoConfig;
import com.vmware.blockchain.db.DbConfig;
import com.vmware.blockchain.db.mapper.TestMapper;
import com.vmware.blockchain.ipam.services.BlockSpecification;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;


/**
 * Test the IPAM.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@ComponentScan(basePackageClasses = {GenericDao.class})
@EnableAutoConfiguration
@AutoConfigureEmbeddedDatabase
@ContextConfiguration(classes = {DbConfig.class, TestDaoConfig.class })
class IpAllocationDaoTest {
    private static final UUID ONPREM_ORG = UUID.fromString("747c4d97-63a5-4d1c-bed1-b9ae4f4dfcd0");
    private static final UUID USER_ID = UUID.fromString("5df113c4-6f7d-45a1-82f3-806dc6ec6a7e");
    private static final String USER_EMAIL = "test@foo.com";

    AddressBlock addressBlock1;
    AddressBlock addressBlock2;
    AddressBlock addressBlock3;
    AddressBlock addressBlock4;

    AddressBlockSegment addressBlockSegment1;
    AddressBlockSegment addressBlockSegment2;
    AddressBlockSegment addressBlockSegment3;
    AddressBlockSegment addressBlockSegment4;

    UUID userId;
    UUID tenantId;

    UUID entityId;

    private IpAllocationDao ipAllocationDao;

    @Autowired
    GenericDao genericDao;

    @Autowired
    TestMapper testMapper;

    @MockBean
    BaseAuthHelper authHelper;

    @BeforeEach
    void setUp() throws Exception {
        userId = UUID.fromString("7a833e12-6de8-4525-b55e-7cd482f6846e");
        when(authHelper.getUserId()).thenReturn(userId);
        when(authHelper.getEmail()).thenReturn("mockuser");

        tenantId = UUID.randomUUID();

        addressBlock1 = new AddressBlock("AB1", new BlockSpecification(), AddressBlock.State.ACTIVE);
        addressBlock2 = new AddressBlock("AB2", new BlockSpecification(), AddressBlock.State.ACTIVE);
        addressBlock3 = new AddressBlock("AB3", new BlockSpecification(), AddressBlock.State.ACTIVE);
        addressBlock4 = new AddressBlock("AB4", new BlockSpecification(), AddressBlock.State.ACTIVE);

        ipAllocationDao.createAddressBlock(addressBlock1);
        ipAllocationDao.createAddressBlock(addressBlock2);

        addressBlockSegment1 = new AddressBlockSegment("ABS1", 0, ByteString.EMPTY);
        addressBlockSegment2 = new AddressBlockSegment("ABS2", 0, ByteString.EMPTY);
        addressBlockSegment3 = new AddressBlockSegment("ABS3", 0, ByteString.EMPTY);
        addressBlockSegment4 = new AddressBlockSegment("ABS4", 0, ByteString.EMPTY);

        ipAllocationDao.createAddressBlockSegment(addressBlockSegment1);
        ipAllocationDao.createAddressBlockSegment(addressBlockSegment2);
    }

    @AfterEach
    void cleanup() {
        // Remove any entities we created
        testMapper.deleteEntity();
        testMapper.deleteEntityHistory();
        testMapper.deleteLink();
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    // @Test
    void testSimpleGetAddressBlock() throws Exception {
        entityId = addressBlock1.getId();
        AddressBlock addressBlock = ipAllocationDao.getAddressBlock(entityId);
        String name = addressBlock.getName();
        Assertions.assertEquals("AB1", name);
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    // @Test
    void testSimpleGetAddressBlockSegment() throws Exception {
        entityId = addressBlockSegment1.getId();
        AddressBlockSegment addressBlockSegment = ipAllocationDao.getAddressBlockSegment(entityId);
        String name = addressBlockSegment.getName();
        Assertions.assertEquals("ABS1", name);
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    // @Test
    void testSimplePutAddressBlock() throws Exception {
        AddressBlock addressBlock = ipAllocationDao.createAddressBlock(addressBlock3);

        Assertions.assertEquals(addressBlock.getName(), addressBlock3.getName());
        Assertions.assertEquals(addressBlock.getSpecification(), addressBlock3.getSpecification());
        Assertions.assertEquals(addressBlock.getState(), addressBlock3.getState());

        Assertions.assertNotNull(addressBlock.getCreated());
        Assertions.assertNotNull(addressBlock.getUpdated());
        Assertions.assertNotNull(addressBlock.getId());
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    // @Test
    void testSimplePutAddressBlockSegment() throws Exception {
        AddressBlockSegment addressBlockSegment = ipAllocationDao.createAddressBlockSegment(addressBlockSegment3);

        Assertions.assertEquals(addressBlockSegment.getName(), addressBlockSegment3.getName());
        Assertions.assertEquals(addressBlockSegment.getSegment(), addressBlockSegment3.getSegment());
        Assertions.assertEquals(addressBlockSegment.getAllocations(), addressBlockSegment3.getAllocations());

        Assertions.assertNotNull(addressBlockSegment.getCreated());
        Assertions.assertNotNull(addressBlockSegment.getUpdated());
        Assertions.assertNotNull(addressBlockSegment.getId());
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    // @Test
    void testSimpleDeleteAddressBlock() throws Exception {
        UUID addressBlockId = addressBlock1.getId();
        ipAllocationDao.deleteAddressBlock(addressBlockId);

        Assertions.assertNull(ipAllocationDao.getAddressBlock(addressBlockId));
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    // @Test
    void testSimpleDeleteAddressBlockSegment() throws Exception {
        UUID absId = addressBlockSegment1.getId();
        ipAllocationDao.deleteAddressBlockSegment(absId);

        Assertions.assertNull(ipAllocationDao.getAddressBlockSegment(absId));
    }
}