/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.UUID;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.services.profiles.Blockchain;

/**
 * Test the ConnectionPoolManger.
 */
@RunWith(SpringRunner.class)
@TestPropertySource(locations = "classpath:test.properties")
@ContextConfiguration(classes = {ConnectionPoolManagerTest.Config.class})
public class ConnectionPoolManagerTest {

    // Mock out the current ConcordConnectionpool bean.  This can be removed when that is.
    @MockBean
    ConcordConnectionPool concordConnectionPool;

    @Autowired
    ConnectionPoolManager manager;

    @Autowired
    ConcordProperties config;

    private Blockchain chain1;
    private Blockchain chain2;

    private ConcordConnectionPool pool1;
    private ConcordConnectionPool pool2;

    /**
     * Initialize mocks and test structures.
     */
    @Before
    public void init() throws IOException {
        // This is the only reason the "type" field exists in the ConnectionPoolManager.
        ReflectionTestUtils.setField(manager, "type", ConnectionType.Mock);
        chain1 = new Blockchain(UUID.fromString("9b22ea6f-5a2f-4159-b2f0-f10a1d751649"), null,
                "ip1:5458,ip2:5458,ip3:5458,ip4:5458");
        chain2 = new Blockchain(UUID.fromString("f6b18a1e-53fa-4716-863c-2b99891ab0b5"), null,
                "vip1:5458,vip2:5458,vip3:5458,vip4:5458");
        pool1 = manager.createPool(chain1);
        pool2 = manager.createPool(chain2);
    }

    @Test
    public void testBasic() {
        Assert.assertNotNull(pool1.getId());
        Assert.assertNotNull(pool2.getId());
        Assert.assertEquals(pool1, manager.getPool(chain1));
        Assert.assertEquals(pool2, manager.getPool(chain2));
        Assert.assertNotEquals(pool1, pool2);
    }

    @Test
    public void testPool() throws IllegalStateException, IOException, InterruptedException {
        MockConnection conn = (MockConnection) manager.getPool(chain1).getConnection();
        // The connection we get should be in chain1, but not chain2
        Assert.assertTrue(chain1.getIpAsList().contains(conn.getIpStr()));
        Assert.assertFalse(chain2.getIpAsList().contains(conn.getIpStr()));
        manager.getPool(chain1).putConnection(conn);
    }

    @Test
    public void testDoubleCreate() throws IOException {
        ConcordConnectionPool pool = manager.createPool(chain1);
        Assert.assertEquals(pool1, pool);

    }

    /**
     * Test config.
     */
    @Configuration
    @ComponentScan(basePackageClasses = {ConnectionPoolManager.class, ConcordProperties.class})
    public static class Config {

    }
}
