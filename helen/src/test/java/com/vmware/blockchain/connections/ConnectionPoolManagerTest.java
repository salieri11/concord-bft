/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.connections.ConnectionPoolManagerTest.Config;
import com.vmware.blockchain.utils.ControllerTestConfig;

/**
 * Test the ConnectionPoolManger.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:test.properties")
@ContextConfiguration(classes = { Config.class, ControllerTestConfig.class })
public class ConnectionPoolManagerTest {

    ConnectionPoolManager manager;

    @Autowired
    ConcordProperties config;

    private ConcordConnectionPool pool1;
    private ConcordConnectionPool pool2;

    private final UUID chainId1 = UUID.fromString("9b22ea6f-5a2f-4159-b2f0-f10a1d751649");
    private final List<String> chainsIps1 = Arrays.asList("ip1:5458", "ip2:5458", "ip3:5458", "ip4:5458");

    private final UUID chainId2 = UUID.fromString("f6b18a1e-53fa-4716-863c-2b99891ab0b5");
    private final List<String> chainIps2 = Arrays.asList("vip1:5458,vip2:5458,vip3:5458,vip4:5458");

    /**
     * Initialize mocks and test structures.
     */
    @BeforeEach
    void init() throws IOException {
        manager = new ConnectionPoolManager(config);
        // This is the only reason the "type" field exists in the ConnectionPoolManager.
        ReflectionTestUtils.setField(manager, "type", ConnectionType.Mock);

        pool1 = manager.createPool(chainId1, chainsIps1);
        pool2 = manager.createPool(chainId2, chainIps2);
    }

    @Test
    public void testBasic() {
        Assertions.assertNotNull(pool1.getId());
        Assertions.assertNotNull(pool2.getId());
        Assertions.assertEquals(pool1, manager.getPool(chainId1));
        Assertions.assertEquals(pool2, manager.getPool(chainId2));
        Assertions.assertNotEquals(pool1, pool2);

    }

    @Test
    public void testPool() throws IllegalStateException, IOException, InterruptedException {
        MockConnection conn = (MockConnection) manager.getPool(chainId1).getConnection();
        // The connection we get should be in chain1, but not chain2
        Assertions.assertEquals(1,
                                chainsIps1.stream().filter(n -> n.equals(conn.getIpStr())).count());
        Assertions.assertEquals(0,
                                chainIps2.stream().filter(n -> n.equals(conn.getIpStr())).count());
        manager.getPool(chainId1).putConnection(conn);
    }

    @Test
    void testDoubleCreate() throws IOException {
        ConcordConnectionPool pool = manager.createPool(chainId1, chainsIps1);
        Assertions.assertEquals(pool1, pool);
    }

    @Configuration
    @ComponentScan(basePackageClasses = {ConnectionPoolManager.class, ConcordProperties.class})
    static class Config {
    }
}
