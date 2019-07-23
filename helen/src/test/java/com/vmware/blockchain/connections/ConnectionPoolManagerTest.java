/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

import com.google.common.base.Splitter;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.connections.ConnectionPoolManagerTest.Config;
import com.vmware.blockchain.services.blockchains.Blockchain;
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

    private Blockchain chain1;
    private Blockchain chain2;

    private ConcordConnectionPool pool1;
    private ConcordConnectionPool pool2;

    /**
     * Initialize mocks and test structures.
     */
    @BeforeEach
    void init() throws IOException {
        manager = new ConnectionPoolManager(config);
        // This is the only reason the "type" field exists in the ConnectionPoolManager.
        ReflectionTestUtils.setField(manager, "type", ConnectionType.Mock);
        chain1 = Blockchain.builder()
                .consortium(null)
                .nodeList(Stream.of("ip1:5458", "ip2:5458", "ip3:5458", "ip4:5458")
                        .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "cert", null))
                        .collect(Collectors.toList()))
                .build();
        chain1.setId(UUID.fromString("9b22ea6f-5a2f-4159-b2f0-f10a1d751649"));
        chain2 = Blockchain.builder().consortium(null)
                .nodeList(Splitter.on(",").splitToList("vip1:5458,vip2:5458,vip3:5458,vip4:5458").stream()
                        .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "cert", null))
                        .collect(Collectors.toList()))
                .build();
        chain2.setId(UUID.fromString("f6b18a1e-53fa-4716-863c-2b99891ab0b5"));
        pool1 = manager.createPool(chain1);
        pool2 = manager.createPool(chain2);
    }

    @Test
    public void testBasic() {
        Assertions.assertNotNull(pool1.getId());
        Assertions.assertNotNull(pool2.getId());
        Assertions.assertEquals(pool1, manager.getPool(chain1.getId()));
        Assertions.assertEquals(pool2, manager.getPool(chain2.getId()));
        Assertions.assertNotEquals(pool1, pool2);

    }

    @Test
    public void testPool() throws IllegalStateException, IOException, InterruptedException {
        MockConnection conn = (MockConnection) manager.getPool(chain1.getId()).getConnection();
        // The connection we get should be in chain1, but not chain2
        Assertions.assertEquals(1,
                chain1.getNodeList().stream().filter(n -> n.getIp().equals(conn.getIpStr())).count());
        Assertions.assertEquals(0,
                chain2.getNodeList().stream().filter(n -> n.getIp().equals(conn.getIpStr())).count());
        manager.getPool(chain1.getId()).putConnection(conn);
    }

    @Test
    void testDoubleCreate() throws IOException {
        ConcordConnectionPool pool = manager.createPool(chain1);
        Assertions.assertEquals(pool1, pool);

    }

    @Configuration
    @ComponentScan(basePackageClasses = {ConnectionPoolManager.class, ConcordProperties.class})
    static class Config {
    }
}
