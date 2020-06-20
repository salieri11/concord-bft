/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;

/**
 * Connection pool unit tests.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:test.properties")
@SpringBootTest(classes = {ConcordProperties.class})
public class ConnectionPoolTest {

    private final Logger log = LogManager.getLogger(ConnectionPoolTest.class);

    private ConcordConnectionPool pool;

    @Autowired
    private ConcordProperties config;

    /**
     * Initialize mocks.
     */
    @BeforeEach
    void setup() throws IOException {
        MockitoAnnotations.initMocks(this);
        pool = new ConcordConnectionPool(Arrays.asList("ip1:5458", "ip2:5458", "ip3:5458", "ip4:5458"),
                                         ConnectionType.Mock).initialize(config);
    }


    /**
     * Tear down the test pool.
     */
    @AfterEach
    void tearDown() {
        log.info("tearDown");
        if (pool != null && pool.isInitialized()) {
            pool.closeAll();
        }
    }

    @Test
    void testPool() {
        Assertions.assertTrue(pool.isInitialized());
        Assertions.assertNotNull(pool.getId());
    }

    @Test
    public void testConnectionCheck() throws IOException, InterruptedException {
        MockConnection conn = (MockConnection) pool.getConnection();
        Assertions.assertNotNull(conn);
        Assertions.assertEquals(1,
                                Arrays.asList("ip1:5458", "ip2:5458", "ip3:5458", "ip4:5458").stream()
                                        .filter(n -> n.equals(conn.getIpStr())).count());
        if (conn != null) {
            pool.putConnection(conn);
        }
        log.info("testConnectionCheck end");
    }

    @Test
    public void testConnectionSetup() {
        Assertions.assertEquals(config.getConnectionPoolSize(), pool.getTotalConnections());
        log.info("testConnectinSetup end");
    }

    @Test
    public void testRoundRobin() throws IllegalStateException, IOException, InterruptedException {
        // copy of the ip list we can modify
        List<String> ips = new ArrayList<>(Arrays.asList("ip1:5458", "ip2:5458", "ip3:5458", "ip4:5458"));
        int n = ips.size();
        // if we call this
        for (int i = 0; i < n; i++) {
            MockConnection conn = (MockConnection) pool.getConnection();
            ips.remove(conn.getIpStr());
        }
        // we should have hit each ip once
        Assertions.assertEquals(0, ips.size());
    }

    @Test
    public void testCloseAll() throws IOException {
        ConcordConnectionPool tPool = new ConcordConnectionPool(Arrays.asList("ip1:5458", "ip2:5458", "ip3:5458",
                                                                              "ip4:5458"),
                                                                ConnectionType.Mock).initialize(config);
        Assertions.assertTrue(tPool.isInitialized());
        tPool.closeAll();
        Assertions.assertFalse(tPool.isInitialized());
    }

}
