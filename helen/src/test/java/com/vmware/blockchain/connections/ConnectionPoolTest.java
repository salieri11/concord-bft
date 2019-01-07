/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.services.profiles.Blockchain;
import com.vmware.blockchain.services.profiles.Consortium;

/**
 * Connection pool unit tests.
 */
@RunWith(SpringRunner.class)
@TestPropertySource(locations = "classpath:test.properties")
@SpringBootTest(classes = {ConcordProperties.class})
public class ConnectionPoolTest {

    private ConcordConnectionPool pool;

    @Autowired
    private ConcordProperties config;

    @Mock
    private Consortium consortium;

    private Blockchain blockchain;

    private final Logger log = LogManager.getLogger(ConnectionPoolTest.class);

    /**
     * Initialize mocks.
     */
    @Before
    public void setup() throws IOException {
        MockitoAnnotations.initMocks(this);
        when(consortium.getConsortiumId()).thenReturn(UUID.fromString("277858b5-b962-4aa5-850e-c992c84cfdcb"));
        when(consortium.getConsortiumName()).thenReturn("Test Name");

        blockchain = new Blockchain(UUID.fromString("33b26eed-d173-47bf-ab3a-184479a1fde0"), consortium,
                "ip1:5458,ip2:5458,ip3:5458,ip4:5458");
        pool = new ConcordConnectionPool(blockchain, ConnectionType.Mock).initialize(config);
    }


    /**
     * Tear down the test pool.
     */
    @After
    public void tearDown() {
        log.info("tearDown");
        if (pool != null && pool.isInitialized()) {
            pool.closeAll();
        }
    }

    @Test
    public void testPool() {
        Assert.assertTrue(pool.isInitialized());
        Assert.assertNotNull(pool.getId());
    }

    @Test
    public void testConnectionCheck() throws IOException, InterruptedException {
        MockConnection conn = (MockConnection) pool.getConnection();
        Assert.assertNotNull(conn);
        Assert.assertTrue(blockchain.getIpAsList().contains(conn.getIpStr()));
        if (conn != null) {
            pool.putConnection(conn);
        }
        log.info("testConnectionCheck end");
    }

    @Test
    public void testConnectionSetup() {
        Assert.assertEquals(config.getConnectionPoolSize(), pool.getTotalConnections());
        log.info("testConnectinSetup end");
    }

    @Test
    public void testRoundRobin() throws IllegalStateException, IOException, InterruptedException {
        // copy of the ip list we can modify
        List<String> ips = new LinkedList<>(blockchain.getIpAsList());
        // if we call this
        for (int i = 0; i < blockchain.getIpAsList().size(); i++) {
            MockConnection conn = (MockConnection) pool.getConnection();
            ips.remove(conn.getIpStr());
        }
        // we should have hit each ip once
        Assert.assertEquals(0, ips.size());
    }

    @Test
    public void testCloseAll() throws IOException {
        ConcordConnectionPool tPool = new ConcordConnectionPool(blockchain, ConnectionType.Mock).initialize(config);
        Assert.assertTrue(tPool.isInitialized());
        tPool.closeAll();
        Assert.assertFalse(tPool.isInitialized());
    }

}
