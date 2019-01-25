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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.services.profiles.Blockchain;
import com.vmware.blockchain.services.profiles.Consortium;

/**
 * Connection pool unit tests.
 */
@ExtendWith(SpringExtension.class)
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
    @BeforeEach
    void setup() throws IOException {
        MockitoAnnotations.initMocks(this);
        when(consortium.getConsortiumId()).thenReturn(UUID.fromString("277858b5-b962-4aa5-850e-c992c84cfdcb"));
        when(consortium.getConsortiumName()).thenReturn("Test Name");

        blockchain = new Blockchain(UUID.fromString("33b26eed-d173-47bf-ab3a-184479a1fde0"),
                consortium, "ip1:5458,ip2:5458,ip3:5458,ip4:5458",
                "a=ip1:5458,b=ip2:5458,c=ip3:5458,d=ip4:5458",
                "a=thisisacert,b=thisisbcert,c=thisisccert,d=thisisdcert");
        pool = new ConcordConnectionPool(blockchain, ConnectionType.Mock).initialize(config);
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
        Assertions.assertTrue(blockchain.getUrlsAsMap().containsValue(conn.getIpStr()));
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
        List<String> ips = new LinkedList<>(blockchain.getIpAsList());
        // if we call this
        for (int i = 0; i < blockchain.getIpAsList().size(); i++) {
            MockConnection conn = (MockConnection) pool.getConnection();
            ips.remove(conn.getIpStr());
        }
        // we should have hit each ip once
        Assertions.assertEquals(0, ips.size());
    }

    @Test
    public void testCloseAll() throws IOException {
        ConcordConnectionPool tPool = new ConcordConnectionPool(blockchain, ConnectionType.Mock).initialize(config);
        Assertions.assertTrue(tPool.isInitialized());
        tPool.closeAll();
        Assertions.assertFalse(tPool.isInitialized());
    }

}
