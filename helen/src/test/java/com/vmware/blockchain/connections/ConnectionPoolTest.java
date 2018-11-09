/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.connections.AthenaConnectionFactory.ConnectionType;

/**
 * Connection pool unit tests.
 */
@RunWith(SpringRunner.class)
@TestPropertySource(locations = "classpath:test.properties")
@SpringBootTest(classes = {AthenaProperties.class})
public class ConnectionPoolTest {

    private AthenaConnectionPool pool;
    @Autowired
    private AthenaProperties config;

    private final Logger log = LogManager.getLogger(ConnectionPoolTest.class);

    @Before
    public void setup() throws IOException {
        pool = new AthenaConnectionPool().initialize(config, new AthenaConnectionFactory(ConnectionType.Mock, config));
    }


    /**
     * Tear down the test pool.
     */
    @After
    public void tearDown() {
        log.info("tearDown");
        if (pool != null) {
            pool.closeAll();
        }
    }

    @Test
    public void testConnectionCheck() throws IOException, InterruptedException {
        IAthenaConnection conn = pool.getConnection();
        Assert.assertNotNull(conn);
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

}
