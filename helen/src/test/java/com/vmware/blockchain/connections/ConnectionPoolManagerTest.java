/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.services.profiles.Blockchain;

/**
 * Test the ConnectionPoolManger.
 */
@ExtendWith(SpringExtension.class)
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
    @BeforeEach
    void init() throws IOException {
        // This is the only reason the "type" field exists in the ConnectionPoolManager.
        ReflectionTestUtils.setField(manager, "type", ConnectionType.Mock);
        chain1 = Blockchain.builder()
                    .consortium(null)
                    .ipList("ip1:5458,ip2:5458,ip3:5458,ip4:5458")
                    .rpcUrls("a=ip1:5458,b=ip2:5458,c=ip3:5458,d=ip4:5458")
                    .rpcCerts("a=thisisacert,b=thisisbcert,c=thisisccert,d=thisisdcert")
                    .build();
        chain1.setId(UUID.fromString("9b22ea6f-5a2f-4159-b2f0-f10a1d751649"));
        chain2 = Blockchain.builder()
                    .consortium(null)
                    .ipList("vip1:5458,vip2:5458,vip3:5458,vip4:5458")
                    .rpcUrls("aa=vip1:5458,bb=vip2:5458,cc=vip3:5458,dd=vip4:5458")
                    .rpcCerts("a=thisisaacert,b=thisisbbcert,c=thisiscccert,d=thisisddcert")
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
        Assertions.assertTrue(chain1.getUrlsAsMap().containsValue(conn.getIpStr()));
        Assertions.assertFalse(chain2.getUrlsAsMap().containsValue(conn.getIpStr()));
        manager.getPool(chain1.getId()).putConnection(conn);
    }

    @Test
    void testDoubleCreate() throws IOException {
        ConcordConnectionPool pool = manager.createPool(chain1);
        Assertions.assertEquals(pool1, pool);

    }

    /**
     * Test config.
     */
    @Configuration
    @ComponentScan(basePackageClasses = {ConnectionPoolManager.class, ConcordProperties.class})
    public static class Config {

    }
}
