/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

import com.vmware.blockchain.common.AthenaProperties;

@RunWith(SpringRunner.class)
@TestPropertySource(locations = "classpath:test.properties")
@SpringBootTest(classes = AthenaProperties.class)
public class ConfigurationTest {
    @Autowired
    AthenaProperties config;

    @Test
    public void testOverrideConfiguration() {
        // Get a copy of the properties
        AthenaProperties props = config.instance();
        props.setConnectionPoolFactor(4);
        props.setConnectionPoolSize(20);
        Assert.assertEquals(4, props.getConnectionPoolFactor());
        Assert.assertEquals(20, props.getConnectionPoolSize());
        Assert.assertNotEquals(props.getConnectionPoolFactor(), config.getConnectionPoolFactor());
        Assert.assertNotEquals(props.getConnectionPoolSize(), config.getConnectionPoolSize());
    }
}
