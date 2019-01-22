/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.ConcordProperties;

/**
 * Connections Unit test configuration.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:test.properties")
@SpringBootTest(classes = ConcordProperties.class)
public class ConfigurationTest {
    @Autowired
    ConcordProperties config;

    @Test
    void testOverrideConfiguration() {
        // Get a copy of the properties
        ConcordProperties props = config.instance();
        props.setConnectionPoolFactor(4);
        props.setConnectionPoolSize(20);
        Assertions.assertEquals(4, props.getConnectionPoolFactor());
        Assertions.assertEquals(20, props.getConnectionPoolSize());
        Assertions.assertNotEquals(props.getConnectionPoolFactor(), config.getConnectionPoolFactor());
        Assertions.assertNotEquals(props.getConnectionPoolSize(), config.getConnectionPoolSize());
    }
}
