/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration for the DAO tests. Really just component scans.
 */
@Configuration
@EnableAutoConfiguration
@ComponentScan(basePackages = {"com.vmware.blockchain.dao", "com.vmware.blockchain.db"})
public class TestDaoConfig {
}

