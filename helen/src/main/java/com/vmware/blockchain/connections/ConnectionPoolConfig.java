/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.services.profiles.Blockchain;

/**
 * This is a temporary config to allow the creation of the ConcordConnectionPool bean.
 * At the moment, this is necessary so the existing APIs work.  When the APIs have been
 * extended to understand blockchains, this can go away.
 */
@Configuration
public class ConnectionPoolConfig {
    private static Logger logger = LogManager.getLogger(ConnectionPoolConfig.class);

    private ConcordProperties config;
    private String blockchainNodes;

    @Autowired
    public ConnectionPoolConfig(ConcordProperties config, @Value("${ConcordAuthorities}") String blockchainNodes) {
        this.config = config;
        this.blockchainNodes = blockchainNodes;
    }

    /**
     * Create the default connection pool.
     */
    @Bean
    public ConcordConnectionPool concordConnectionPool() {
        Blockchain blockchain = new Blockchain(null, null, blockchainNodes);
        try {
            return new ConcordConnectionPool(blockchain).initialize(config);
        } catch (IOException e) {
            logger.warn("Could not create default connection pool", e);
            return null;
        }
    }

}
