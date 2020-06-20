/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.EventListener;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;

/**
 * Handle the Startup event, and initialize things that need initializing.
 */
@Configuration
public class StartupConfig {

    private static final Logger logger = LogManager.getFormatterLogger(StartupConfig.class);

    private DefaultProfiles defaultProfiles;
    private BlockchainService blockchainService;
    private ConnectionPoolManager connectionPoolManager;
    private ZoneService zoneService;
    private ReplicaService replicaService;

    @Autowired
    public StartupConfig(DefaultProfiles defaultProfiles, BlockchainService blockchainService,
                         ReplicaService replicaService,
                         ConnectionPoolManager connectionPoolManager, ZoneService zoneService) {
        this.defaultProfiles = defaultProfiles;
        this.blockchainService = blockchainService;
        this.connectionPoolManager = connectionPoolManager;
        this.zoneService = zoneService;
        this.replicaService = replicaService;
    }

    /**
     * Perform initialization tasks after Application is ready.
     */
    @EventListener(classes = ApplicationStartedEvent.class)
    public void applicationStarted() {
        // Create a connection pool for all the blockchains
        List<Blockchain> blockchains = blockchainService.list();
        for (Blockchain b : blockchains) {
            var ips = replicaService.getReplicas(b.getId()).stream().map(r -> r.getPublicIp())
                    .collect(Collectors.toList());
            try {
                connectionPoolManager.createPool(b.getId(), ips);
            } catch (IOException e) {
                logger.warn("Could not create connection pool for {}", b.getId());
            }
        }
        defaultProfiles.initialize();
        logger.info(String.format("Number of default zones loaded: %d", zoneService.getDefaultZones().size()));
    }

}
