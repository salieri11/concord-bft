/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import java.io.IOException;
import java.security.Security;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.EventListener;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
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
        Security.addProvider(new BouncyCastleProvider());
        defaultProfiles.initialize();
        logger.info(String.format("Number of default zones loaded: %d", zoneService.getDefaultZones().size()));

        // Create a connection pool for all ETHEREUM blockchains in a background task
        createConnectionPoolForAllBlockchains();
    }

    /**
     * Create Connection pool for All Blockchains asynchronously.
     */
    private void createConnectionPoolForAllBlockchains() {
        Map<UUID, List<Replica>> blockchainsWithReplicas =
                blockchainService.getCloudBlockchainsWithReplicas(Blockchain.BlockchainType.ETHEREUM);
        if (blockchainsWithReplicas != null && !blockchainsWithReplicas.isEmpty()) {
            blockchainsWithReplicas.keySet().forEach(bcId -> {
                // Ignore replicas with null public IP.
                List<String> ips = blockchainsWithReplicas.getOrDefault(bcId, List.of()).stream()
                        .filter(replica -> (replica.getPublicIp() != null && !replica.getPublicIp().isEmpty()))
                        .map(replica -> replica.getPublicIp())
                        .collect(Collectors.toList());
                try {
                    logger.debug("ips available {}", ips);
                    if (!ips.isEmpty()) {
                        connectionPoolManager.createPool(bcId, ips);
                    }
                } catch (IOException e) {
                    logger.warn("Could not create connection pool for {}", bcId);
                }
            });
        }
    }
}
