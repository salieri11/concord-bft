/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool.ConnectionType;
import com.vmware.blockchain.services.blockchains.BlockchainManagerEvent;

/**
 * Connection Pool Manager for blockchains.
 * Create/delete and get connection pools.
 */
@Component
public class ConnectionPoolManager {
    private ConcurrentMap<UUID, ConcordConnectionPool> pools;
    // This field is really just for testing.  In unit tests, this will be set to Mock
    private ConnectionType type = ConnectionType.TCP;
    private ConcordProperties config;

    @Autowired
    public ConnectionPoolManager(ConcordProperties config) {
        this.pools = new ConcurrentHashMap<>();
        this.config = config;
    }

    /**
     * Create a new connection pool for the blockchain.
     * Throws AlreadyBoundException if the blockchain is already present.
     */
    public ConcordConnectionPool createPool(UUID blockchainId, List<String> ips) throws IOException {
        ConcordConnectionPool newPool = new ConcordConnectionPool(ips, type);
        // set the pool.  If it already was there, pool != null
        ConcordConnectionPool pool = pools.putIfAbsent(blockchainId, newPool);
        if (pool == null) {
            return newPool.initialize(config);
        }
        return pool;
    }

    public ConcordConnectionPool getPool(UUID blockchain) {
        return pools.get(blockchain);
    }

    public void deletePool(UUID blockchain) {
        ConcordConnectionPool pool = pools.remove(blockchain);
        pool.closeAll();
    }

    @EventListener
    public void handleBlockchainEvent(BlockchainManagerEvent event)  {
        // TODO: update the connection pool.
    }
}
