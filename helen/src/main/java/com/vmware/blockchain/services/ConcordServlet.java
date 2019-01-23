/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services;

import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.profiles.DefaultProfiles;

/**
 * Base for all Helen Controllers.
 */
@Controller
public abstract class ConcordServlet implements IParseToJson {
    protected static final long serialVersionUID = 1L;

    protected ConnectionPoolManager connectionPoolManager;
    protected DefaultProfiles defaultProfiles;

    @Autowired
    public ConcordServlet(ConnectionPoolManager connectionPoolManager, DefaultProfiles defaultProfiles) {
        this.connectionPoolManager = connectionPoolManager;
        this.defaultProfiles = defaultProfiles;
    }

    protected ConcordConnectionPool getConnectionPool(Optional<UUID> id) {
        UUID bid = id.orElse(defaultProfiles.getBlockchain().getId());
        return connectionPoolManager.getPool(bid);
    }

    protected UUID getBlockchainId(Optional<UUID> id) {
        return id.orElse(defaultProfiles.getBlockchain().getId());
    }

    protected ConcordControllerHelper getHelper(Optional<UUID> id) {
        UUID bid = id.orElse(defaultProfiles.getBlockchain().getId());
        ConcordConnectionPool connectionPool = connectionPoolManager.getPool(bid);
        return new ConcordControllerHelper(bid, connectionPool, this);
    }
}
