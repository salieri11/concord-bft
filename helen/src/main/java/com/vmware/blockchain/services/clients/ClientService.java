/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.clients;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Service layer for clients.
 */
public class ClientService {
    private static final Logger logger = LogManager.getLogger();

    private GenericDao genericDao;

    @Autowired
    public ClientService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public Client put(Client client) {
        return genericDao.put(client, null);
    }

    public Client get(UUID id) {
        return genericDao.get(id, Client.class);
    }
}
