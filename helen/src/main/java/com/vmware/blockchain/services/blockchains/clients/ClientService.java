/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Service layer for clients.
 */
@Service
public class ClientService {

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

    public List<Client> getClientsByBlockchainId(UUID id) {
        return genericDao.getByParentId(id, Client.class);
    }
}