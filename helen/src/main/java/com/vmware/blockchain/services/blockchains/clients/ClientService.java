/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import static com.vmware.blockchain.services.blockchains.clients.ClientController.ClientPatch;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.HelenException;
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

    // This will return a list of Clients for a given Zone ID or Blockchain ID.
    // Retrieves based on fields marked @LinkedEntityId.
    public List<Client> getClientsByParentId(UUID id) {
        return genericDao.getByParentId(id, Client.class);
    }

    /**
     * Updates a client entity, currently only DAML DB password update is implemented.
     * */
    public Client updateClient(Client client, ClientPatch newClientValues) {
        String newPassword = newClientValues.getDamlDbPassword();
        if (newPassword.isEmpty()) {
            throw new HelenException(HttpStatus.BAD_REQUEST, ErrorCodeType.EMPTY_PASSWORD_NOT_ALLOWED);
        }
        if (client.getDamlDbPassword().compareTo(newPassword) != 0) {
            client.setDamlDbPassword(newPassword);
            return genericDao.put(client, client);
        }
        return client;
    }
}
