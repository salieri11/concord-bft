/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.replicas;

import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Replica Service. Handle replica persistence.
 */
@Service
public class ReplicaService {
    private static final Logger logger = LogManager.getLogger();

    private GenericDao genericDao;

    @Autowired
    public ReplicaService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    public Replica put(Replica replica) {
        return genericDao.put(replica, null);
    }

    public Replica get(UUID id) {
        return genericDao.get(id, Replica.class);
    }

    public List<Replica> getReplicas(UUID id) {
        return genericDao.getByParentId(id, Replica.class);
    }

    // This will return a list of Replicas for a given Zone ID.
    // Retrieves based on fields marked @LinkedEntityId.
    public List<Replica> getReplicasByParentId(UUID id) {
        return genericDao.getByParentId(id, Replica.class);
    }
}
