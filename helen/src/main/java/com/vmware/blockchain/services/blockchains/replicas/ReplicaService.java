/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.replicas;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;

/**
 * Replica Service. Handle replica persistence.
 */
@Service
public class ReplicaService {
    private static final Logger logger = LogManager.getLogger();

    private GenericDao genericDao;
    private ZoneService zoneService;

    @Autowired
    public ReplicaService(GenericDao genericDao, ZoneService zoneService) {
        this.genericDao = genericDao;
        this.zoneService = zoneService;
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

    /**
     * Get Replicas based on the zone type.
     *
     * @param zoneType Zone type
     * @return A list of Replicas on the supplied zone type.
     */
    public List<Replica> getReplicasByZoneType(Zone.Type zoneType) {
        // Get a list of cloud based zones.
        List<UUID> zoneIds = zoneService.getZoneIdsByType(zoneType);
        List<Replica> replicas = new ArrayList<>();
        zoneIds.forEach(zoneId -> {
            replicas.addAll(getReplicasByParentId(zoneId));
        });
        return replicas;
    }

}
