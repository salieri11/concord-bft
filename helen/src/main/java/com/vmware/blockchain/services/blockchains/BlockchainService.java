/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainPatch;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.profiles.Consortium;


/**
 * Manage all persistence for Blockchain entities.
 */
@Service
public class BlockchainService {

    private static final Logger logger = LogManager.getLogger(Blockchain.class);

    private GenericDao genericDao;
    private ApplicationEventPublisher publisher;
    private ReplicaService replicaService;
    private ZoneService zoneService;

    @Autowired
    public BlockchainService(GenericDao genericDao, ApplicationEventPublisher publisher,
                             ReplicaService replicaService, ZoneService zoneService) {
        this.genericDao = genericDao;
        this.publisher = publisher;
        this.replicaService = replicaService;
        this.zoneService = zoneService;
    }

    /**
     * Create a new blockchain with the parameters and a specified UUID. Use this call when all we know about the
     * consortium is its Id.
     *
     * @param id           Preset UUID for this blockchain
     * @param consortiumId ID of consortium owning this blockchain
     * @param type         Type of blockchain
     * @param metadata     Blockchain component versions
     * @return Blockchain   Blockchain entity
     */
    public Blockchain create(UUID id, UUID consortiumId, BlockchainType type,
                             Map<String, String> metadata) {
        Blockchain b = new Blockchain.BlockchainBuilder()
                .consortium(consortiumId)
                .type(type)
                .metadata(metadata)
                .build();
        b.setId(id);
        b.setState(Blockchain.BlockchainState.ACTIVE);
        return genericDao.put(b, null);
    }

    /**
     * Patches a blockchain instance with new values.
     */
    public Blockchain update(Blockchain blockchain, BlockchainPatch newValues) {

        if (newValues.getBlockchainVersion() != null) {
            blockchain.setBlockchainVersion(newValues.getBlockchainVersion());
        }
        if (newValues.getExecutionEngineVersion() != null) {
            blockchain.setExecutionEngineVersion(newValues.getExecutionEngineVersion());
        }
        return genericDao.put(blockchain, blockchain);
    }

    /**
     * Create a new blockchain from an old one.  Copy the urls and iplist.
     */
    Blockchain update(Blockchain newBlockchain) {
        return genericDao.put(newBlockchain, null);
    }

    Blockchain put(Blockchain b) {
        return genericDao.put(b, null);
    }

    public List<Blockchain> list() {
        return genericDao.getAllByType(Blockchain.class);
    }

    public List<Blockchain> listByConsortium(Consortium consortium) {
        return genericDao.getByParentId(consortium.getId(), Blockchain.class);
    }

    // Returns the list of Blockchains from the permitted chain list
    public List<Blockchain> listByIds(List<UUID> ids) {
        return ids.stream().map(this::get).collect(Collectors.toList());
    }

    /**
     * Get a list of Blockchains of requested type.
     *
     * @param type Blockchain type
     * @return A list of Blockchains.
     */
    public List<Blockchain> listByType(BlockchainType type) {
        List<Blockchain> blockchains = genericDao.getAllByType(Blockchain.class);
        if (blockchains == null) {
            return new ArrayList<>();
        }
        return blockchains.stream().filter(bc -> (bc != null && bc.type == type)).collect(Collectors.toList());
    }

    public Blockchain get(UUID id) {
        return genericDao.get(id, Blockchain.class);
    }

    public Consortium getConsortium(UUID id) {
        return genericDao.get(id, Consortium.class);
    }

    /**
     * Get Key value pairs of Cloud based BlockchainIds along with a list of Replicas, for the given Blockchain type.
     *
     * @return A map of BlockchainId and its replicas.
     */
    public Map<UUID, List<Replica>> getCloudBlockchainsWithReplicas(BlockchainType blockchainType) {
        List<Blockchain> blockchains = listByType(blockchainType);
        if (blockchains == null) {
            logger.info("No Blockchain of type {} is available.", blockchainType);
            return new HashMap<>();
        }
        List<Replica> replicas = replicaService.getReplicasByZoneType(Zone.Type.VMC_AWS);
        if (replicas == null || replicas.isEmpty()) {
            logger.info("No replica of type {} is available.", Zone.Type.VMC_AWS);
            return new HashMap<>();
        }
        Map<UUID, List<Replica>> blockchainIdAndReplicas = new HashMap<>();
        replicas.forEach(replica -> {
            if (replica != null) {
                List<Replica> replicaList = blockchainIdAndReplicas.getOrDefault(replica.getBlockchainId(),
                                                                                 new ArrayList<>());
                replicaList.add(replica);
                blockchainIdAndReplicas.put(replica.getBlockchainId(), replicaList);
            }
        });
        return blockchainIdAndReplicas;
    }
}