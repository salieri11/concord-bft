/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

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
import com.vmware.blockchain.services.profiles.Consortium;


/**
 * Manage all persistence for Blockchain entities.
 */
@Service
public class BlockchainService {
    private static final Logger logger = LogManager.getLogger(Blockchain.class);

    private GenericDao genericDao;
    private ApplicationEventPublisher publisher;

    @Autowired
    public BlockchainService(GenericDao genericDao, ApplicationEventPublisher publisher) {
        this.genericDao = genericDao;
        this.publisher = publisher;
    }

    /**
     * Create a new blockchain with the parameters and a specified UUID.
     * Use this call when all we know about the consortium is its Id.
     *
     * @param id            Preset UUID for this blockchain
     * @param consortiumId  ID of consortium owning this blockchain
     * @param type          Type of blockchain
     * @param metadata      Blockchain component versions
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

    public Blockchain get(UUID id) {
        return genericDao.get(id, Blockchain.class);
    }

    public Consortium getConsortium(UUID id) {
        return genericDao.get(id, Consortium.class);
    }

}
