/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.connections.ConnectionPoolManager;


/**
 * Manage all persistence for Blockchain entities.
 */
@Component
public class BlockchainManager {
    private static final Logger logger = LogManager.getLogger(Blockchain.class);

    private BlockchainRepository blockchainRepo;
    private ApplicationEventPublisher publisher;
    private ConnectionPoolManager connectionPoolManager;

    @Autowired
    public BlockchainManager(BlockchainRepository blockchainRepo, ApplicationEventPublisher publisher,
            ConnectionPoolManager connectionPoolManager) {
        this.blockchainRepo = blockchainRepo;
        this.publisher = publisher;
        this.connectionPoolManager = connectionPoolManager;
    }

    // Clean up the string.  Remove leading and trailing spaces,
    // remove leading and trailing spaces around "=" and ","
    private String cleanupIpMap(String ipList) {
        // split on comma, remove leading and trailing whitespace.
        return ipList.trim().replaceAll("\\s*,\\s*", ",").replaceAll("\\s*=\\s*", "=");
    }

    /**
     * Create a new blockchain with the given ip list.
     * @param ipList List of IPs for the concord nodes
     * @return Blockchain entity
     */
    public Blockchain create(Consortium consortium, String ipList, String rpcUrls) {
        Blockchain b = new Blockchain();
        b.setConsortium(consortium);
        b.setIpList(ipList);
        b.setRpcUrls(cleanupIpMap(rpcUrls));
        b = blockchainRepo.save(b);
        try {
            connectionPoolManager.createPool(b);
        } catch (IOException e) {
            // Not sure what to do here.  Probably need to throw an error.
            logger.warn("Connection pool creation failed for blockchain {}", b.getId());
        }
        return b;
    }

    /**
     * Create a new blockchain from an old one.  Copy the urls and iplist.
     */
    public Blockchain update(Blockchain newBlockchain) {
        newBlockchain.setIpList(newBlockchain.getIpList());
        newBlockchain.setRpcUrls(cleanupIpMap(newBlockchain.getRpcUrls()));
        return blockchainRepo.save(newBlockchain);
    }


    public List<Blockchain> list() {
        return blockchainRepo.findAll();
    }

    public List<Blockchain> listByConsortium(Consortium consortium) {
        return blockchainRepo.findAllByConsortium(consortium);
    }

    public Optional<Blockchain> get(UUID id) {
        return blockchainRepo.findById(id);
    }
}
