/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;


/**
 * Manage all persistence for Blockchain entities.
 */
@Component
public class BlockchainService {
    private static final Logger logger = LogManager.getLogger(Blockchain.class);

    private GenericDao genericDao;
    private ApplicationEventPublisher publisher;
    private ConnectionPoolManager connectionPoolManager;

    @Autowired
    public BlockchainService(GenericDao genericDao, ApplicationEventPublisher publisher,
            ConnectionPoolManager connectionPoolManager) {
        this.genericDao = genericDao;
        this.publisher = publisher;
        this.connectionPoolManager = connectionPoolManager;
    }

    // Clean up the string.  Remove leading and trailing spaces,
    // remove leading and trailing spaces around "=" and ","
    private String cleanupIpString(String ipList) {
        // split on comma, remove leading and trailing whitespace.
        return ipList.trim().replaceAll("\\s*,\\s*", ",").replaceAll("\\s*=\\s*", "=");
    }

    /**
     * Create a new blockchain with the given ip list.
     * @param ipList List of IPs for the concord nodes
     * @return Blockchain entity
     */
    public Blockchain create(Consortium consortium, String ipList, String rpcUrls, String rpcCerts) {
        Blockchain b = new Blockchain.BlockchainBuilder()
                .consortium(consortium.getConsortiumId())
                .ipList(cleanupIpString(ipList))
                .rpcUrls(cleanupIpString(rpcUrls))
                .rpcCerts(cleanupIpString(rpcCerts))
                .build();
        b = genericDao.put(b, null);
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
        newBlockchain.setIpList(cleanupIpString(newBlockchain.getIpList()));
        newBlockchain.setRpcUrls(cleanupIpString(newBlockchain.getRpcUrls()));
        newBlockchain.setRpcCerts(cleanupIpString(newBlockchain.getRpcCerts()));
        return genericDao.put(newBlockchain, null);
    }


    public List<Blockchain> list() {
        return genericDao.getAllByType(Blockchain.class);
    }

    public List<Blockchain> listByConsortium(Consortium consortium) {
        return genericDao.getByParentId(consortium.getConsortiumId(), Blockchain.class);
    }

    public Blockchain get(UUID id) {
        return genericDao.get(id, Blockchain.class);
    }
}
