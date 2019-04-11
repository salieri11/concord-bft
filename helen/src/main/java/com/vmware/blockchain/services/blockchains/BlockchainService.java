/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.profiles.Consortium;


/**
 * Manage all persistence for Blockchain entities.
 */
@Service
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

    /**
     * Create a new blockchain with the parameters.
     *
     * @param nodeList       List of node entries
     * @return Blockchain   Blockchain entity
     */
    public Blockchain create(Consortium consortium, List<NodeEntry> nodeList) {
        Blockchain b = new Blockchain.BlockchainBuilder()
                .consortium(consortium.getId())
                .nodeList(nodeList)
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
     * Create a new blockchain with String parameters.  Used in default profiles,
     * and should probably go away at some point.
     *
     * @param ipList        String ist of IPs for the concord nodes
     * @param rpcUrls       String map of hostname, url to ethRpc nodes
     * @param rpcCerts      String map of hostname, cert for SSL to rpc nodes
     */
    public Blockchain create(Consortium consortium, String ipList, String rpcUrls, String rpcCerts) {
        // use magic google Splitter to split, trim and convert to Lists and Maps.
        String[] ips = ipList.split(",");
        String[] urls = rpcUrls.split(",");
        String[] certs = rpcCerts.split(",");
        List<NodeEntry> entries = new ArrayList<>();
        for (int i = 0; i < ips.length; i++) {
            NodeEntry n = new NodeEntry();
            n.setNodeId(UUID.randomUUID());
            n.setIp(ips[i]);
            // If we have a url, split at = and take the last part
            n.setUrl(i >= urls.length ? "" : urls[i].split("=")[1]);
            n.setCert(i >= certs.length ? "" : certs[i].split("=")[1]);
            entries.add(n);
        }
        return create(consortium, entries);
    }

    /**
     * Create a new blockchain from an old one.  Copy the urls and iplist.
     */
    public Blockchain update(Blockchain newBlockchain) {
        newBlockchain.setNodeList(newBlockchain.getNodeList());
        return genericDao.put(newBlockchain, null);
    }


    public List<Blockchain> list() {
        return genericDao.getAllByType(Blockchain.class);
    }

    public List<Blockchain> listByConsortium(Consortium consortium) {
        return genericDao.getByParentId(consortium.getId(), Blockchain.class);
    }

    public Blockchain get(UUID id) {
        return genericDao.get(id, Blockchain.class);
    }
}
