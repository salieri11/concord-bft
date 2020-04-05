/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
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
     * Create a new blockchain with the parameters and a specified UUID.
     * Use this call when all we know about the consortium is its Id.
     *
     * @param id            Preset UUID for this blockchain
     * @param consortiumId  ID of consortium owning this blockchain
     * @param nodeList      List of node entries
     * @return Blockchain   Blockchain entity
     */
    public Blockchain create(UUID id, UUID consortiumId, BlockchainType type, List<NodeEntry> nodeList) {
        Blockchain b = new Blockchain.BlockchainBuilder()
                .consortium(consortiumId)
                .type(type)
                .nodeList(nodeList)
                .build();
        b.setId(id);
        b.setState(Blockchain.BlockchainState.ACTIVE);
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
     * Create a new blockchain with the parameters and a specified UUID.
     *
     * @param id            Preset UUID for this blockchain
     * @param consortium    Consortium owning this blockchain
     * @param nodeList      List of node entries
     * @return Blockchain   Blockchain entity
     */
    public Blockchain create(UUID id, Consortium consortium, List<NodeEntry> nodeList) {
        return create(id, consortium.getId(), BlockchainType.ETHEREUM, nodeList);
    }

    /**
     * Create a new blockchain with the parameters.
     *
     * @param nodeList      List of node entries
     * @return Blockchain   Blockchain entity
     */
    public Blockchain create(Consortium consortium, List<NodeEntry> nodeList) {
        return create(null, consortium, nodeList);
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
        UUID zoneId = UUID.randomUUID();
        for (int i = 0; i < ips.length; i++) {
            NodeEntry n = new NodeEntry();
            n.setNodeId(UUID.randomUUID());
            n.setIp(ips[i]);
            // If we have a url, split at "=".  First part is host name, last part is url or cert
            String[] urlParts = i >= urls.length ? new String[2] : urls[i].split("=");
            n.setHostName(urlParts[0]);
            n.setUrl(urlParts[1]);
            String certFileName = i >= certs.length ? "" : certs[i].split("=")[1];
            String cert = readCertFile(certFileName);
            n.setCert(cert);
            n.setZoneId(zoneId);
            entries.add(n);
        }
        return create(consortium, entries);
    }

    /**
     * Read the certificate file, for inclusion in the JSON response. If the argument is null, or any error occurs,
     * return an empty string.
     */
    private String readCertFile(String filename) {
        if (filename == null) {
            return "";
        }

        try {
            byte[] certBytes = Files.readAllBytes(FileSystems.getDefault().getPath(filename));
            return new String(certBytes, StandardCharsets.UTF_8);
        } catch (IOException e) {
            logger.warn("Problem reading cert file '{}'", filename);
            return "";
        }
    }

    /**
     * Create a new blockchain from an old one.  Copy the urls and iplist.
     */
    public Blockchain update(Blockchain newBlockchain) {
        newBlockchain.setNodeList(newBlockchain.getNodeList());
        return genericDao.put(newBlockchain, null);
    }

    public Blockchain put(Blockchain b) {
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

    public List<Replica> getReplicas(UUID id) {
        return genericDao.getByParentId(id, Replica.class);
    }
}
