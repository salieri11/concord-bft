/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.services.profiles.BlockchainManagerEvent.Action;


/**
 * Manage all persistence for Blockchain entities.
 */
@Component
public class BlockchainManager {

    private BlockchainRepository blockchainRepo;
    private ApplicationEventPublisher publisher;

    @Autowired
    public BlockchainManager(BlockchainRepository blockchainRepo, ApplicationEventPublisher publisher) {
        this.blockchainRepo = blockchainRepo;
        this.publisher = publisher;
    }

    // Normalize the IP list: split into IPs, sort, and reassemble
    private String normalizeIpList(String ipList) {
        // split on comma, remove leading and trailing whitespace.
        String[] ips = ipList.trim().split("\\s*,\\s*");
        Arrays.sort(ips);
        return String.join(",", ips);
    }

    /**
     * Create a new blockchain with the given ip list.
     * @param ipList List of IPs for the concord nodes
     * @return Blockchain entity
     */
    public Blockchain create(String ipList) {
        Blockchain b = new Blockchain();
        b.setIpList(normalizeIpList(ipList));
        return blockchainRepo.save(b);
    }

    public Blockchain update(Blockchain newBlockchain) {
        newBlockchain.setIpList(normalizeIpList(newBlockchain.getIpList()));
        return blockchainRepo.save(newBlockchain);
    }

    /**
     * Add a node to the existing IP addresses.
     */
    public Blockchain addNode(UUID id, String ip) {
        Optional<Blockchain> oBlockchain = blockchainRepo.findById(id);
        if (!oBlockchain.isPresent()) {
            throw new EntityModificationException("No blockchain found with ID: " + id.toString());
        }
        Blockchain newBlockchain = oBlockchain.get();
        String newIpList = newBlockchain.getIpList() + "," + ip;
        newBlockchain.setIpList(normalizeIpList(newIpList));
        Blockchain blockchain = blockchainRepo.save(newBlockchain);
        publisher.publishEvent(new BlockchainManagerEvent(this, blockchain, Action.ADD_NODE, ip));
        return blockchain;
    }

    /**
     * Delete a node from the blockchain.
     */
    public Blockchain deleteNode(UUID id, String ip) {
        Optional<Blockchain> oBlockchain = blockchainRepo.findById(id);
        if (!oBlockchain.isPresent()) {
            throw new EntityModificationException("No blockchain found with ID: " + id.toString());
        }
        Blockchain newBlockchain = oBlockchain.get();
        // get a copy that we can modify
        List<String> ips = new LinkedList<>(newBlockchain.getIpAsList());
        if (!ips.contains(ip)) {
            throw new EntityModificationException("No such node in blockchain: " + ip);
        }
        ips.remove(ip);
        Collections.sort(ips);
        newBlockchain.setIpAsList(ips);
        Blockchain blockchain = blockchainRepo.save(newBlockchain);
        publisher.publishEvent(new BlockchainManagerEvent(this, blockchain, Action.DELETE_NODE, ip));
        return blockchain;
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
