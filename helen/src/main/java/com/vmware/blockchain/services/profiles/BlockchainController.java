/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vmware.blockchain.common.NoSuchConsortiumException;

import javassist.NotFoundException;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Controller to create and list blockchains.
 */
@RestController
public class BlockchainController {

    @Getter
    @Setter
    private static class BlockchainPost {
        private UUID consortiumId;
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    private static class BlockchainPatch {
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    private static class BlockchainResponse {
        private UUID id;
        private UUID consortiumId;
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    private BlockchainManager manager;
    private ConsortiumRepository cnRepo;

    @Autowired
    public BlockchainController(BlockchainManager manager, ConsortiumRepository cnRepo) {
        this.manager = manager;
        this.cnRepo = cnRepo;
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.GET)
    ResponseEntity<List<BlockchainResponse>> list() {
        List<BlockchainResponse> idList = manager.list().stream().map(b -> new BlockchainResponse(b.getId(),
                b.getConsortium().getConsortiumId(), b.getIpList(), b.getRpcUrls(), b.getRpcCerts()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(idList, HttpStatus.OK);
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.GET)
    ResponseEntity<BlockchainResponse> get(@PathVariable UUID id) throws NotFoundException {
        Optional<Blockchain> oBlockchain = manager.get(id);
        if (oBlockchain.isPresent()) {
            Blockchain b = oBlockchain.get();
            return new ResponseEntity<>(new BlockchainResponse(b.getId(), b.getConsortium().getConsortiumId(),
                    b.getIpList(), b.getRpcUrls(), b.getRpcCerts()), HttpStatus.OK);
        } else {
            throw new NotFoundException(id + " does not exist");
        }
    }

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    public ResponseEntity<Blockchain> createBlockchain(@RequestBody BlockchainPost body) {
        Optional<Consortium> oConsortium = cnRepo.findById(body.consortiumId);
        if (oConsortium.isEmpty()) {
            throw new NoSuchConsortiumException(body.consortiumId + " does not exist");
        }
        Blockchain b = manager.create(oConsortium.get(), body.getIpList(), body.getRpcUrls(), body.getRpcCerts());
        return new ResponseEntity<>(b, HttpStatus.OK);
    }

    /**
     * Update the given blockchain.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.PATCH)
    public ResponseEntity<Blockchain> updateBlockchain(@PathVariable UUID id, @RequestBody BlockchainPatch body)
            throws NotFoundException {
        Optional<Blockchain> oBlockchain = manager.get(id);
        if (oBlockchain.isEmpty()) {
            throw new NotFoundException(id + " was not found");
        }
        Blockchain b = oBlockchain.get();
        if (body.getIpList() != null) {
            b.setIpList(body.getIpList());
        }

        if (body.getRpcUrls() != null) {
            b.setRpcUrls(b.getRpcUrls());
        }

        if (body.getRpcCerts() != null) {
            b.setRpcCerts(body.getRpcCerts());
        }
        return new ResponseEntity<>(manager.update(b), HttpStatus.OK);
    }
}
