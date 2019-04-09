/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
import java.util.List;
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
import com.vmware.blockchain.common.ForbiddenException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.security.AuthHelper;

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
    static class BlockchainPost {
        private UUID consortiumId;
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPatch {
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class BlockchainResponse {
        private UUID id;
        private UUID consortiumId;
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;

        public BlockchainResponse(Blockchain b) {
            this.id = b.getId();
            this.consortiumId = b.getConsortium();
            this.ipList = b.getIpList();
            this.rpcUrls = b.getRpcUrls();
            this.rpcCerts = b.getRpcCerts();
        }
    }

    private BlockchainService manager;
    private ConsortiumService consortiumService;
    private AuthHelper authHelper;

    @Autowired
    public BlockchainController(BlockchainService manager,
            ConsortiumService consortiumService,
            AuthHelper authHelper) {
        this.manager = manager;
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.GET)
    ResponseEntity<List<BlockchainResponse>> list() {
        List<Blockchain> chains = Collections.emptyList();
        // if we are operator, we can get all blockchains.
        if (authHelper.hasAnyAuthority(Roles.operatorRoles())) {
            chains = manager.list();
        } else {
            // Otherwise, we can only see our consortium.
            try {
                Consortium c = consortiumService.get(authHelper.getConsortiumId());
                chains = manager.listByConsortium(c);
            } catch (NotFoundException e) {
                // Just ignore
            }
        }
        List<BlockchainResponse> idList = chains.stream().map(b -> new BlockchainResponse(b.getId(),
                b.getConsortium(), b.getIpList(), b.getRpcUrls(), b.getRpcCerts()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(idList, HttpStatus.OK);
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.GET)
    ResponseEntity<BlockchainResponse> get(@PathVariable UUID id) throws NotFoundException {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles()) && !authHelper.getPermittedChains().contains(id)) {
            throw new ForbiddenException(id + " Forbidden");
        }
        Blockchain b = manager.get(id);
        BlockchainResponse br = new BlockchainResponse(b.getId(), b.getConsortium(),
                b.getIpList(), b.getRpcUrls(), b.getRpcCerts());
        return new ResponseEntity<>(br, HttpStatus.OK);
    }

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    public ResponseEntity<BlockchainResponse> createBlockchain(@RequestBody BlockchainPost body) {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles())) {
            throw new ForbiddenException("Action Forbidden");
        }
        Consortium consortium = consortiumService.get(body.consortiumId);
        Blockchain b = manager.create(consortium, body.getIpList(), body.getRpcUrls(), body.getRpcCerts());
        return new ResponseEntity<>(new BlockchainResponse(b), HttpStatus.OK);
    }

    /**
     * Update the given blockchain.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.PATCH)
    public ResponseEntity<BlockchainResponse> updateBlockchain(@PathVariable UUID id, @RequestBody BlockchainPatch body)
            throws NotFoundException {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles()) && !authHelper.getPermittedChains().contains(id)) {
            throw new ForbiddenException(id + " Forbidden");
        }
        Blockchain b = manager.get(id);
        if (body.getIpList() != null) {
            b.setIpList(body.getIpList());
        }

        if (body.getRpcUrls() != null) {
            b.setRpcUrls(body.getRpcUrls());
        }

        if (body.getRpcCerts() != null) {
            b.setRpcCerts(body.getRpcCerts());
        }
        return new ResponseEntity<>(new BlockchainResponse(manager.update(b)), HttpStatus.OK);
    }
}
