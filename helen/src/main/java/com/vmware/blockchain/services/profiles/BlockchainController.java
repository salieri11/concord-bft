/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Collections;
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
            this.consortiumId = b.getConsortium().getConsortiumId();
            this.ipList = b.getIpList();
            this.rpcUrls = b.getRpcUrls();
            this.rpcCerts = b.getRpcCerts();
        }
    }

    private BlockchainManager manager;
    private ConsortiumRepository cnRepo;
    private AuthHelper authHelper;

    @Autowired
    public BlockchainController(BlockchainManager manager, ConsortiumRepository cnRepo,
            AuthHelper authHelper) {
        this.manager = manager;
        this.cnRepo = cnRepo;
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
            Optional<Consortium> c = cnRepo.findById(UUID.fromString(authHelper.getConsortiumId()));
            if (c.isPresent()) {
                chains = manager.listByConsortium(c.get());
            }
        }
        List<BlockchainResponse> idList = chains.stream().map(b -> new BlockchainResponse(b.getId(),
                b.getConsortium().getConsortiumId(), b.getIpList(), b.getRpcUrls(), b.getRpcCerts()))
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
        Optional<Blockchain> oBlockchain = manager.get(id);
        if (oBlockchain.isPresent()) {
            Blockchain b = oBlockchain.get();
            BlockchainResponse br = new BlockchainResponse(b.getId(), b.getConsortium().getConsortiumId(),
                    b.getIpList(), b.getRpcUrls(), b.getRpcCerts());
            return new ResponseEntity<>(br, HttpStatus.OK);
        } else {
            throw new NotFoundException(id + " does not exist");
        }
    }

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    public ResponseEntity<BlockchainResponse> createBlockchain(@RequestBody BlockchainPost body) {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles())) {
            throw new ForbiddenException("Action Forbidden");
        }
        Optional<Consortium> oConsortium = cnRepo.findById(body.consortiumId);
        if (oConsortium.isEmpty()) {
            throw new NotFoundException("Consortium {0} does not exist", body.consortiumId);
        }
        Blockchain b = manager.create(oConsortium.get(), body.getIpList(), body.getRpcUrls(), body.getRpcCerts());
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
        return new ResponseEntity<>(new BlockchainResponse(manager.update(b)), HttpStatus.OK);
    }
}
