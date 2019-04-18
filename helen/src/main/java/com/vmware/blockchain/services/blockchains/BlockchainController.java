/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

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
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Controller to create and list blockchains.
 */
@RestController
public class BlockchainController {

    /**
     * The type of sites we want in the deployment.
     */
    public enum DeploymentType {
        FIXED,
        UNSPECIFIED
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPost {
        private UUID consortiumId;
        private int fCount;
        private int cCount;
        private DeploymentType deploymentType;
        private List<String> sites;
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
    static class BlockchainNodeEntry {
        private UUID nodeId;
        private String ip;
        private String url;
        private String cert;
        private String region;

        public BlockchainNodeEntry(NodeEntry n) {
            nodeId = n.getNodeId();
            ip = n.getIp();
            url = n.getUrl();
            cert = n.getCert();
            region = n.getRegion();
        }

    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class BlockchainGetResponse {
        private UUID id;
        private UUID consortiumId;
        private List<BlockchainNodeEntry> nodeList;

        public BlockchainGetResponse(Blockchain b) {
            this.id = b.getId();
            this.consortiumId = b.getConsortium();
            this.nodeList = b.getNodeList().stream().map(BlockchainNodeEntry::new).collect(Collectors.toList());
        }
    }


    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    static class BlockchainTaskResponse {
        private UUID taskId;
    }

    private BlockchainService manager;
    private ConsortiumService consortiumService;
    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private TaskService taskService;

    @Autowired
    public BlockchainController(BlockchainService manager,
            ConsortiumService consortiumService,
            AuthHelper authHelper,
            DefaultProfiles defaultProfiles,
            TaskService taskService) {
        this.manager = manager;
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.GET)
    ResponseEntity<List<BlockchainGetResponse>> list() {
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
        List<BlockchainGetResponse> idList = chains.stream().map(BlockchainGetResponse::new)
                .collect(Collectors.toList());
        return new ResponseEntity<>(idList, HttpStatus.OK);
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.GET)
    ResponseEntity<BlockchainGetResponse> get(@PathVariable UUID id) throws NotFoundException {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles()) && !authHelper.getPermittedChains().contains(id)) {
            throw new ForbiddenException(id + " Forbidden");
        }
        Blockchain b = manager.get(id);
        BlockchainGetResponse br = new BlockchainGetResponse(b);
        return new ResponseEntity<>(br, HttpStatus.OK);
    }

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    public ResponseEntity<BlockchainTaskResponse> createBlockchain(@RequestBody BlockchainPost body) {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles())) {
            throw new ForbiddenException("Action Forbidden");
        }

        // Temporary: create a completed task that points to the default bockchain
        Task task = new Task();
        task.setState(State.SUCCEEDED);
        task.setMessage("Default Blockchain");
        task.setResourceId(defaultProfiles.getBlockchain().getId());
        task.setResourceLink("/api/blockchains/".concat(defaultProfiles.getBlockchain().getId().toString()));
        task = taskService.put(task);

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

    /**
     * Update the given blockchain.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.PATCH)
    public ResponseEntity<BlockchainTaskResponse> updateBlockchain(@PathVariable UUID id,
            @RequestBody BlockchainPatch body) throws NotFoundException {
        if (!authHelper.hasAnyAuthority(Roles.operatorRoles()) && !authHelper.getPermittedChains().contains(id)) {
            throw new ForbiddenException(id + " Forbidden");
        }
        // Temporary: create a completed task that points to the default bockchain
        Task task = new Task();
        task.setState(State.SUCCEEDED);
        task.setMessage("Default Blockchain");
        task.setResourceId(defaultProfiles.getBlockchain().getId());
        task.setResourceLink("/api/blockchains/".concat(defaultProfiles.getBlockchain().getId().toString()));
        task = taskService.put(task);

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }
}
