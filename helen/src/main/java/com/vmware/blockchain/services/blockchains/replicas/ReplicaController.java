/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.replicas;

import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainTaskResponse;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.assertj.core.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.concord.ConcordService;
import com.vmware.blockchain.services.models.NodeGetCredentialsResponse;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.tasks.ITaskService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;
import com.vmware.concord.Concord.Peer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Controller to handle node commands, currently just start/stop.
 */
@RestController
@RequestMapping(path = "/api/blockchains/{bid}")
public class ReplicaController {
    static Logger logger = LogManager.getLogger(ReplicaController.class);

    private BlockchainService blockchainService;
    private ReplicaService replicaService;
    private ConsortiumService consortiumService;
    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private ITaskService taskService;
    private OperationContext operationContext;
    private ConcordService concordService;

    @Autowired
    public ReplicaController(BlockchainService blockchainService,
                             ReplicaService replicaService,
                             ConsortiumService consortiumService,
                             AuthHelper authHelper,
                             DefaultProfiles defaultProfiles,
                             TaskService taskService,
                             OperationContext operationContext,
                             ConcordService concordService) {
        this.blockchainService = blockchainService;
        this.replicaService = replicaService;
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
        this.operationContext = operationContext;
        this.concordService = concordService;
    }

    @Data
    static class NodeList {
        private List<UUID> nodeIds;
        private List<UUID> replicaIds;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    static class TaskList {
        private List<UUID> taskIds;
    }

    enum NodeAction {
        START,
        STOP;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    static class ReplicaGetResponse {
        UUID id;
        String name;
        String publicIp;
        String privateIp;
        String rpcUrl;
        String status;
        long millisSinceLastMessage;
        long millisSinceLastMessageThreshold;
        String certificate;
        UUID zoneId;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    static class ReplicaGetCredentialsResponse {
        // TODO: username is a placeholder if it ever needs to be non-root.
        String username;
        String password;
    }

    /**
     * Get the list of replicas, and their status.
     */
    @RequestMapping(method = RequestMethod.GET, path = {"/replicas"})
    @PreAuthorize("@authHelper.canAccessChain(#bid)")
    public ResponseEntity<Collection<ReplicaGetResponse>> getReplicas(@PathVariable UUID bid) throws NotFoundException {
        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }
        List<Replica> replicas = replicaService.getReplicas(bid);
        if (replicas.isEmpty()) {
            // empty replica map is either old Blockchain instance, or blockchain not found.
            throw new NotFoundException(ErrorCode.NOT_FOUND);
        } else {
            // Temporary work around.
            replicas = replicas.stream()
                    .filter(replica -> replica.getReplicaType() != Replica.ReplicaType.DAML_PARTICIPANT)
                    .collect(Collectors.toList());
        }

        Blockchain bc = blockchainService.get(bid);

        // Store the results in a tree map, so the order is always the same.
        // Sort (arbitrarily) by hostname
        SortedMap<String, ReplicaGetResponse> response = new TreeMap<>();

        if (bc.getType() == Blockchain.BlockchainType.DAML) {
            Integer i = 0;
            for (Replica each : replicas) {
                String replicaName = (Strings.isNullOrEmpty(each.getHostName()) ? "Committer" : each.getHostName())
                                     + "" + i++;
                response.put(replicaName, ReplicaGetResponse.builder()
                        .id(each.getId())
                        .name(replicaName)
                        .privateIp(each.privateIp)
                        .publicIp(each.publicIp)
                        .rpcUrl(each.getUrl())
                        .zoneId(each.getZoneId())
                        .millisSinceLastMessage(0)
                        .millisSinceLastMessageThreshold(1)
                        .status("live")
                        .build());
            }
        } else if (bc.getType() == Blockchain.BlockchainType.ETHEREUM) {
            // TODO do this check based on Site Type.
            List<Peer> peers = new ArrayList<>();
            try {
                peers = concordService.getMembers(bid);
            } catch (Exception e) {
                logger.warn("Unable to get concord connection for blockchain {]", bid);
            }

            Map<String, Peer> ipToPeer =
                    peers.stream().collect(Collectors.toMap(p -> p.getAddress().split(":")[0], Function.identity()));

            int i = 0;
            for (Replica each : replicas) {
                String replicaName = (Strings.isNullOrEmpty(each.getHostName()) ? "Committer" : each.getHostName())
                                     + "" + i++;
                Peer value = ipToPeer.get(each.getPublicIp());
                response.put(replicaName, ReplicaGetResponse.builder()
                        .id(each.getId())
                        .name(replicaName)
                        .privateIp(each.privateIp)
                        .publicIp(each.publicIp)
                        .rpcUrl(each.getUrl())
                        .zoneId(each.getZoneId())
                        .millisSinceLastMessage(value != null ? value.getMillisSinceLastMessage() : 0)
                        .millisSinceLastMessageThreshold(value != null ? value.getMillisSinceLastMessageThreshold() : 1)
                        .status(value != null ? value.getStatus() : "live")
                        .build());
            }
        }
        return new ResponseEntity<>(response.values(), HttpStatus.OK);
    }

    /**
     * Perform the start/stop action on a single node.
     * @param bid       blockchain ID.
     * @param nodeId    node ID.
     * @param action    start/stop
     * @return          202 with Task
     * @throws Exception BadRequest if parameters are wrong
     */
    @RequestMapping(method = RequestMethod.POST, path = {"/nodes/{nodeId}", "/replicas/{nodeId}"})
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainTaskResponse> nodeAction(@PathVariable UUID bid,
                                                             @PathVariable UUID nodeId,
                                                             @RequestParam NodeAction action) throws Exception {
        // TODO: Replica Start/Stop POST is not currently in use and does not function.

        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }
        // make sure we can access this node.
        if (b.getNodeList().stream().noneMatch(n -> n.getNodeId().equals(nodeId))) {
            throw new BadRequestException(ErrorCode.INVALID_NODE, nodeId);
        }
        Task task = startStopNode(bid, nodeId, action);
        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

    /**
     * Perform the action a the list of nodes.
     * @param bid       Blockchain ID
     * @param nodeList  List of node ids.
     * @param action    start/stop
     * @return          202 with TaskList
     * @throws Exception BadRequest
     */
    @RequestMapping(method = RequestMethod.POST, path = {"/nodes", "/replicas"})
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<TaskList> nodeListAction(@PathVariable UUID bid,
                                                   @RequestBody NodeList nodeList,
                                                   @RequestParam NodeAction action) throws Exception {
        // TODO: Replica Start/Stop POST is not currently in use and does not function.

        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }

        // check that all the nodes are part of this blockchain
        List<UUID> uuidList = nodeList.getReplicaIds() != null ? nodeList.getReplicaIds() : nodeList.getNodeIds();
        if (uuidList == null) {
            throw new BadRequestException((ErrorCode.BAD_REQUEST));
        }
        List<UUID> bcNodeList = b.getNodeList().stream().map(n -> n.getNodeId()).collect(Collectors.toList());
        if (!bcNodeList.containsAll(uuidList)) {
            throw new BadRequestException(ErrorCode.INVALID_NODE, nodeList.nodeIds);
        }
        List<UUID> taskIds =
                uuidList.stream().map(n -> startStopNode(bid, n, action)).map(t -> t.getId()).collect(
                Collectors.toList());
        return new ResponseEntity<>(new TaskList(taskIds), HttpStatus.ACCEPTED);
    }

    /**
     * Get credentials for a given replica.
     * @param bid       Blockchain ID
     * @param replicaId Replica ID for which credentials are requested
     * @return          200 with ReplicaGetCredentialsResponse
     */
    @RequestMapping(method = RequestMethod.GET, path = {"/replicas/{replicaId}/credentials"})
    @PreAuthorize("@authHelper.isUserConsortiumAdminForBlockchain(#bid)")
    public ResponseEntity<NodeGetCredentialsResponse> getReplicaCredentials(@PathVariable UUID bid,
                                                                            @PathVariable UUID replicaId) {
        Blockchain b = blockchainService.get(bid);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }




        Optional<Replica> replicaOpt = replicaService.getReplicas(bid).stream()
                                                        .filter(r -> r.getId().equals(replicaId)).findFirst();
        if (replicaOpt.isEmpty()) {
            throw new NotFoundException(ErrorCode.NOT_FOUND);
        }
        Replica replica = replicaOpt.get();

        return new ResponseEntity<>(NodeGetCredentialsResponse.builder()
                                        .username("root").password(replica.password).build(), HttpStatus.OK);
    }


    private Task startStopNode(UUID bid, UUID nodeId, NodeAction action) {

        // TODO Implement the functionality when the messaging platform is ready.
        // make sure the string is start or stop
        Task task = new Task();
        task.setState(State.RUNNING);
        task.setResourceId(bid);
        task.setResourceLink(String.format("/api/blockchains/%s", bid));
        task = taskService.put(task);
        return task;
    }
}