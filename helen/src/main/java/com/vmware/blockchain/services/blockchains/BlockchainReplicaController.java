/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.common.fleetmanagment.FleetUtils.identifier;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.model.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.model.FleetManagementServiceStub;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.ServiceState;
import com.vmware.blockchain.deployment.model.UpdateInstanceRequest;
import com.vmware.blockchain.deployment.model.UpdateInstanceResponse;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.BlockchainController.BlockchainTaskResponse;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.tasks.ITaskService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.stub.StreamObserver;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Controller to handle node commands, currently just start/stop.
 */
@RestController
@RequestMapping(path = "/api/blockchains/{bid}")
public class BlockchainReplicaController {
    static Logger logger = LogManager.getLogger(BlockchainReplicaController.class);

    private BlockchainService blockchainService;
    private ConsortiumService consortiumService;
    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private ITaskService taskService;
    private FleetManagementServiceStub client;
    private OperationContext operationContext;
    private boolean mockDeployment;

    @Autowired
    public BlockchainReplicaController(BlockchainService blockchainService,
                                       ConsortiumService consortiumService,
                                       AuthHelper authHelper,
                                       DefaultProfiles defaultProfiles,
                                       TaskService taskService,
                                       FleetManagementServiceStub client,
                                       OperationContext operationContext,
                                       @Value("${mock.deployment:false}") boolean mockDeployment) {
        this.blockchainService = blockchainService;
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
        this.client = client;
        this.operationContext = operationContext;
        this.mockDeployment = mockDeployment;
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

    // ReplicaObserver.  This is the callback from GRPC when starting/stoping nodes.  Package private for testing.
    static class ReplicaObserver implements StreamObserver<UpdateInstanceResponse> {
        private UUID taskId;
        private ITaskService taskService;
        private Authentication auth;
        private OperationContext operationContext;
        private String opId;

        public ReplicaObserver(UUID taskId, ITaskService taskService, OperationContext operationContext) {
            this.taskId = taskId;
            this.taskService = taskService;
            this.operationContext = operationContext;
            auth = SecurityContextHolder.getContext().getAuthentication();
            opId = operationContext.getId();
        }

        @Override
        public void onNext(UpdateInstanceResponse value) {
            // Not really much to do here
        }

        @Override
        public void onError(Throwable t) {
            SecurityContextHolder.getContext().setAuthentication(auth);
            operationContext.setId(opId);
            try {
                logger.info("Error during stop/start", t);
                Task task = taskService.get(taskId);
                task.setState(State.FAILED);
                task.setMessage(t.getMessage());
                taskService.put(task);
            } finally {
                SecurityContextHolder.getContext().setAuthentication(null);
                operationContext.removeId();
            }
        }

        @Override
        public void onCompleted() {
            SecurityContextHolder.getContext().setAuthentication(auth);
            operationContext.setId(opId);
            try {
                logger.info("Start/stop completed");
                Task task = taskService.get(taskId);
                task.setState(State.SUCCEEDED);
                task.setMessage("Operation Complete");
                taskService.put(task);
            } finally {
                SecurityContextHolder.getContext().setAuthentication(null);
                operationContext.removeId();
            }
        }
    }

    // map our actions to the FleetService actions
    private final Map<NodeAction, ServiceState> actionMap =
            ImmutableMap.of(NodeAction.START, new ServiceState(ServiceType.CONCORD, ServiceState.State.ACTIVE),
                            NodeAction.STOP, new ServiceState(ServiceType.CONCORD, ServiceState.State.INACTIVE));

    /**
     * Perform the start/stop action on a single node.
     * @param bid       blockchain ID.
     * @param nodeId    node ID.
     * @param action    start/stop
     * @return          202 with Task
     * @throws Exception BadRequest if paramenter are wrong
     */
    @RequestMapping(method = RequestMethod.POST, path = {"/nodes/{nodeId}", "/replicas/{nodeId}"})
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainTaskResponse> nodeAction(@PathVariable UUID bid,
                                                             @PathVariable UUID nodeId,
                                                             @RequestParam NodeAction action) throws Exception {
        Blockchain b = blockchainService.get(bid);
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
     * @throws Exception BadReqest
     */
    @RequestMapping(method = RequestMethod.POST, path = {"/nodes", "/replicas"})
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<TaskList> nodeListAction(@PathVariable UUID bid,
                                                   @RequestBody NodeList nodeList,
                                                   @RequestParam NodeAction action) throws Exception {
        Blockchain b = blockchainService.get(bid);

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


    private Task startStopNode(UUID bid, UUID nodeId, NodeAction action) {
        final UpdateInstanceRequest request =
                new UpdateInstanceRequest(
                        new MessageHeader(),
                        identifier(ConcordClusterIdentifier.class, bid),
                        identifier(ConcordNodeIdentifier.class, nodeId),
                        Collections.singletonList(actionMap.get(action)));
        // make sure the string is start or stop
        Task task = new Task();
        task.setState(State.RUNNING);
        task.setResourceId(bid);
        task.setResourceLink(String.format("/api/blockchains/%s", bid));
        task = taskService.put(task);
        ReplicaObserver replicaObserver = new ReplicaObserver(task.getId(), taskService, operationContext);
        client.updateInstance(request, replicaObserver);
        return task;
    }


}
