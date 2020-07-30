/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.deployment.v1.DeployedResource.DeployedResourcePropertyKey.PRIVATE_IP;
import static com.vmware.blockchain.deployment.v1.DeployedResource.DeployedResourcePropertyKey.PUBLIC_IP;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import com.google.common.base.Strings;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.clients.Client;
import com.vmware.blockchain.services.blockchains.clients.ClientService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.stub.StreamObserver;

/**
 * Stream observer for creating a blockchain cluster.  Need to save the current Auth Context, and update task and
 * blockchain objects as they occur.
 */
public class BlockchainObserver implements StreamObserver<DeploymentExecutionEvent> {

    private static final Logger logger = LogManager.getLogger(BlockchainObserver.class);

    private final Authentication auth;
    private final AuthHelper authHelper;
    private final OperationContext operationContext;

    private final BlockchainService blockchainService;
    private final ReplicaService replicaService;
    private final ClientService clientService;
    private final TaskService taskService;

    private final ConnectionPoolManager connectionPoolManager;

    private final UUID taskId;
    private final String opId;
    private final NodeAssignment nodeAssignment;

    // Used internally
    private Map<String, List<DeployedResource>> deployedResources = new HashMap<>();
    private Map<String, NodeAssignment.Entry> nodeById = new HashMap<>();
    private Blockchain rawBlockchain;
    private DeploymentExecutionEvent.Status status = DeploymentExecutionEvent.Status.UNRECOGNIZED;

    /**
     * Create a new Blockchain Observer.  This handles the callbacks from the deployment process.
     */
    public BlockchainObserver(
            AuthHelper authHelper,
            OperationContext operationContext,
            BlockchainService blockchainService,
            ReplicaService replicaService,
            ClientService clientService,
            TaskService taskService,
            ConnectionPoolManager connectionPoolManager,
            UUID taskId,
            NodeAssignment nodeAssignment,
            Blockchain rawBlockchain) {
        this.authHelper = authHelper;
        this.operationContext = operationContext;
        this.blockchainService = blockchainService;
        this.replicaService = replicaService;
        this.clientService = clientService;
        this.taskService = taskService;
        this.connectionPoolManager = connectionPoolManager;
        this.taskId = taskId;
        auth = SecurityContextHolder.getContext().getAuthentication();
        opId = operationContext.getId();
        this.nodeAssignment = nodeAssignment;
        this.rawBlockchain = rawBlockchain;
        this.nodeAssignment.getEntriesList()
                .forEach(each -> {
                    deployedResources.put(each.getNodeId(), new ArrayList<>());
                    nodeById.put(each.getNodeId(), each);
                });
    }

    @Override
    public void onNext(DeploymentExecutionEvent deploymentExecutionEvent) {
        logger.info("On Next: {}", deploymentExecutionEvent.getType());
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        operationContext.setId(opId);
        String message = "Deployment in progress";
        try {

            // Persist the current state of the task.
            final Task task = taskService.get(taskId);

            // Assumption: ACKNOWLEDGED is first and COMPLETED is last.
            switch (deploymentExecutionEvent.getType()) {
                case ACKNOWLEDGED:
                    logger.info("Type: {}", deploymentExecutionEvent.getType());
                    status = deploymentExecutionEvent.getStatus();

                    rawBlockchain.setId(UUID.fromString(deploymentExecutionEvent.getBlockchainId()));

                    rawBlockchain.getMetadata()
                            .put(Blockchain.BLOCKCHAIN_VERSION,
                                 deploymentExecutionEvent.getResource()
                                         .getAdditionalInfo()
                                         .getValuesOrDefault(DeploymentAttributes.BLOCKCHAIN_VERSION.name(),
                                                             "NA"));

                    rawBlockchain.getMetadata()
                            .put(Blockchain.DAML_SDK_VERSION,
                                 deploymentExecutionEvent.getResource()
                                         .getAdditionalInfo()
                                         .getValuesOrDefault(DeploymentAttributes.DAML_SDK_VERSION.name(),
                                                             "NA"));

                    // TODO Move to sync call, along with id generation.
                    task.setResourceId(rawBlockchain.getId());
                    task.setResourceLink(String.format("/api/blockchains/%s", rawBlockchain.getId()));
                    break;
                case RESOURCE:
                    deployedResources.get(deploymentExecutionEvent.getResource().getNodeId())
                            .add(deploymentExecutionEvent.getResource());
                    break;
                case COMPLETED:
                    status = deploymentExecutionEvent.getStatus();
                    message = String.format("Deployment completed on blockchain %s, status %s", rawBlockchain.getId(),
                                            status);

                    deployedResources.forEach((k, v) -> logger
                            .info("Total number of {} deployed resource events received for node {}.", v.size(), k));

                    break;
                default:
                    logger.error("Invalid stream event {}", deploymentExecutionEvent);
                    break;
            }

            // need a final for the lambda
            final String mes = message;
            // Not clear if we need the merge here,
            taskService.merge(task, m -> {
                // if the latest entry is in completed, don't change anything
                if (m.getState() != Task.State.SUCCEEDED && m.getState() != Task.State.FAILED) {
                    // Otherwise, set the fields
                    m.setMessage(mes);
                }
            });
        } finally {
            SecurityContextHolder.getContext().setAuthentication(null);
            operationContext.removeId();
        }
    }

    @Override
    public void onError(Throwable t) {
        logger.info("On Error", t);
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        operationContext.setId(opId);

        logger.info("Printing all the resources created for the blockchain for cleanup {}", rawBlockchain.getId());
        logger.info(deployedResources);
        // var blockchain = blockchainService.get(blockchainId);
        // blockchain.state = Blockchain.BlockchainState.FAILED;
        // blockchainService.update(blockchain);

        final Task task = taskService.get(taskId);
        task.setState(Task.State.FAILED);
        task.setMessage(t.getMessage());
        taskService.merge(task, m -> {
            m.setState(task.getState());
            m.setMessage(task.getMessage());
        });
        SecurityContextHolder.getContext().setAuthentication(null);
        operationContext.removeId();
    }

    @Override
    public void onCompleted() {
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        operationContext.setId(opId);
        // Just log this.  Looking to see how often this happens.
        logger.info("Task {} completed, status {}", taskId, status);
        // We need to evict the auth token from the cache, since the available blockchains has just changed
        authHelper.evictToken();
        var blockchain = blockchainService.create(rawBlockchain.getId(), rawBlockchain.getConsortium(),
                                                  rawBlockchain.type, rawBlockchain.getMetadata());

        final Task task = taskService.get(taskId);
        task.setMessage("Operation finished");
        if (status == DeploymentExecutionEvent.Status.SUCCESS) {
            task.setState(Task.State.SUCCEEDED);
        } else {
            blockchain.state = Blockchain.BlockchainState.FAILED;
            task.setState(Task.State.FAILED);
        }
        // Validate and convert to blockchain metadata
        try {
            createBlockchainNodeInfo();
        } catch (Exception e) {
            logger.error("Exception saving the nodes", e);
            task.setState(Task.State.FAILED);

            // Important state change.
            blockchain.state = Blockchain.BlockchainState.FAILED;
        }
        blockchain.setRawResourceInfo(deployedResources.entrySet().stream().collect(Collectors.toMap(
            e -> e.getKey(),
            e -> transformDeployedResourceToString(e.getValue())
        )));

        // Backward compatible workflow for ETH.
        try {
            if (blockchain.type == Blockchain.BlockchainType.ETHEREUM) {
                var ips = replicaService.getReplicas(blockchain.getId()).stream().map(r -> r.getPublicIp())
                        .collect(Collectors.toList());
                connectionPoolManager.createPool(blockchain.getId(), ips);
            }
        } catch (IOException e) {
            // Not sure what to do here.  Probably need to throw an error.
            logger.warn("Connection pool creation failed for blockchain {}", blockchain.getId());
        }

        blockchainService.update(blockchain);

        // Persist the finality of the task, success or failure.
        taskService.merge(task, m -> {
            // if the latest entry is in completed, don't change anything
            if (m.getState() != Task.State.SUCCEEDED && m.getState() != Task.State.FAILED) {
                // Otherwise, set the fields
                m.setMessage(task.getMessage());
                m.setResourceId(task.getResourceId());
                m.setResourceLink(task.getResourceLink());
                m.setState(task.getState());
            }
        });

        logger.info("Updated task {}", task);
        SecurityContextHolder.getContext().setAuthentication(null);
        operationContext.removeId();
    }

    private void createBlockchainNodeInfo() {

        deployedResources.entrySet()
                .forEach(entry -> {
                    updateNodeEntry(UUID.fromString(entry.getKey()), entry.getValue());
                });
    }

    private void updateNodeEntry(UUID nodeId, List<DeployedResource> resources) {
        // Assumption that nodetype is fixed.

        var node = nodeById.get(nodeId.toString());
        var nodeType = node.getType();
        var zoneId = UUID.fromString(node.getSite().getId());
        switch (nodeType) {
            case REPLICA:
                Replica replicaNode = new Replica();
                replicaNode.setId(nodeId);
                replicaNode.setZoneId(zoneId);
                replicaNode.setBlockchainId(rawBlockchain.getId());
                resources.forEach(each -> {
                    var attributes = each.getAdditionalInfo().getValuesMap();
                    switch (each.getType()) {
                        case COMPUTE_RESOURCE:
                            replicaNode.setPassword(attributes.get(
                                    DeployedResource.DeployedResourcePropertyKey.NODE_LOGIN.name()));

                            replicaNode.setUrl(attributes.get(DeployedResource
                                                                      .DeployedResourcePropertyKey
                                                                      .CLIENT_ENDPOINT.name()));
                            break;
                        case NETWORK_RESOURCE:
                            if (attributes
                                    .containsKey(PRIVATE_IP.name())) {
                                replicaNode.setPrivateIp(
                                        attributes.get(PRIVATE_IP.name()));
                            }
                            if (attributes.containsKey(PUBLIC_IP.name())) {
                                replicaNode.setPublicIp(
                                        attributes.get(PUBLIC_IP.name()));
                            }
                            break;
                        default:
                            break;
                    }
                });
                replicaNode.setUrl(transformEndpoint(replicaNode));
                replicaService.put(replicaNode);
                break;
            case CLIENT:
                Client clientNode = new Client();
                clientNode.setId(nodeId);
                clientNode.setZoneId(zoneId);
                clientNode.setBlockchainId(rawBlockchain.getId());
                resources.forEach(each -> {
                    var attributes = each.getAdditionalInfo().getValuesMap();

                    // CLIENT_GROUP_ID is available regardless of the resource type.
                    String clientGroupId = attributes.get(NodeProperty.Name.CLIENT_GROUP_ID.name());
                    if (clientGroupId != null && !clientGroupId.isEmpty()) {
                        clientNode.setGroupId(UUID.fromString(clientGroupId));
                    }

                    switch (each.getType()) {
                        case COMPUTE_RESOURCE:
                            clientNode.setPassword(attributes.get(
                                    DeployedResource.DeployedResourcePropertyKey.NODE_LOGIN.name()));

                            clientNode.setAuthJwtUrl(attributes.get(NodeProperty.Name.CLIENT_AUTH_JWT.name()));

                            clientNode.setUrl(attributes.get(DeployedResource
                                                                     .DeployedResourcePropertyKey
                                                                     .CLIENT_ENDPOINT.name()));
                            break;
                        case NETWORK_RESOURCE:
                            if (attributes
                                    .containsKey(PRIVATE_IP.name())) {
                                clientNode.setPrivateIp(
                                        attributes.get(PRIVATE_IP.name()));
                            }
                            if (attributes.containsKey(PUBLIC_IP.name())) {
                                clientNode.setPublicIp(
                                        attributes.get(PUBLIC_IP.name()));
                            }
                            break;
                        default:
                            break;
                    }
                });
                clientNode.setUrl(transformEndpoint(clientNode));
                clientService.put(clientNode);
                break;
            default:
                logger.error("Unidentified node type {}", nodeType);
        }
    }

    // Figure a better way to manage endpoints.
    private String transformEndpoint(NodeInterface nodeInterface) {
        if (Strings.isNullOrEmpty(nodeInterface.getUrl())) {
            return "";
        }
        String ipToUse = Strings.isNullOrEmpty(nodeInterface.getPublicIp()) ? nodeInterface.getPrivateIp()
                                                                            : nodeInterface.getPublicIp();

        // Pattern fixed in PS.
        return nodeInterface.getUrl().replace("{{ip}}", ipToUse);
    }

    private List<String> transformDeployedResourceToString(List<DeployedResource> resources) {
        return resources.stream().map(e -> e.toString()).collect(Collectors.toList());
    }
}
