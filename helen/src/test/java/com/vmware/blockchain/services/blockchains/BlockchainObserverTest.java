/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.deployment.v1.ConcordCluster;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordClusterInfo;
import com.vmware.blockchain.deployment.v1.ConcordModelIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConcordNode;
import com.vmware.blockchain.deployment.v1.ConcordNodeEndpoint;
import com.vmware.blockchain.deployment.v1.ConcordNodeHostInfo;
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordNodeInfo;
import com.vmware.blockchain.deployment.v1.ConcordNodeStatus;
import com.vmware.blockchain.deployment.v1.DeploymentSession.Status;
import com.vmware.blockchain.deployment.v1.DeploymentSession;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent.Type;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.ProvisionedResource;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.clients.Client;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;

/**
 * Test the blockchain observer.
 */
@ExtendWith(SpringExtension.class)
public class BlockchainObserverTest {

    static final UUID TASK_ID = UUID.fromString("97668112-178b-4ed3-a1ac-84435e0f1f86");
    static final UUID CLUSTER_ID = UUID.fromString("fb212e5a-0428-46f9-8faa-b3f15c9843e8");
    static final UUID ORG_ID = UUID.fromString("7051db6a-9f34-4cfd-8dd4-ae8539025f3e");
    static final UUID CONS_ID = UUID.fromString("7d9bb239-45a3-453c-9b9f-beb2f9e4dffe");
    static final UUID NODE_ID = UUID.fromString("e6ef4604-4029-4080-b621-1efa25000e8a");
    static final List<UUID> C_NODES =
            ImmutableList.of(UUID.fromString("afc3fd3f-0d27-412a-8200-167b9c09343b"),
                             UUID.fromString("57d049eb-caea-411c-94c1-fa8c7de973db"),
                             UUID.fromString("60df6645-38f1-428d-b737-fd37a9cac955"),
                             UUID.fromString("a8303207-af47-4466-b84d-cc90a0ee2be5"));

    @MockBean
    private AuthHelper authHelper;

    @MockBean
    private TaskService taskService;

    @MockBean
    private BlockchainService blockchainService;

    @MockBean
    private ReplicaService replicaService;

    private BlockchainObserver blockchainObserver;

    private Task task;

    private Blockchain blockchain;

    private DeploymentSessionEvent value;

    private OperationContext operationContext;

    @BeforeEach
    void init() throws Exception {
        task = new Task();
        task.setId(TASK_ID);
        task.setState(task.getState().RUNNING);
        when(taskService.get(TASK_ID)).thenReturn(task);
        when(taskService.merge(any(Task.class), any())).thenAnswer(i -> {
            ((Consumer<Task>) i.getArgument(1)).accept(i.getArgument(0));
            return i.getArgument(0);
        });
        when(authHelper.getOrganizationId()).thenReturn(ORG_ID);
        blockchain = new Blockchain(new UUID(1, 2), BlockchainType.ETHEREUM,
                                    Blockchain.BlockchainState.INACTIVE, Collections.emptyList());
        blockchain.setId(CLUSTER_ID);
        when(blockchainService.create(any(UUID.class), any(UUID.class), any(BlockchainType.class), any()))
                .thenAnswer(i -> {
                    blockchain.setId(i.getArgument(0));
                    blockchain.setConsortium(i.getArgument(1));
                    blockchain.setType(i.getArgument(2));
                    blockchain.setNodeList(i.getArgument(3));
                    return blockchain;
                });
        operationContext = new OperationContext();
        operationContext.initId();
        blockchainObserver = new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService,
                                                    taskService, TASK_ID, CONS_ID, null,
                                                    BlockchainType.ETHEREUM,
                                                    Replica.ReplicaType.NONE, Client.ClientType.NONE);
        ConcordCluster cluster = createTestCluster(CLUSTER_ID);
        DeploymentSessionIdentifier deploymentSessionIdentifier = DeploymentSessionIdentifier.newBuilder()
                .setId(UUID.randomUUID().toString())
                .build();
        value = DeploymentSessionEvent.newBuilder()
                .setType(DeploymentSessionEvent.Type.NOOP)
                .setStatus(DeploymentSession.Status.UNKNOWN)
                .setSession(deploymentSessionIdentifier)
                .setResource(ProvisionedResource.newBuilder()
                        .setType(ProvisionedResource.Type.GENERIC)
                        .setName("")
                        .setSite(OrchestrationSiteIdentifier.newBuilder()
                                         .setId(UUID.randomUUID().toString())
                                .build())
                        .setCluster(ConcordClusterIdentifier.newBuilder()
                                            .setId(UUID.randomUUID().toString())
                                .build())
                        .setNode(ConcordNodeIdentifier.newBuilder()
                                         .setId(UUID.randomUUID().toString())
                                .build())
                        .build())
                .setNode(ConcordNode.newBuilder()
                        .setId(ConcordNodeIdentifier.newBuilder()
                                       .setId(UUID.randomUUID().toString())
                                .build())
                        .setInfo(ConcordNodeInfo.newBuilder()
                                .setModel(ConcordModelIdentifier.newBuilder()
                                                  .setId(UUID.randomUUID().toString())
                                        .build())
                                .putAllIpv4Addresses(Collections.emptyMap())
                                .setBlockchainType(ConcordModelSpecification.BlockchainType.ETHEREUM)
                                .build())
                        .setHostInfo(ConcordNodeHostInfo.newBuilder()
                                .setSite(OrchestrationSiteIdentifier.newBuilder()
                                                 .setId(UUID.randomUUID().toString())
                                        .build())
                                .putAllIpv4AddressMap(Collections.emptyMap())
                                .putAllEndpoints(Collections.emptyMap())
                                .build())
                        .build())
                .setNodeStatus(ConcordNodeStatus.newBuilder()
                        .setId(ConcordNodeIdentifier.newBuilder()
                                       .setId(UUID.randomUUID().toString())
                                .build())
                        .setStatus(ConcordNodeStatus.Status.UNKNOWN)
                        .build())
                .setCluster(ConcordCluster.newBuilder()
                        .setId(ConcordClusterIdentifier.newBuilder()
                                       .setId(UUID.randomUUID().toString())
                                .build())
                        .setInfo(ConcordClusterInfo.newBuilder()
                                .addAllMembers(Collections.emptyList())
                                .build())
                        .build())
                .build();
        ReflectionTestUtils.setField(value, "cluster_", cluster);
    }

    @Test
    void deployNodeSuccess() throws Exception {
        ConcordNode node = ConcordNode.newBuilder()
                .setId(ConcordNodeIdentifier.newBuilder()
                        .setId(NODE_ID.toString())
                        .build())
                .setInfo(ConcordNodeInfo.newBuilder().build())
                .setHostInfo(ConcordNodeHostInfo.newBuilder().build())
                .build();
        ConcordNodeStatus nodeStatus = ConcordNodeStatus.newBuilder()
                .setId(node.getId())
                .setStatus(ConcordNodeStatus.Status.ACTIVE)
                .build();
        // sigh.. no setters generated
        ReflectionTestUtils.setField(value, "type_", Type.NODE_DEPLOYED.getNumber());
        ReflectionTestUtils.setField(value, "status_", Status.ACTIVE.getNumber());
        ReflectionTestUtils.setField(value, "node_", node);
        ReflectionTestUtils.setField(value, "nodeStatus_", nodeStatus);
        blockchainObserver.onNext(value);
        Assertions.assertEquals("Node e6ef4604-4029-4080-b621-1efa25000e8a deployed, status ACTIVE", task.getMessage());
    }

    @Test
    void deployClusterSuccess() throws Exception {
        ReflectionTestUtils.setField(value, "type_", Type.CLUSTER_DEPLOYED.getNumber());
        ReflectionTestUtils.setField(value, "status_", Status.SUCCESS.getNumber());
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        Assertions.assertEquals("Cluster fb212e5a-0428-46f9-8faa-b3f15c9843e8 deployed", task.getMessage());
        ReflectionTestUtils.setField(value, "type_", Type.COMPLETED.getNumber());
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        Assertions.assertEquals("Deployment completed on cluster fb212e5a-0428-46f9-8faa-b3f15c9843e8, status SUCCESS",
                                task.getMessage());
        blockchainObserver.onCompleted();
        Assertions.assertEquals(State.SUCCEEDED, task.getState());
        Assertions.assertEquals(CONS_ID, blockchain.getConsortium());
        Assertions.assertEquals(4, blockchain.getNodeList().size());
        Assertions.assertEquals(CLUSTER_ID, blockchain.getId());
        Assertions.assertEquals("/api/blockchains/" + CLUSTER_ID.toString(), task.getResourceLink());
        Assertions.assertEquals(CLUSTER_ID, task.getResourceId());
        ArgumentCaptor<Replica> captor = ArgumentCaptor.forClass(Replica.class);
        // Replica service put should have been called once for each node.
        verify(replicaService, times(4)).put(captor.capture());
        // Make sure the replicas are correct.
        List<Replica> replicas = captor.getAllValues();
        // All the IDs must be the same as in C_LIST
        List<UUID> rIds = replicas.stream().map(r -> r.getId()).collect(Collectors.toList());
        Assertions.assertEquals(C_NODES, rIds);
        // And make sure the blockchain id, public and private ips are correct
        Assertions.assertTrue(replicas.stream()
                                      .allMatch(r -> CLUSTER_ID.equals(r.getBlockchainId())
                                      && "0.0.0.1".equals(r.getPublicIp())
                                      && "0.0.0.2".equals(r.getPrivateIp())));
    }

    @Test
    void deployClusterFailure() throws Exception {
        ReflectionTestUtils.setField(value, "type_", Type.CLUSTER_DEPLOYED.getNumber());
        ReflectionTestUtils.setField(value, "status_", Status.FAILURE.getNumber());
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        ReflectionTestUtils.setField(value, "type_", Type.COMPLETED.getNumber());
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        blockchainObserver.onCompleted();
        Assertions.assertEquals(State.FAILED, task.getState());
    }

    @Test
    void testOnError() throws Exception {
        final HelenException e = new HelenException(HttpStatus.BAD_REQUEST, "This is a bad request");
        ReflectionTestUtils.setField(value, "type_", Type.CLUSTER_DEPLOYED.getNumber());
        ReflectionTestUtils.setField(value, "status_", Status.FAILURE.getNumber());
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        blockchainObserver.onError(e);
        Assertions.assertEquals(State.FAILED, task.getState());
        Assertions.assertEquals("This is a bad request", task.getMessage());
    }

    private ConcordCluster createTestCluster(UUID clusterId) {
        ConcordClusterIdentifier id = ConcordClusterIdentifier.newBuilder()
                .setId(clusterId.toString())
                .build();
        List<ConcordNode> nodes = C_NODES.stream().map(this::createNode).collect(Collectors.toList());
        ConcordClusterInfo info = ConcordClusterInfo.newBuilder()
                .addAllMembers(nodes)
                .build();
        return ConcordCluster.newBuilder()
                .setId(id)
                .setInfo(info)
                .build();
    }

    private ConcordNode createNode(UUID nodeId) {
        UUID mId = UUID.randomUUID();
        ConcordModelIdentifier modeId = ConcordModelIdentifier.newBuilder()
                .setId(mId.toString())
                .build();

        ConcordNodeInfo concordNodeInfo = ConcordNodeInfo.newBuilder()
                .setModel(modeId)
                .putAllIpv4Addresses(Collections.emptyMap())
                .setBlockchainType(ConcordModelSpecification.BlockchainType.ETHEREUM)
                .build();

        ConcordNodeIdentifier nId = ConcordNodeIdentifier.newBuilder()
                .setId(nodeId.toString())
                .build();

        return ConcordNode.newBuilder()
                .setId(nId)
                .setInfo(concordNodeInfo)
                .setHostInfo(createNodeHostInfo())
                .build();
    }

    private ConcordNodeHostInfo createNodeHostInfo() {
        OrchestrationSiteIdentifier oId = OrchestrationSiteIdentifier.newBuilder()
                .setId(UUID.randomUUID().toString())
                .build();
        Map<Integer, Integer>  ipMap = ImmutableMap.of(1, 2);
        ConcordNodeEndpoint endpoint = ConcordNodeEndpoint.newBuilder()
                .setUrl("http://localhost")
                .setCertificate("cert")
                .build();
        Map<String, ConcordNodeEndpoint> m = ImmutableMap.of("ethereum-rpc", endpoint);
        return ConcordNodeHostInfo.newBuilder()
                .setSite(oId)
                .putAllIpv4AddressMap(ipMap)
                .putAllEndpoints(m)
                .build();
    }
}
