/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.deployment.model.ConcordCluster;
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.model.ConcordClusterInfo;
import com.vmware.blockchain.deployment.model.ConcordModelIdentifier;
import com.vmware.blockchain.deployment.model.ConcordNode;
import com.vmware.blockchain.deployment.model.ConcordNodeEndpoint;
import com.vmware.blockchain.deployment.model.ConcordNodeHostInfo;
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.model.ConcordNodeInfo;
import com.vmware.blockchain.deployment.model.DeploymentSession.Status;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent.Type;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
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

    @MockBean
    private AuthHelper authHelper;

    @MockBean
    private TaskService taskService;

    @MockBean
    private BlockchainService blockchainService;

    private BlockchainObserver blockchainObserver;

    private Task task;

    private Blockchain blockchain;

    private DeploymentSessionEvent value;

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
        blockchain = new Blockchain(new UUID(1, 2), Collections.emptyList());
        blockchain.setId(CLUSTER_ID);
        when(blockchainService.create(any(UUID.class), any(UUID.class), any()))
                .thenAnswer(i -> {
                    blockchain.setId(i.getArgument(0));
                    blockchain.setConsortium(i.getArgument(1));
                    blockchain.setNodeList(i.getArgument(2));
                    return blockchain;
                });

        blockchainObserver = new BlockchainObserver(authHelper, blockchainService, taskService, TASK_ID, CONS_ID);
        ConcordCluster cluster = createTestCluster(CLUSTER_ID);
        value = new DeploymentSessionEvent();
        ReflectionTestUtils.setField(value, "cluster", cluster);
    }

    @Test
    void deployNodeSuccess() throws Exception {
        ReflectionTestUtils.setField(value, "type", Type.NODE_DEPLOYED);
        ReflectionTestUtils.setField(value, "status", Status.SUCCESS);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(Type.NODE_DEPLOYED.name(), task.getMessage());
    }

    @Test
    void deployClusterSuccess() throws Exception {
        ReflectionTestUtils.setField(value, "type", Type.CLUSTER_DEPLOYED);
        ReflectionTestUtils.setField(value, "status", Status.SUCCESS);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        ReflectionTestUtils.setField(value, "type", Type.COMPLETED);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        blockchainObserver.onCompleted();
        Assertions.assertEquals(State.SUCCEEDED, task.getState());
        Assertions.assertEquals(CONS_ID, blockchain.getConsortium());
        Assertions.assertEquals(4, blockchain.getNodeList().size());
        Assertions.assertEquals(CLUSTER_ID, blockchain.getId());
        Assertions.assertEquals("/api/blockchains/" + CLUSTER_ID.toString(), task.getResourceLink());
        Assertions.assertEquals(CLUSTER_ID, task.getResourceId());
    }

    @Test
    void deployClusterFailure() throws Exception {
        ReflectionTestUtils.setField(value, "type", Type.CLUSTER_DEPLOYED);
        ReflectionTestUtils.setField(value, "status", Status.FAILURE);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        ReflectionTestUtils.setField(value, "type", Type.COMPLETED);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        blockchainObserver.onCompleted();
        Assertions.assertEquals(State.FAILED, task.getState());
    }

    @Test
    void testOnError() throws Exception {
        final HelenException e = new HelenException(HttpStatus.BAD_REQUEST, "This is a bad request");
        ReflectionTestUtils.setField(value, "type", Type.CLUSTER_DEPLOYED);
        ReflectionTestUtils.setField(value, "status", Status.FAILURE);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        blockchainObserver.onError(e);
        Assertions.assertEquals(State.FAILED, task.getState());
        Assertions.assertEquals("This is a bad request", task.getMessage());
    }

    private ConcordCluster createTestCluster(UUID clusterId) {
        ConcordClusterIdentifier id =
                new ConcordClusterIdentifier(CLUSTER_ID.getLeastSignificantBits(), CLUSTER_ID.getMostSignificantBits());
        List<ConcordNode> nodes = Stream.of(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
                .map(this::createNode).collect(Collectors.toList());
        ConcordClusterInfo info = new ConcordClusterInfo(nodes);
        return new ConcordCluster(id, info);
    }

    private ConcordNode createNode(UUID nodeId) {
        UUID mId = UUID.randomUUID();
        ConcordModelIdentifier modeId =
                new ConcordModelIdentifier(mId.getLeastSignificantBits(), mId.getMostSignificantBits());
        ConcordNodeInfo concordNodeInfo = new ConcordNodeInfo(modeId, Collections.emptyMap());
        ConcordNodeIdentifier nId
                = new ConcordNodeIdentifier(nodeId.getLeastSignificantBits(), nodeId.getMostSignificantBits());
        return new ConcordNode(nId, concordNodeInfo, createNodeHostInfo());
    }

    private ConcordNodeHostInfo createNodeHostInfo() {
        OrchestrationSiteIdentifier oId = new OrchestrationSiteIdentifier(1, 2);
        Map<Integer, Integer>  ipMap = ImmutableMap.of(1, 2);
        ConcordNodeEndpoint endpoint = new ConcordNodeEndpoint("http://localhost", "cert");
        Map<String, ConcordNodeEndpoint> m = ImmutableMap.of("ethereum-rpc", endpoint);
        return new ConcordNodeHostInfo(oId, ipMap, m);
    }
}
