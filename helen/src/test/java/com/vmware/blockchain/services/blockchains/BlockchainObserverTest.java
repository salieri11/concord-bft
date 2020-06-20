/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent.Status;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent.Type;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import com.vmware.blockchain.services.blockchains.clients.ClientService;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
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

    @MockBean
    private ClientService clientService;

    @MockBean
    private ConnectionPoolManager connectionPoolManager;

    private BlockchainObserver blockchainObserver;

    private Task task;

    private Blockchain blockchain;

    private DeploymentExecutionEvent value;

    private OperationContext operationContext;

    @BeforeEach
    void init() throws Exception {
        task = new Task();
        task.setId(TASK_ID);
        task.setState(task.getState().RUNNING);

        HashMap<String, String> metadata = new HashMap<>();
        metadata.put("concord", "1.0");

        when(taskService.get(TASK_ID)).thenReturn(task);
        when(taskService.merge(any(Task.class), any())).thenAnswer(i -> {
            ((Consumer<Task>) i.getArgument(1)).accept(i.getArgument(0));
            return i.getArgument(0);
        });
        when(authHelper.getOrganizationId()).thenReturn(ORG_ID);
        blockchain = new Blockchain(new UUID(1, 2), BlockchainType.ETHEREUM,
                                    Blockchain.BlockchainState.INACTIVE, Collections.emptyList(), metadata,
                                    null);
        blockchain.setId(CLUSTER_ID);
        when(blockchainService.create(any(), any(), any(), any()))
                .thenAnswer(i -> {
                    blockchain.setId(i.getArgument(0));
                    blockchain.setConsortium(i.getArgument(1));
                    blockchain.setType(i.getArgument(2));
                    blockchain.setMetadata(i.getArgument(3));
                    return blockchain;
                });
        operationContext = new OperationContext();
        operationContext.initId();
        Blockchain rawBlockchain = new Blockchain();
        rawBlockchain.setConsortium(CONS_ID);
        blockchainObserver = new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService,
                                                    clientService,
                                                    taskService,
                                                    connectionPoolManager,
                                                    TASK_ID, NodeAssignment.newBuilder().build(),
                                                    rawBlockchain);

        value = DeploymentExecutionEvent.newBuilder()
                .setType(DeploymentExecutionEvent.Type.NOOP)
                .setStatus(DeploymentExecutionEvent.Status.ACTIVE)
                .setSessionId(UUID.randomUUID().toString())
                .setResource(DeployedResource.newBuilder()
                                     .setType(DeployedResource.Type.GENERIC)
                                     .setName("")
                                     .setSiteId(UUID.randomUUID().toString())
                                     .setNodeId(UUID.randomUUID().toString())
                                     .build())
                .build();
    }

    @Test
    void deployClusterFailure() throws Exception {
        ReflectionTestUtils.setField(value, "type_", Type.COMPLETED.getNumber());
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
        ReflectionTestUtils.setField(value, "type_", Type.COMPLETED.getNumber());
        ReflectionTestUtils.setField(value, "status_", Status.FAILURE.getNumber());
        blockchainObserver.onNext(value);
        Assertions.assertEquals(State.RUNNING, task.getState());
        blockchainObserver.onError(e);
        Assertions.assertEquals(State.FAILED, task.getState());
        Assertions.assertEquals("This is a bad request", task.getMessage());
    }

}
