/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.function.Consumer;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent.Type;
import com.vmware.blockchain.security.AuthHelper;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.TaskService;

/**
 * Test the blockchain observer.
 */
@ExtendWith(SpringExtension.class)
public class BlockchainObserverTest {

    static final UUID TASK_ID = UUID.fromString("97668112-178b-4ed3-a1ac-84435e0f1f86");

    @MockBean
    private AuthHelper authHelper;

    @MockBean
    private TaskService taskService;

    @MockBean
    private BlockchainService blockchainService;

    private BlockchainObserver blockchainObserver;

    private Task task;

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
        blockchainObserver = new BlockchainObserver(authHelper, blockchainService, taskService, TASK_ID);
        // TODO: Expand the cluster, and add in more tests.
        value = new DeploymentSessionEvent();
    }

    @Test
    void deployNode() throws Exception {
        ReflectionTestUtils.setField(value, "type", Type.NODE_DEPLOYED);
        blockchainObserver.onNext(value);
        Assertions.assertEquals(Type.NODE_DEPLOYED.name(), task.getMessage());
    }


}
