/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.services.tasks.Task.State;

/**
 * Test tasks.
 */
public class TaskTests {

    @Test
    void testTask() {
        Task t = new Task();
        Assertions.assertEquals(State.NONE, t.getState());
    }

}
