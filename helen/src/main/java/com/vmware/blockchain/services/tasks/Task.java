/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.tasks;

import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Entity for tasks.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@EntityColumnName("helen.task")
public class Task extends AbstractEntity {

    /**
     * Current state of the task.
     */
    public enum State {
        NONE,
        RUNNING,
        SUCCEEDED,
        FAILED
    }

    private State state = State.NONE;
    private String message;
    private UUID resourceId;
    private String resourceLink;

}
