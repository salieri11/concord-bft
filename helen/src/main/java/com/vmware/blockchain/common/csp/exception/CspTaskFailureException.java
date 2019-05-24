/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Task Failure exception.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class CspTaskFailureException extends CspException {

    private String taskUrl;
    private String taskStage;
    private String failureMessage;

    private static final String TASK_FAILURE_MESSAGE = "The csp task at url %s failed with stage %s and message %s";

    /**
     * Create an exception with the task that failed.
     * @param taskUrl - task url that failed.
     * @param taskStage - task stage returned from csp.
     * @param failureMessage - failure message from csp.
     */
    public CspTaskFailureException(String taskUrl, String taskStage, String failureMessage) {
        super(String.format(TASK_FAILURE_MESSAGE, taskUrl, taskStage, failureMessage));
        this.taskStage = taskStage;
        this.taskUrl = taskUrl;
        this.failureMessage = failureMessage;
    }

}
