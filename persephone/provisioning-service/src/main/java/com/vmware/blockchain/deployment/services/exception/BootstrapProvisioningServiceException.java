/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

/**
 * Bootstrap exception.
 */
public class BootstrapProvisioningServiceException extends PersephoneException {

    /**
     * Constructor with variable arguments.
     * @param message error message
     * @param args variable arguments
     */
    public BootstrapProvisioningServiceException(String message, Object... args) {
        super(message, args);
    }
}
