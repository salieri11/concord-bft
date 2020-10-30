/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

/**
 * Invalid Configuration exception, used during invalid configuration.
 */
public class InvalidConfigurationException extends RuntimeException {

    public InvalidConfigurationException(String msg) {
        super(msg);
    }
}
