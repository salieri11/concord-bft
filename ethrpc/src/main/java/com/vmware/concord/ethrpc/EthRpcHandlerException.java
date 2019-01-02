/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.ethrpc;

/**
 * This class serves as a common exception type thrown by all EthRPC handlers.
 */
public class EthRpcHandlerException extends Exception {
    private static final long serialVersionUID = 1L;

    public EthRpcHandlerException(String message) {
        super(message);
    }
}
