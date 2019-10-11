/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.rpc;

/**
 * This class serves as a common exception type thrown by all EthRPC handlers.
 */
public class AgentRpcHandlerException extends Exception {
    private static final long serialVersionUID = 1L;

    public AgentRpcHandlerException(String message) {
        super(message);
    }
}
