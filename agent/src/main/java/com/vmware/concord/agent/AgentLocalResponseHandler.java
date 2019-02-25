/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONObject;

/**
 * AgentLocalResponseHandler --
 * This implements the abstract RPC handler for Agent.
 * Currently this is a dummy implementation.
 */
public class AgentLocalResponseHandler extends AbstractAgentHandler {

    private static Logger logger = LogManager.getLogger(AgentLocalResponseHandler.class);

    public boolean buildRequest() throws Exception {
        return false;
    }

    public JSONObject buildResponse() throws Exception {
        return new JSONObject();
    }
}
