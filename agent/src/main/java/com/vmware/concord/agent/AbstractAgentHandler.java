/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;


/**
 * Handle all the Agent requests.
 */
public abstract class AbstractAgentHandler {
    protected String jsonRpc;

    public AbstractAgentHandler() {
        this.jsonRpc = Constants.JSONRPC;
    }

    /**
     * This method extracts relevant parameters from the user request.
     *
     * @return True if the request should be sent to Concord. False if the request should be ignored, and buildResponse
     *         should be called with a null ConcordResponse.
     */
    public abstract boolean buildRequest()
        throws Exception;

    /**
     * This method extracts the relevant parameters from internal response and build a JSONObject which is
     * then sent to the user as a response to the RPC request.
     *
     * @return Response object to be returned to the user.
     */
    public abstract JSONObject buildResponse()
            throws Exception;

    /**
     * Extracts the "params" part of the user's request.
     *
     * @param requestJson User request
     * @return the "params" array
     */
    JSONArray extractRequestParams(JSONObject requestJson) throws AgentRpcHandlerException {
        JSONArray params = null;
        try {
            params = (JSONArray) requestJson.get("params");
            if (params == null) {
                throw new AgentRpcHandlerException("'params' not present");
            }
        } catch (ClassCastException cse) {
            throw new AgentRpcHandlerException("'params' must be an array");
        }
        return params;
    }
}
