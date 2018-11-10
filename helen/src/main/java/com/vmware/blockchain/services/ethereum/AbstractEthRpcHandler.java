/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.blockchain.common.Constants;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * <p>
 * This abstract class serves as a template for all EthRPC Handler classes. These handlers are used to construct
 * AthenaRequest objects from user requests and to construct responses for the user from AthenaResponses based on the
 * method specified by the user.
 * </p>
 *
 * <p>
 * Concrete helper methods have been implemented for performing some common actions on request/response objects.
 * </p>
 */
public abstract class AbstractEthRpcHandler {


    protected String jsonRpc;

    public AbstractEthRpcHandler() {
        this.jsonRpc = Constants.JSONRPC;
    }

    /**
     * This method extracts relevant parameters from the user request and sets them in the AthenaRequest builder object
     * passed by the caller as a parameter.
     *
     * @param builder Builder object in which parameters are set.
     * @param requestJson User request
     */
    public abstract void buildRequest(Athena.AthenaRequest.Builder builder, JSONObject requestJson) throws Exception;

    /**
     * This method extracts the relevant parameters from an AthenaResponse and uses them to build a JSONObject which is
     * then sent to the user as a response to the RPC request.
     *
     * @param athenaResponse Response received from Athena.
     * @param requestJson User request.
     * @return Response object to be returned to the user.
     */
    public abstract JSONObject buildResponse(Athena.AthenaResponse athenaResponse, JSONObject requestJson)
            throws Exception;

    /**
     * Initializes an EthRequest builder object and pre-sets the request id in it.
     *
     * @param requestJson User request
     * @return Newly initialized EthRequest builder object.
     */
    EthRequest.Builder initializeRequestObject(JSONObject requestJson) throws EthRpcHandlerException {
        EthRequest.Builder b = Athena.EthRequest.newBuilder();
        long id = EthDispatcher.getEthRequestId(requestJson);
        b.setId(id);
        return b;
    }

    /**
     * Extracts the "params" part of the user's request.
     *
     * @param requestJson User request
     * @return the "params" array
     */
    JSONArray extractRequestParams(JSONObject requestJson) throws EthRpcHandlerException {
        JSONArray params = null;
        try {
            params = (JSONArray) requestJson.get("params");
            if (params == null) {
                throw new EthRpcHandlerException("'params' not present");
            }
        } catch (ClassCastException cse) {
            throw new EthRpcHandlerException("'params' must be an array");
        }
        return params;
    }

    /**
     * Initializes the response to be sent to the user and pre-sets the 'id' and 'jsonrpc' fields.
     *
     * @param ethResponse EthResponse part of the response received from Athena.
     * @return
     */
    @SuppressWarnings("unchecked")
    JSONObject initializeResponseObject(EthResponse ethResponse) {
        JSONObject respObject = new JSONObject();
        respObject.put("id", ethResponse.getId());
        respObject.put("jsonrpc", jsonRpc);
        return respObject;
    }

}
