/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.concord.Concord;
import com.vmware.concord.Concord.EthRequest;
import com.vmware.concord.Concord.EthRequest.EthMethod;
import com.vmware.concord.Concord.EthResponse;

/**
 * This handler is used to service personal_newAccount POST requests.
 */
public class EthNewAccountHandler extends AbstractEthRpcHandler {

    private static Logger logger = LogManager.getLogger(EthNewAccountHandler.class);

    /**
     * Builds the Concord request builder. Extracts the passphrase from the request and uses it to set up an Concord
     * Request builder with an EthRequest.
     *
     * @param concordRequestBuilder Object in which request is built
     * @param requestJson Request parameters passed by the user
     * @return Always true - send the request.
     */
    @Override
    public boolean buildRequest(Concord.ConcordRequest.Builder concordRequestBuilder, JSONObject requestJson)
            throws Exception {
        Concord.EthRequest ethRequest;
        try {
            EthRequest.Builder b = initializeRequestObject(requestJson);
            b.setMethod(EthMethod.NEW_ACCOUNT);
            JSONArray params = extractRequestParams(requestJson);

            String passphrase = (String) params.get(0);
            try {
                b.setData(ByteString.copyFrom(passphrase, StandardCharsets.UTF_8.name()));
            } catch (UnsupportedEncodingException e) {
                logger.error("Invalid passphrase");
                throw new EthRpcHandlerException(
                        EthDispatcher.errorMessage(
                                ErrorCodeType.PASSPHRASE_INVALID.getErrorCodeTypeValue(),
                                b.getId(),
                                jsonRpc
                        ).toJSONString()
                );
            }
            ethRequest = b.build();
        } catch (Exception e) {
            logger.error("Exception in new account handler", e);
            throw e;
        }
        concordRequestBuilder.addEthRequest(ethRequest);
        return true;
    }

    /**
     * Builds the response object to be returned to the user.
     *
     * @param concordResponse Response received from Concord
     * @param requestJson Request parameters passed by the user
     * @return response to be returned to the user
     */
    @SuppressWarnings("unchecked")
    @Override
    public JSONObject buildResponse(Concord.ConcordResponse concordResponse, JSONObject requestJson) {
        EthResponse ethResponse = concordResponse.getEthResponse(0);
        JSONObject respObject = initializeResponseObject(ethResponse);
        respObject.put("result", ApiHelper.binaryStringToHex(ethResponse.getData()));
        return respObject;
    }
}
