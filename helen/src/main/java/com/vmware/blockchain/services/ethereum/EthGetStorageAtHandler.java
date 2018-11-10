/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;

/**
 * This handler is used to service eth_getStorageAt POST requests.
 */
public class EthGetStorageAtHandler extends AbstractEthRpcHandler {

    Logger logger = LogManager.getLogger(EthBlockNumberHandler.class);

    /**
     * Builds the Athena request builder. Extracts the 'to' address and data from the request and uses it to set up an
     * Athena Request builder with an EthRequest.
     *
     * @param athenaRequestBuilder Object in which request is built
     * @param requestJson Request parameters passed by the user
     */
    @Override
    public void buildRequest(Athena.AthenaRequest.Builder athenaRequestBuilder, JSONObject requestJson)
            throws Exception {
        Athena.EthRequest ethRequest = null;
        try {
            EthRequest.Builder b = initializeRequestObject(requestJson);
            b.setMethod(EthMethod.GET_STORAGE_AT);
            JSONArray params = extractRequestParams(requestJson);
            b.setAddrTo(ApiHelper.hexStringToBinary((String) params.get(0)));
            String p = (String) params.get(1);
            String s = ApiHelper.padZeroes(p);
            b.setData(ApiHelper.hexStringToBinary(s));
            // add "block" parameter, the default block parameter is "latest".
            // if no parameter or its value is negative, athena treat is as default
            if (params.size() == 3) {
                long blockNumber = ApiHelper.parseBlockNumber(params);
                if (blockNumber >= 0) {
                    b.setBlockNumber(blockNumber);
                }
            }
            ethRequest = b.build();
        } catch (Exception e) {
            logger.error("Exception in get storage at handler", e);
            throw e;
        }
        athenaRequestBuilder.addEthRequest(ethRequest);
    }

    /**
     * Builds the response object to be returned to the user.
     *
     * @param athenaResponse Response received from Athena
     * @param requestJson Request parameters passed by the user
     * @return response to be returned to the user
     */
    @SuppressWarnings("unchecked")
    @Override
    public JSONObject buildResponse(Athena.AthenaResponse athenaResponse, JSONObject requestJson) {
        EthResponse ethResponse = athenaResponse.getEthResponse(0);
        JSONObject respObject = initializeResponseObject(ethResponse);
        respObject.put("result", ApiHelper.binaryStringToHex(ethResponse.getData()));
        return respObject;
    }
}
