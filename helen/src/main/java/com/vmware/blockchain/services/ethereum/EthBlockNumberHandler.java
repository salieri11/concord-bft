/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;

/**
 * Handler for handling the `eth_blockNumber` RPC call.
 */
public class EthBlockNumberHandler extends AbstractEthRPCHandler {

    Logger logger = LogManager.getLogger(EthBlockNumberHandler.class);

    public EthBlockNumberHandler(AthenaProperties config) {
        super(config);
        // TODO Auto-generated constructor stub
    }

    /**
     * Builds the EthRequest object which can be sent to Athena from given requestJson object.
     *
     * @param builder Builder object in which parameters are set.
     * @param requestJson User request
     * @throws Exception
     */
    @Override
    public void buildRequest(Athena.AthenaRequest.Builder builder, JSONObject requestJson) throws Exception {
        Athena.EthRequest.Builder ethRequestBuilder = initializeRequestObject(requestJson);
        ethRequestBuilder.setMethod(Athena.EthRequest.EthMethod.BLOCK_NUMBER);
        builder.addEthRequest(ethRequestBuilder.build());
    }

    /**
     * Builds the RPC JSON response object from given AthenaResponse. Extracts the EthResponse object from
     * athenaRequest. This EthResponse object must contain the data filed with hex string representing latex block
     * number.
     *
     * @param athenaResponse Response received from Athena.
     * @param requestJson User request.
     * @return JSON RPC response object
     * @throws Exception
     */
    @Override
    public JSONObject buildResponse(Athena.AthenaResponse athenaResponse, JSONObject requestJson) throws Exception {
        Athena.EthResponse ethResponse = athenaResponse.getEthResponse(0);
        JSONObject responseObject = initializeResponseObject(ethResponse);
        ByteString latestBlock = ethResponse.getData();
        responseObject.put("result", APIHelper.binaryStringToHex(latestBlock, true /* drop leading zeros */));
        return responseObject;
    }
}
