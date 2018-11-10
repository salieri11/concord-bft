/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import com.vmware.athena.Athena.FilterRequest;
import com.vmware.athena.Athena.FilterRequest.FilterRequestType;
import com.vmware.athena.Athena.FilterResponse;
import com.vmware.blockchain.common.Constants;

/**
 * This handler is used to service following types of filter requests.
 * <ul>
 * <li>eth_newBlockFilter</li>
 * <li>eth_newFilter(TODO)</li>
 * <li>eth_newPendingTransactionFilter(TODO)</li>
 * <li>eth_getFilterChanges</li>
 * <li>eth_uninstallFilter</li>
 * </ul>
 * </p>
 */
public class EthFilterHandler extends AbstractEthRpcHandler {

    private static Logger logger = LogManager.getLogger(EthFilterHandler.class);

    /**
     * Builds the EthRequest Object from the type of eth request specified in requestJson. Adds the built request into
     * AthenaRequest using given builder.
     */
    public void buildRequest(Athena.AthenaRequest.Builder builder, JSONObject requestJson) throws Exception {
        Athena.EthRequest.Builder b = initializeRequestObject(requestJson);
        String ethMethodName = EthDispatcher.getEthMethodName(requestJson);
        JSONArray params = extractRequestParams(requestJson);

        b.setMethod(EthMethod.FILTER_REQUEST);

        if (ethMethodName.equals(Constants.NEWFILTER_NAME)) {
            // TODO: handle new filter
            logger.warn("eth_newFilter method is not implemented yet");
        } else if (ethMethodName.equals(Constants.NEWBLOCKFILTER_NAME)) {
            FilterRequest.Builder fb = FilterRequest.newBuilder();
            fb.setType(FilterRequestType.NEW_BLOCK_FILTER);
            b.setFilterRequest(fb.build());
        } else if (ethMethodName.equals(Constants.NEWPENDINGTRANSACTIONFILTER_NAME)) {
            // TODO: handle new pending transaction filter
            logger.warn("eth_newPendingTransactionFilter method is not" + "implemented yet");
        } else if (ethMethodName.equals(Constants.FILTERCHANGE_NAME)) {
            FilterRequest.Builder fb = FilterRequest.newBuilder();
            fb.setType(FilterRequestType.FILTER_CHANGE_REQUEST);
            fb.setFilterId(ApiHelper.hexStringToBinary((String) params.get(0)));
            b.setFilterRequest(fb.build());
        } else if (ethMethodName.equals(Constants.UNINSTALLFILTER_NAME)) {
            FilterRequest.Builder fb = FilterRequest.newBuilder();
            fb.setType(FilterRequestType.UNINSTALL_FILTER);
            fb.setFilterId(ApiHelper.hexStringToBinary((String) params.get(0)));
            b.setFilterRequest(fb.build());
        }

        Athena.EthRequest athenaEthRequest = b.build();

        builder.addEthRequest(athenaEthRequest);
    }

    /**
     * Extracts the FilterResponse objects from passed athenaResponse object and returns a RPC JSONObject made from
     * FilterResponse.
     *
     * @param athenaResponse Object of AthenaResponse
     * @param requestJson The original request Json
     * @return the reply JSON object made from FilterResponse object inside AthenaResponse.
     */
    @SuppressWarnings("unchecked")
    public JSONObject buildResponse(Athena.AthenaResponse athenaResponse, JSONObject requestJson) throws Exception {
        try {
            EthResponse ethResponse = athenaResponse.getEthResponse(0);
            JSONObject respObject = initializeResponseObject(ethResponse);
            String ethMethodName = EthDispatcher.getEthMethodName(requestJson);

            if (ethMethodName.equals(Constants.NEWFILTER_NAME)
                    || ethMethodName.equals(Constants.NEWBLOCKFILTER_NAME)
                    || ethMethodName.equals(Constants.NEWPENDINGTRANSACTIONFILTER_NAME)) {
                respObject.put("result", ApiHelper.binaryStringToHex(ethResponse.getFilterResponse().getFilterId()));
            } else if (ethMethodName.equals(Constants.FILTERCHANGE_NAME)) {
                JSONArray arr = new JSONArray();
                FilterResponse fresponse = ethResponse.getFilterResponse();
                for (ByteString hash : fresponse.getBlockHashesList()) {
                    arr.add(ApiHelper.binaryStringToHex(hash));
                }
                respObject.put("result", arr);
            } else {
                respObject.put("result", ethResponse.getFilterResponse().getSuccess());
            }
            return respObject;
        } catch (Exception e) {
            logger.error("Exception in Filter Handler build response", e);
            throw e;
        }
    }

}
