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
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.athena.Athena.FilterResponse;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved
 * </p>
 *
 * <p>
 * This handler is used to service following types of filter requests:
 * <ul>
 * <li>eth_newBlockFilter</li>
 * <li>eth_newFilter(TODO)</li>
 * <li>eth_newPendingTransactionFilter(TODO)</li>
 * <li>eth_getFilterChanges</li>
 * <li>eth_uninstallFilter</li>
 * </ul>
 * </p>
 */
public class EthFilterHandler extends AbstractEthRPCHandler {

    private static Logger logger = LogManager.getLogger(EthFilterHandler.class);

    public EthFilterHandler(AthenaProperties config) {
        super(config);
        // TODO Auto-generated constructor stub
    }

    /**
     * Builds the EthRequest Object from the type of eth request specified in requestJson. Adds the built request into
     * AthenaRequest using given builder.
     *
     * @param builder
     * @param requestJson
     * @throws Exception
     */
    public void buildRequest(Athena.AthenaRequest.Builder builder, JSONObject requestJson) throws Exception {
        Athena.EthRequest.Builder b = initializeRequestObject(requestJson);
        String ethMethodName = EthDispatcher.getEthMethodName(requestJson);
        JSONArray params = extractRequestParams(requestJson);

        b.setMethod(EthMethod.FILTER_REQUEST);

        if (ethMethodName.equals(config.getNewFilter_Name())) {
            // TODO: handle new filter
            logger.warn("eth_newFilter method is not implemented yet");
        } else if (ethMethodName.equals(config.getNewBlockFilter_Name())) {
            FilterRequest.Builder fb = FilterRequest.newBuilder();
            fb.setType(FilterRequestType.NEW_BLOCK_FILTER);
            b.setFilterRequest(fb.build());
        } else if (ethMethodName.equals(config.getNewPendingTransactionFilter_Name())) {
            // TODO: handle new pending transaction filter
            logger.warn("eth_newPendingTransactionFilter method is not" + "implemented yet");
        } else if (ethMethodName.equals(config.getFilterChange_Name())) {
            FilterRequest.Builder fb = FilterRequest.newBuilder();
            fb.setType(FilterRequestType.FILTER_CHANGE_REQUEST);
            fb.setFilterId(APIHelper.hexStringToBinary((String) params.get(0)));
            b.setFilterRequest(fb.build());
        } else if (ethMethodName.equals(config.getUninstallFilter_Name())) {
            FilterRequest.Builder fb = FilterRequest.newBuilder();
            fb.setType(FilterRequestType.UNINSTALL_FILTER);
            fb.setFilterId(APIHelper.hexStringToBinary((String) params.get(0)));
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
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    public JSONObject buildResponse(Athena.AthenaResponse athenaResponse, JSONObject requestJson) throws Exception {
        try {
            EthResponse ethResponse = athenaResponse.getEthResponse(0);
            JSONObject respObject = initializeResponseObject(ethResponse);
            String ethMethodName = EthDispatcher.getEthMethodName(requestJson);

            if (ethMethodName.equals(config.getNewFilter_Name())
                    || ethMethodName.equals(config.getNewBlockFilter_Name())
                    || ethMethodName.equals(config.getNewPendingTransactionFilter_Name())) {
                respObject.put("result", APIHelper.binaryStringToHex(ethResponse.getFilterResponse().getFilterId()));
            } else if (ethMethodName.equals(config.getFilterChange_Name())) {
                JSONArray arr = new JSONArray();
                FilterResponse fresponse = ethResponse.getFilterResponse();
                for (ByteString hash : fresponse.getBlockHashesList()) {
                    arr.add(APIHelper.binaryStringToHex(hash));
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
