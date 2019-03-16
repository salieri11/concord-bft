/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.vmware.blockchain.common.Constants;
import com.vmware.concord.Concord;

/**
 * This handler is used for all `eth_getTransactionReceipt` requests.
 */
public class EthGetLogsHandler extends AbstractEthRpcHandler {

    Logger logger = LogManager.getLogger(EthGetLogsHandler.class);

    /**
     * Build a LogsRequest from the given requestJson.
     *
     * @param  builder      ConcordRequest Builder
     * @param  requestJson  JSONObject from the original RPC request
     * @return true         Always send the request
     */
    public boolean buildRequest(Concord.ConcordRequest.Builder builder, JSONObject requestJson) throws Exception {
        try {
            JSONArray params = extractRequestParams(requestJson);

            if (params.size() > 1) {
                throw new EthRpcHandlerException("Too many parameters. Either none or a JSON object.");
            }

            Concord.LogsRequest.Builder logsReq = Concord.LogsRequest.newBuilder();

            if (params.size() == 0) {
                // Default - request all logs from the latest block
                throw new EthRpcHandlerException("all logs not supported yet.");
            } else {
                // Evaluate filter options
                JSONObject filter = (JSONObject) params.get(0);

                if (filter.containsKey("fromBlock") || filter.containsKey("toBlock")) {
                    throw new EthRpcHandlerException("block filter not supported yet.");
                }
                if (filter.containsKey("address")) {
                    throw new EthRpcHandlerException("'address' filter not supported yet.");
                }
                if (filter.containsKey("topics")) {
                    throw new EthRpcHandlerException("'topics' filter not supported yet.");
                }
                if (filter.containsKey("blockHash")) {
                    logsReq.setBlockHash(ApiHelper.hexStringToBinary((String) filter.get("blockHash")));
                }
            }

            builder.setLogsRequest(logsReq.build());
            return true;
        } catch (Exception e) {
            logger.error("Exception in tx receipt handler", e);
            throw e;
        }
    }

    /**
     * Build a JSONObject response from the LogsResponse within the given ConcordResponse.
     *
     * @param  concordResponse  ConcordResponse object
     * @param  requestJson      JSONObject from the original RPC request
     * @return JSONObject
     */
    @Override
    public JSONObject buildResponse(Concord.ConcordResponse concordResponse, JSONObject requestJson) {

        JSONObject respObject = new JSONObject();
        JSONArray result = new JSONArray();
        try {
            Concord.LogsResponse logsResp = concordResponse.getLogsResponse();
            for (int i = 0; i < logsResp.getLogCount(); i++) {
                Concord.LogResponse log = logsResp.getLog(i);
                JSONObject logJson = new JSONObject();

                logJson.put("address",             ApiHelper.binaryStringToHex(log.getContractAddress()));
                logJson.put("blockHash",           ApiHelper.binaryStringToHex(log.getBlockHash()));
                logJson.put("blockNumber",         "0x" + Long.toHexString(log.getBlockNumber()));
                logJson.put("logIndex",            "0x" + Long.toHexString(log.getLogIndex()));
                logJson.put("transactionHash",     ApiHelper.binaryStringToHex(log.getTransactionHash()));
                logJson.put("transactionIndex",    "0x" + Long.toHexString(log.getTransactionIndex()));
                logJson.put("transactionLogIndex", "0x" + Long.toHexString(log.getTransactionLogIndex()));

                JSONArray topics = new JSONArray();
                for (int j = 0; j < log.getTopicCount(); j++) {
                    topics.add(ApiHelper.binaryStringToHex(log.getTopic(j)));
                }
                logJson.put("topics", topics);

                if (log.hasData()) {
                    logJson.put("data", ApiHelper.binaryStringToHex(log.getData()));
                } else {
                    logJson.put("data", "0x");
                }

                result.add(logJson);
            }

            respObject.put("result", result);
            respObject.put("id", EthDispatcher.getEthRequestId(requestJson));
            respObject.put("jsonrpc", Constants.JSONRPC);
        } catch (Exception e) {
            logger.fatal("Building JSON response failed.", e);
        }
        return respObject;
    }
}
