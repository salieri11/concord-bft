/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.google.protobuf.ByteString;
import com.vmware.concord.Concord;

/**
 * This Handler is used for handling all `eth_getTransactionReceipt` types of requests. EthGetTxReceiptHandler is little
 * different than other handlers because It leverages already existing `TransactionReceipt` ConcordRequest to handle
 * `eth_getTransactionReceipt` requests (see Transaction.java file which implements this API). Hence, in this handler we
 * actually put a `TransactionRequest` inside ConcordRequest and read a TransactionResponse from ConcordResponse.
 */
public class EthGetTxReceiptHandler extends AbstractEthRpcHandler {

    Logger logger = LogManager.getLogger(EthGetTxReceiptHandler.class);

    /**
     * Builds a TransactionRequest object from given requestJson and inserts it into ConcordRequest Object.
     *
     * @param builder Concord Request Builder.
     * @param requestJson The JSONObject of original RPC request.
     */
    public void buildRequest(Concord.ConcordRequest.Builder builder, JSONObject requestJson) throws Exception {
        try {
            logger.debug("Inside GetTXReceipt buildRequest");
            // Construct a transaction request object.
            JSONArray params = extractRequestParams(requestJson);
            String txHash = (String) params.get(0);
            ByteString hashBytes = ApiHelper.hexStringToBinary(txHash);

            Concord.TransactionRequest txRequestObj =
                Concord.TransactionRequest.newBuilder().setHash(hashBytes).build();
            builder.setTransactionRequest(txRequestObj);
        } catch (Exception e) {
            logger.error("Exception in tx receipt handler", e);
            throw e;
        }
    }

    /**
     * Since the parents initializeResponseObject method takes EthResponse object as input we override it here to take
     * in the id directly.
     */
    @SuppressWarnings("unchecked")
    JSONObject initializeResponseObject(long id) {
        JSONObject respObject = new JSONObject();
        respObject.put("id", id);
        respObject.put("jsonrpc", jsonRpc);
        return respObject;
    }

    /**
     * Builds a response JSON object by extracting TransactionResponse object from given ConcordResponse Object.
     *
     * @param concordResponse The ConcordResponse object
     * @param requestJson The json object of original RPC request.
     * @return the response JSONObject.
     */
    @SuppressWarnings("unchecked")
    @Override
    public JSONObject buildResponse(Concord.ConcordResponse concordResponse, JSONObject requestJson) {

        JSONObject respObject = new JSONObject();
        try {
            Concord.TransactionResponse transactionResponse = concordResponse.getTransactionResponse();

            respObject = initializeResponseObject(EthDispatcher.getEthRequestId(requestJson));

            JSONObject result = new JSONObject();

            result.put("transactionHash", ApiHelper.binaryStringToHex(transactionResponse.getHash()));
            result.put("transactionIndex", transactionResponse.getTransactionIndex());
            result.put("blockNumber", transactionResponse.getBlockNumber());
            result.put("blockHash", ApiHelper.binaryStringToHex(transactionResponse.getBlockHash()));
            if (transactionResponse.hasContractAddress()) {
                result.put("contractAddress", ApiHelper.binaryStringToHex(transactionResponse.getContractAddress()));
            } else {
                result.put("contractAddress", null);
            }

            // Concord EVM has status code '0' for success and other Positive
            // values to denote error. However, for JSON RPC '1' is success
            // and '0' is failure. Here we need to reverse status value of concord
            // response before returning it.
            result.put("status", "0x" + Integer.toString(transactionResponse.getStatus() == 0 ? 1 : 0));

            result.put("logs", buildLogs(transactionResponse));

            respObject.put("result", result);
        } catch (Exception e) {
            // This should never get triggered as params are already checked while
            // building the request
            logger.fatal("'params' not present");
        }
        return respObject;
    }

    /**
     * Build the loggin JSON.
     */
    public static JSONArray buildLogs(Concord.TransactionResponse transactionResponse) {
        JSONArray logs = new JSONArray();
        for (int i = 0; i < transactionResponse.getLogCount(); i++) {
            Concord.LogResponse log = transactionResponse.getLog(i);
            JSONObject logJson = new JSONObject();
            logJson.put("address", ApiHelper.binaryStringToHex(log.getAddress()));

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
            logs.add(logJson);
        }
        return logs;
    }

}
