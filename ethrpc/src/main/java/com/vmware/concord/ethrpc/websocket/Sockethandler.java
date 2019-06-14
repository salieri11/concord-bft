/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.ethrpc.websocket;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import com.vmware.concord.Concord;

import com.vmware.concord.Concord.ConcordResponse;
import com.vmware.concord.Concord.ErrorResponse;
import com.vmware.concord.ConcordHelper;
import com.vmware.concord.IConcordConnection;

import com.vmware.concord.connections.ConcordConnectionPool;
import com.vmware.concord.ethrpc.AbstractEthRpcHandler;
import com.vmware.concord.ethrpc.ApiHelper;
import com.vmware.concord.ethrpc.Constants;
import com.vmware.concord.ethrpc.EthBlockNumberHandler;
import com.vmware.concord.ethrpc.EthFilterChangeHandler;
import com.vmware.concord.ethrpc.EthFilterManageHandler;
import com.vmware.concord.ethrpc.EthGetBalanceHandler;
import com.vmware.concord.ethrpc.EthGetBlockHandler;
import com.vmware.concord.ethrpc.EthGetCodeHandler;
import com.vmware.concord.ethrpc.EthGetLogsHandler;
import com.vmware.concord.ethrpc.EthGetStorageAtHandler;
import com.vmware.concord.ethrpc.EthGetTransactionByHashHandler;
import com.vmware.concord.ethrpc.EthGetTransactionCountHandler;
import com.vmware.concord.ethrpc.EthGetTransactionReceiptHandler;
import com.vmware.concord.ethrpc.EthLocalResponseHandler;
import com.vmware.concord.ethrpc.EthRpcHandlerException;
import com.vmware.concord.ethrpc.EthSendTxHandler;

/**
 * <p>
 * Websocket handler for Eth RPCs.
 * url endpoint : ws:localhost:8545/ws/
 * </p>
 */
@Component
public class Sockethandler extends TextWebSocketHandler {
    private static final long serialVersionUID = 1L;
    public static long netVersion;
    public static boolean netVersionSet;
    private String jsonRpc;
    private static Logger logger = LogManager.getLogger("websocketLogger");
    private ConcordConnectionPool concordConnectionPool;
    List<Object> sessions = new CopyOnWriteArrayList<>();

    public Sockethandler(ConcordConnectionPool connectionPool) {
        this.concordConnectionPool = connectionPool;
    }

    @Override
    public void handleTextMessage(WebSocketSession session, TextMessage message)
            throws InterruptedException, IOException {
        JSONAware aware = getConcordMessage(message.getPayload());
        session.sendMessage(new TextMessage(aware.toJSONString()));
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        sessions.add(session);
    }

    /**
     * Services the websocket request for listing currently exposed Eth RPCs.
     */
    @SuppressWarnings("unchecked")
    public JSONAware getConcordMessage(String paramString) {
        // Retrieve the request fields
        JSONArray batchRequest = null;
        JSONArray batchResponse = new JSONArray();
        JSONParser parser = new JSONParser();
        JSONAware responseBody;
        boolean isBatch = false;
        ThreadContext.put("organization_id", "1234");
        ThreadContext.put("consortium_id", "1234");
        ThreadContext.put("method", "POST");
        ThreadContext.put("uri", "/");
        try {
            logger.debug("Request Parameters: " + paramString);

            // If we receive a single request, add it to a JSONArray for the sake
            // of uniformity.
            if (paramString.startsWith("[")) {
                isBatch = true;
                batchRequest = (JSONArray) parser.parse(paramString);
                if (batchRequest == null || batchRequest.size() == 0) {
                    throw new Exception("Invalid request");
                }
            } else {
                batchRequest = new JSONArray();
                batchRequest.add(parser.parse(paramString));
            }

            for (Object params : batchRequest) {
                JSONObject requestParams = (JSONObject) params;

                // Dispatch requests to the corresponding handlers
                batchResponse.add(dispatch(requestParams));
            }
            if (isBatch) {
                responseBody = batchResponse;
            } else {
                responseBody = (JSONObject) batchResponse.get(0);
            }
        } catch (ParseException e) {
            logger.error("Invalid request", e);
            responseBody = errorMessage("Unable to parse request", -1, jsonRpc);
        } catch (Exception e) {
            logger.error(ApiHelper.exceptionToString(e));
            responseBody = errorMessage(e.getMessage(), -1, jsonRpc);
        } finally {
            ThreadContext.clearAll();
        }
        logger.debug("Response: " + responseBody.toJSONString());
        return responseBody;
    }

    /**
     * Creates the appropriate handler object and calls its functions to construct an ConcordRequest object. Sends this
     * request to Concord and converts its response into a format required by the user.
     *
     * @param  requestJson  User input
     * @return JSONObject   JSON returned to the caller
     */
    public JSONObject dispatch(JSONObject requestJson) throws Exception {
        // Default initialize variables, so that if exception is thrown
        // while initializing the variables error message can be constructed
        // with default values.
        long id = -1;
        String ethMethodName;
        AbstractEthRpcHandler handler;
        boolean isLocal = false;
        JSONObject responseObject;
        ConcordResponse concordResponse;
        // Create object of the suitable handler based on the method specified in
        // the request
        try {
            ethMethodName = getEthMethodName(requestJson);
            ThreadContext.put("source", ethMethodName);
            id = getEthRequestId(requestJson);
            ThreadContext.put("id", String.valueOf(id));
            switch (ethMethodName) {
                case Constants.SEND_TRANSACTION_NAME:
                case Constants.SEND_RAWTRANSACTION_NAME:
                case Constants.CALL_NAME:
                    handler = new EthSendTxHandler();
                    break;

                case Constants.GET_TRANSACTIONBYHASH_NAME:
                    handler = new EthGetTransactionByHashHandler();
                    break;

                case Constants.GET_TRANSACTIONRECEIPT_NAME:
                    handler = new EthGetTransactionReceiptHandler();
                    break;

                case Constants.GET_STORAGEAT_NAME:
                    handler = new EthGetStorageAtHandler();
                    break;

                case Constants.GET_CODE_NAME:
                    handler = new EthGetCodeHandler();
                    break;

                case Constants.GET_TRANSACTIONCOUNT_NAME:
                    handler = new EthGetTransactionCountHandler();
                    break;

                case Constants.GET_BALANCE_NAME:
                    handler = new EthGetBalanceHandler();
                    break;

                case Constants.GET_BLOCKBYHASH_NAME:
                case Constants.GET_BLOCKBYNUMBER_NAME:
                    handler = new EthGetBlockHandler();
                    break;

                case Constants.UNINSTALLFILTER_NAME:
                    isLocal = true; // fallthrough
                case Constants.NEWFILTER_NAME:
                case Constants.NEWBLOCKFILTER_NAME:
                case Constants.NEWPENDINGTRANSACTIONFILTER_NAME:
                    handler = new EthFilterManageHandler();
                    break;

                case Constants.FILTERCHANGE_NAME:
                    handler = new EthFilterChangeHandler();
                    break;

                case Constants.NETVERSION_NAME:
                    handler = new EthLocalResponseHandler();
                    isLocal = netVersionSet;
                    break;

                case Constants.WEB3_SHA3_NAME:
                case Constants.RPC_MODULES_NAME:
                case Constants.COINBASE_NAME:
                case Constants.CLIENT_VERSION_NAME:
                case Constants.MINING_NAME:
                case Constants.ACCOUNTS_NAME:
                case Constants.GAS_PRICE_NAME:
                case Constants.ESTIMATE_GAS_NAME:
                case Constants.SYNCING_NAME:
                    handler = new EthLocalResponseHandler();
                    isLocal = true;
                    break;

                case Constants.BLOCKNUMBER_NAME:
                    handler = new EthBlockNumberHandler();
                    break;

                case Constants.GET_LOGS_NAME:
                    handler = new EthGetLogsHandler();
                    break;

                default:
                    throw new Exception("Invalid method name.");
            }

            if (!isLocal) {
                Concord.ConcordRequest.Builder concordRequestBuilder = Concord.ConcordRequest.newBuilder();
                if (handler.buildRequest(concordRequestBuilder, requestJson)) {
                    concordResponse = communicateWithConcord(concordRequestBuilder.build());
                    // If there is an error reported by Concord
                    if (concordResponse.getErrorResponseCount() > 0) {
                        ErrorResponse errResponse = concordResponse.getErrorResponse(0);
                        if (errResponse.hasDescription()) {
                            responseObject = errorMessage(errResponse.getDescription(), id, jsonRpc);
                        } else {
                            responseObject = errorMessage("Error received from concord", id, jsonRpc);
                        }
                    } else {
                        responseObject = handler.buildResponse(concordResponse, requestJson);
                    }
                } else {
                    // Handler said not to bother with the request. This request is handled locally instead.
                    responseObject = handler.buildResponse(null, requestJson);
                }
            }
            // There are some RPC methods which are handled locally by Helen. No
            // need to talk to Concord for these cases.
            else {
                // In local request we don't have valid eth resposne from
                // concord. Just pass null.
                responseObject = handler.buildResponse(null, requestJson);
            }
            logger.info("Eth RPC request");
            // TODO: Need to refactor exception handling.  Shouldn't just catch an exception and eat it.
        } catch (Exception e) {
            logger.error(ApiHelper.exceptionToString(e));
            responseObject = errorMessage(e.getMessage(), id, jsonRpc);
        }
        return responseObject;
    }

    /**
     * Constructs the response in case of error.
     *
     * @param message Error message
     * @param id Request Id
     * @param jsonRpc RPC version
     * @return Error message string
     */
    @SuppressWarnings("unchecked")
    public static JSONObject errorMessage(String message, long id, String jsonRpc) {
        JSONObject responseJson = new JSONObject();
        responseJson.put("id", id);
        responseJson.put("jsonprc", jsonRpc);

        JSONObject error = new JSONObject();
        error.put("message", message);
        responseJson.put("error", error);

        return responseJson;
    }

    /**
     * Sends an ConcordRequest to Concord and receives Concord's response.
     *
     * @param req ConcordRequest object
     * @return Response received from Concord
     */
    private ConcordResponse communicateWithConcord(Concord.ConcordRequest req) throws Exception {
        IConcordConnection conn = null;
        Concord.ConcordResponse concordResponse = null;
        try {
            conn = concordConnectionPool.getConnection();
            if (conn == null) {
                throw new Exception("Error communicating with concord");
            }

            boolean res = ConcordHelper.sendToConcord(req, conn);
            if (!res) {
                throw new Exception("Error communicating with concord");
            }

            // receive response from Concord
            concordResponse = ConcordHelper.receiveFromConcord(conn);
            if (concordResponse == null) {
                throw new Exception("Error communicating with concord");
            }
        } catch (Exception e) {
            logger.error("General exception communicating with concord: ", e);
            throw e;
        } finally {
            concordConnectionPool.putConnection(conn);
        }

        return concordResponse;
    }

    /**
     * Extracts the RPC method name from the request JSON.
     *
     * @param ethRequestJson Request JSON
     * @return Method name
     */
    public static String getEthMethodName(JSONObject ethRequestJson) throws EthRpcHandlerException {
        if (ethRequestJson.containsKey("method")) {
            if (ethRequestJson.get("method") instanceof String) {
                return (String) ethRequestJson.get("method");
            } else {
                throw new EthRpcHandlerException("method must be a string");
            }
        } else {
            throw new EthRpcHandlerException("request must contain a method");
        }
    }

    /**
     * Extracts the Request Id from the request JSON.
     *
     * @param ethRequestJson Request JSON
     * @return Request id
     */
    public static long getEthRequestId(JSONObject ethRequestJson) throws EthRpcHandlerException {
        if (ethRequestJson.containsKey("id")) {
            if (ethRequestJson.get("id") instanceof Number) {
                return ((Number) ethRequestJson.get("id")).longValue();
            } else {
                throw new EthRpcHandlerException("id must be a number");
            }
        } else {
            throw new EthRpcHandlerException("request must contain an id");
        }
    }
}
