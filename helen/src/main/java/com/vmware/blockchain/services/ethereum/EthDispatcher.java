/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.services.ConcordServlet;
import com.vmware.blockchain.services.contracts.ContractService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.ProfilesService;
import com.vmware.concord.Concord;
import com.vmware.concord.Concord.ConcordResponse;
import com.vmware.concord.Concord.ErrorResponse;
import com.vmware.concord.ConcordHelper;
import com.vmware.concord.IConcordConnection;

/**
 * <p>
 * GET: Used to list available RPC methods. A list of currently exposed Eth RPC methods is read from the config file and
 * returned to the client.
 * </p>
 *
 * <p>
 * url endpoint : /api/concord/eth
 * </p>
 *
 * <p>
 * POST: Used to execute the specified method. Request and response construction are handled by the appropriate
 * handlers. A TCP socket connection is made to Concord and requests and responses are encoded in the Google Protocol
 * Buffer format. Also supports a group of requests.
 * </p>
 */
@Controller
public class EthDispatcher extends ConcordServlet {
    private static final long serialVersionUID = 1L;
    public static long netVersion;
    public static boolean netVersionSet;
    private static Logger logger = LogManager.getLogger("ethLogger");
    private JSONArray rpcList;
    private String jsonRpc;
    private ContractService registryManager;
    private ProfilesService profilesRegistryManager;
    private AuthHelper authHelper;

    @Autowired
    public EthDispatcher(ContractService registryManager,
            ConnectionPoolManager connectionPoolManager,
            ProfilesService profilesRegistryManager,
            DefaultProfiles defaultProfiles,
                         AuthHelper authHelper) throws ParseException {
        super(connectionPoolManager, defaultProfiles);
        JSONParser p = new JSONParser();
        this.registryManager = registryManager;
        this.profilesRegistryManager = profilesRegistryManager;
        this.authHelper = authHelper;
        try {
            rpcList = (JSONArray) p.parse(Constants.ETHRPC_LIST);
            jsonRpc = Constants.JSONRPC;
        } catch (Exception e) {
            logger.error("Failed to read RPC information from config file", e);
        }
    }

    /**
     * Constructs the response in case of error.
     *
     * @param message Error message
     * @param id      Request Id
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
                throw new EthRpcHandlerException(ErrorCodeType.INVALID_METHOD_TYPE);
            }
        } else {
            throw new EthRpcHandlerException(ErrorCodeType.METHOD_UNSPECIFIED);
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
                throw new EthRpcHandlerException(ErrorCodeType.ID_TYPE_WRONG);
            }
        } else {
            throw new EthRpcHandlerException(ErrorCodeType.ID_UNSPECIFIED);
        }
    }

    /**
     * Services the Get request for listing currently exposed Eth RPCs. Retrieves the list from the configurations file
     * and returns it to the client.
     */
    @RequestMapping(path = "/api/concord/eth", method = RequestMethod.GET)
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<JSONAware> doGet() {
        UserDetails userDetails = (UserDetails) authHelper.getDetails();
        UUID organizationId = profilesRegistryManager.getUserOrganizationIdWithEmail(userDetails.getUsername());
        UUID consortiumId = profilesRegistryManager.getUserConsortiumIdWithEmail(userDetails.getUsername());
        ThreadContext.put("organization_id", organizationId.toString());
        ThreadContext.put("consortium_id", consortiumId == null ? "Unknown" : consortiumId.toString());
        ThreadContext.put("uri", "/api/concord/eth");
        ThreadContext.put("source", "rpcList");
        ThreadContext.put("method", "GET");
        if (rpcList == null) {
            logger.error("Configurations not read.");
            return new ResponseEntity<>(new JSONArray(), HttpStatus.SERVICE_UNAVAILABLE);
        }
        logger.info("Request Eth RPC API list");
        ThreadContext.clearAll();
        return new ResponseEntity<>(rpcList, HttpStatus.OK);
    }

    /**
     * Services the POST request for executing the specified methods. Retrieves the request parameters and calls the
     * dispatch function. Builds the response for sending to client.
     */
    @SuppressWarnings("unchecked")
    @RequestMapping(path = {"/api/concord/eth", "/api/blockchains/{id}/concord/eth"}, method = RequestMethod.POST)
    @PreAuthorize("@authHelper.canAccessChain(#id)")
    public ResponseEntity<JSONAware> doPost(@PathVariable(name = "id", required = false) Optional<UUID> id,
                                            @RequestBody String paramString) {
        // Retrieve the request fields
        JSONArray batchRequest = null;
        JSONArray batchResponse = new JSONArray();
        JSONParser parser = new JSONParser();
        JSONAware responseBody;
        boolean isBatch = false;
        ResponseEntity<JSONAware> responseEntity;

        UserDetails userDetails = (UserDetails) authHelper.getDetails();
        UUID organizationId = profilesRegistryManager.getUserOrganizationIdWithEmail(userDetails.getUsername());
        UUID consortiumId = profilesRegistryManager.getUserConsortiumIdWithEmail(userDetails.getUsername());
        ThreadContext.put("organization_id", organizationId.toString());
        ThreadContext.put("consortium_id", consortiumId == null ? "Unknown" : consortiumId.toString());
        ThreadContext.put("method", "POST");
        ThreadContext.put("uri", "/api/concord/eth");
        try {
            logger.debug("Request Parameters: " + paramString);

            // If we receive a single request, add it to a JSONArray for the sake
            // of uniformity.
            if (paramString.startsWith("[")) {
                isBatch = true;
                batchRequest = (JSONArray) parser.parse(paramString);
                if (batchRequest == null || batchRequest.size() == 0) {
                    throw new BadRequestException(ErrorCodeType.BAD_REQUEST);
                }
            } else {
                batchRequest = new JSONArray();
                batchRequest.add(parser.parse(paramString));
            }

            for (Object params : batchRequest) {
                JSONObject requestParams = (JSONObject) params;

                // Dispatch requests to the corresponding handlers
                batchResponse.add(dispatch(id, requestParams));
            }
            if (isBatch) {
                responseBody = batchResponse;
            } else {
                responseBody = (JSONObject) batchResponse.get(0);
            }
        } catch (ParseException e) {
            logger.error(ErrorCodeType.INVALID_REQUEST, e);
            responseBody = errorMessage("Unable to parse request", -1, jsonRpc);
        } catch (Exception e) {
            logger.error(ApiHelper.exceptionToString(e));
            responseBody = errorMessage(e.getMessage(), -1, jsonRpc);
        } finally {
            ThreadContext.clearAll();
        }
        logger.debug("Response: " + responseBody.toJSONString());
        return new ResponseEntity<>(responseBody, HttpStatus.OK);
    }


    /**
     * Creates the appropriate handler object and calls its functions to construct an ConcordRequest object. Sends this
     * request to Concord and converts its response into a format required by the user.
     *
     * @param requestJson Request parameters
     * @return Response for user
     */
    public JSONObject dispatch(Optional<UUID> blockchain, JSONObject requestJson) throws Exception {
        // Default initialize variables, so that if exception is thrown
        // while initializing the variables error message can be constructed
        // with default values.
        long id = -1;
        String ethMethodName;
        AbstractEthRpcHandler handler;
        boolean isLocal = false;
        JSONObject responseObject;
        ConcordResponse concordResponse;
        ConcordConnectionPool concordConnectionPool = getConnectionPool(blockchain);
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
                    if (requestJson.containsKey("isInternalContract")) {
                        requestJson.remove("isInternalContract");
                        handler = new EthSendTxHandler(connectionPoolManager, defaultProfiles,
                                profilesRegistryManager, blockchain, registryManager, true, authHelper);
                    } else {
                        handler = new EthSendTxHandler(connectionPoolManager, defaultProfiles, profilesRegistryManager,
                                blockchain, registryManager, false, authHelper);
                    }
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
                    handler = new EthLocalResponseHandler(authHelper);
                    isLocal = netVersionSet;
                    break;

                case Constants.WEB3_SHA3_NAME:
                case Constants.RPC_MODULES_NAME:
                case Constants.COINBASE_NAME:
                case Constants.CLIENT_VERSION_NAME:
                case Constants.MINING_NAME:
                case Constants.NEWACCOUNT_NAME:
                case Constants.ACCOUNTS_NAME:
                case Constants.GAS_PRICE_NAME:
                case Constants.ESTIMATE_GAS_NAME:
                case Constants.SYNCING_NAME:
                    handler = new EthLocalResponseHandler(authHelper);
                    isLocal = true;
                    break;

                case Constants.BLOCKNUMBER_NAME:
                    handler = new EthBlockNumberHandler();
                    break;

                case Constants.GET_LOGS_NAME:
                    handler = new EthGetLogsHandler();
                    break;

                default:
                    throw new BadRequestException(ErrorCodeType.INVALID_METHOD_NAME);
            }

            if (!isLocal) {
                Concord.ConcordRequest.Builder concordRequestBuilder = Concord.ConcordRequest.newBuilder();
                if (handler.buildRequest(concordRequestBuilder, requestJson)) {
                    concordResponse = communicateWithConcord(concordConnectionPool, concordRequestBuilder.build());
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
     * Sends an ConcordRequest to Concord and receives Concord's response.
     *
     * @param req ConcordRequest object
     * @return Response received from Concord
     */
    private ConcordResponse communicateWithConcord(ConcordConnectionPool concordConnectionPool,
                                                   Concord.ConcordRequest req) throws Exception {
        IConcordConnection conn = null;
        Concord.ConcordResponse concordResponse = null;
        try {
            conn = concordConnectionPool.getConnection();
            if (conn == null) {
                throw new BadRequestException(ErrorCodeType.CONCORD_CONNECTION);
            }

            boolean res = ConcordHelper.sendToConcord(req, conn);
            if (!res) {
                throw new BadRequestException(ErrorCodeType.CONCORD_CONNECTION);
            }

            // receive response from Concord
            concordResponse = ConcordHelper.receiveFromConcord(conn);
            if (concordResponse == null) {
                throw new BadRequestException(ErrorCodeType.CONCORD_CONNECTION);
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
     * Not required for this Servlet as each handler builds its response object separately.
     */
    @Override
    public JSONAware parseToJson(ConcordResponse concordResponse) {
        throw new BadRequestException(ErrorCodeType.JSON_METHOD_UNSUPPORTED);
    }
}
