/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.connections.IAthenaConnection;

/**
 * <p>
 * Handles the RPC requests which can be processes directly in helen without forwarding them to Athena. However, this is
 * an exception. When handling `net_version` we do connect to athena but just get the version number from athena. This
 * handler handles following eth RPC requests :
 * <ul>
 * <li>web3_sha3</li>
 * <li>eth_coinbase</li>
 * <li>web3_clientVersion</li>
 * <li>eth_mining</li>
 * <li>net_version</li>
 * <li>eth_accounts</li>
 * <li>rpc_modules</li>
 * <li>eth_gasPrice</li>
 * <li>eth_syncing</li>
 * </ul>
 * </p>
 */
public class EthLocalResponseHandler extends AbstractEthRpcHandler {

    private AthenaConnectionPool connectionPool;

    private static Logger logger = LogManager.getLogger(EthLocalResponseHandler.class);

    /**
     * This method does not build any request since we do not need to send any request to Athena for requests handled by
     * this handler. However, having an empty method like this is probably not a very good idea. TODO: Figure out how to
     * remove this empty method.
     *
     */
    public void buildRequest(Athena.AthenaRequest.Builder athenaRequestBuilder, JSONObject requestJson)
            throws Exception {}

    /**
     * Initializes response object by using request JSONObject for local methods. An overload of this method which
     * accepts EthResponse as input parameter is defined in {@link AbstractEthRpcHandler} class. However, for requests
     * which can be handled locally we do not even call athena and hence do not have a valid athena response. Hence we
     * can not use the method provided by parent class.
     *
     */
    @SuppressWarnings("unchecked")
    JSONObject initializeResponseObject(JSONObject requestJson) {
        JSONObject respObject = new JSONObject();
        respObject.put("id", requestJson.get("id"));
        respObject.put("jsonrpc", Constants.JSONRPC);
        return respObject;
    }

    /**
     * Builds the response JSONObject by processing the given type of request locally. For example if the request is of
     * type `web3_sha3` then here we will actually generate the hash of given data and produce a response object
     * containing that hash. We do not do kind of processing in `buildRequest` method.
     *
     * @param athenaResponse The response receive from athena Note: Since, we do not build anything in
     *        buildAthenaRequest and we also do not call athena for these requests. Hence this method completely ignores
     *        the athenaResponse object. It can be NULL
     * @param requestJson The original RPC request JSONObject.
     * @return the JSONObject of the response.
     */
    @SuppressWarnings("unchecked")
    public JSONObject buildResponse(Athena.AthenaResponse athenaResponse, JSONObject requestJson) throws Exception {
        long id = (long) requestJson.get("id");
        String ethMethodName = EthDispatcher.getEthMethodName(requestJson);
        /*
         * Here we can not use parents initializeResponseObject method because it takes a valid EthResponse object as
         * input parameter, however in local methods we do not even call athena and hence do not have a valid value for
         * that parameter. Instead we provide our own overload of initializeResponseObject method in this class which
         * initializes object with requestJson object.
         */
        JSONObject result = initializeResponseObject(requestJson);

        Object localData = null;

        if (ethMethodName.equals(Constants.WEB3_SHA3_NAME)) {
            JSONArray params = extractRequestParams(requestJson);
            // Request should contain just one param value
            if (params.size() != 1) {
                logger.error("Invalid request parameter : params");
                throw new EthRpcHandlerException(EthDispatcher
                        .errorMessage("'params' must contain only one element", id, jsonRpc).toJSONString());
            }

            try {
                localData = ApiHelper.getKeccak256Hash((String) params.get(0));
                logger.info("Generated keccak hash is: " + localData);
            } catch (Exception e) {
                logger.error("Error in calculating Keccak hash", e);
                throw new EthRpcHandlerException(
                        EthDispatcher.errorMessage("'invalid param", id, jsonRpc).toJSONString());
            }
        } else if (ethMethodName.equals(Constants.RPC_MODULES_NAME)) {
            JSONParser p = new JSONParser();
            localData = (((JSONArray) p.parse(Constants.RPC_MODULES)).get(0));
        } else if (ethMethodName.equals(Constants.COINBASE_NAME)) {
            localData = Constants.COINBASE;
        } else if (ethMethodName.equals(Constants.CLIENT_VERSION_NAME)) {
            localData = Constants.CLIENT_VERSION;
        } else if (ethMethodName.equals(Constants.MINING_NAME)) {
            localData = Constants.IS_MINING == 0 ? false : true;
        } else if (ethMethodName.equals(Constants.NETVERSION_NAME)) {
            if (!EthDispatcher.netVersionSet) {
                // The act of creating a connection retrieves info about athena.
                IAthenaConnection conn = null;
                try {
                    conn = connectionPool.getConnection();
                    conn.check();
                } catch (IllegalStateException | InterruptedException e) {
                    e.printStackTrace();
                } catch (Exception e) {
                    logger.error("Unable to connect to athena.", e);
                    throw new EthRpcHandlerException(
                            EthDispatcher.errorMessage("Unable to connect to athena.", id, jsonRpc).toJSONString());
                } finally {
                    if (conn != null) {
                        connectionPool.putConnection(conn);
                    }
                }
            } else {
                EthDispatcher.netVersionSet = true;
            }
            localData = EthDispatcher.netVersion;
        } else if (ethMethodName.equals(Constants.ACCOUNTS_NAME)) {
            JSONArray usersJsonArr = new JSONArray();
            String usersStr = Constants.USERS;
            if (usersStr != null && !usersStr.trim().isEmpty()) {
                String[] usersArr = usersStr.split(",");
                for (int i = 0; i < usersArr.length; i++) {
                    usersJsonArr.add(usersArr[i]);
                }
            }
            localData = usersJsonArr;
        } else if (ethMethodName.equals(Constants.GAS_PRICE_NAME)) {
            localData = Constants.GAS_PRICE;
        } else if (ethMethodName.equals(Constants.SYNCING_NAME)) {
            // "false" in this context means that the node believes it is up to
            // date. In the future, we may use this to share when a node knows that
            // it is processing a state transfer, but for now, all nodes believe
            // they are always up to date.
            localData = false;
        }

        result.put("result", localData);

        return result;
    }

}
