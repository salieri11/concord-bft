/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.athena;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.google.protobuf.ByteString;
import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;

/**
 * Servlet class.
 */
@Controller
public final class Transaction extends BaseServlet {
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LogManager.getLogger(Transaction.class);

    @Autowired
    public Transaction(AthenaProperties config, AthenaConnectionPool athenaConnectionPool) {
        super(config, athenaConnectionPool);
        // TODO Auto-generated constructor stub
    }

    protected static JSONObject buildTransactionResponseJson(Athena.TransactionResponse tr) {
        // Construct the reponse JSON object.
        JSONObject responseJson = new JSONObject();

        responseJson.put("hash", ApiHelper.binaryStringToHex(tr.getHash()));
        responseJson.put("from", ApiHelper.binaryStringToHex(tr.getFrom()));
        responseJson.put("block_hash", ApiHelper.binaryStringToHex(tr.getBlockHash()));
        responseJson.put("block_number", tr.getBlockNumber());

        if (tr.hasTo()) {
            responseJson.put("to", ApiHelper.binaryStringToHex(tr.getTo()));
        }

        if (tr.hasContractAddress()) {
            responseJson.put("contract_address", ApiHelper.binaryStringToHex(tr.getContractAddress()));
        }

        if (tr.hasValue()) {
            responseJson.put("value", Long.toString(tr.getValue()));
        }

        if (tr.hasInput()) {
            responseJson.put("input", ApiHelper.binaryStringToHex(tr.getInput()));
        }
        responseJson.put("nonce", tr.getNonce());

        return responseJson;
    }

    /**
     * Services a get request. Constructs a protobuf request of type transaction request (enveloped in an athena
     * request) as defined in athena.proto. Sends this request to Athena. Parses the response and converts it into json
     * for responding to the client.
     *
     * @param request The request received by the servlet
     * @param response The response object used to respond to the client
     */
    @RequestMapping(path = "/api/athena/transactions/{hash}", method = RequestMethod.GET)
    protected ResponseEntity<JSONAware> doGet(@PathVariable(value = "hash", required = true) String hash) {
        ResponseEntity<JSONAware> responseEntity;
        ByteString hashBytes = null;
        try {
            hashBytes = ApiHelper.hexStringToBinary(hash);
        } catch (Exception e) {
            logger.error("Invalid Hash");
            return new ResponseEntity<>(new JSONObject(), standardHeaders, HttpStatus.BAD_REQUEST);
        }

        if (hashBytes == null) {
            logger.error("Invalid hash in request");
            return new ResponseEntity<>(new JSONObject(), standardHeaders, HttpStatus.BAD_REQUEST);
        }

        // Construct a transaction request object.
        final Athena.TransactionRequest txRequestObj =
                Athena.TransactionRequest.newBuilder().setHash(hashBytes).build();

        // Envelope the transaction request object into an athena object.
        final Athena.AthenaRequest athenarequestObj =
                Athena.AthenaRequest.newBuilder().setTransactionRequest(txRequestObj).build();

        return sendToAthenaAndBuildHelenResponse(athenarequestObj);
    }

    /**
     * Parses the Protocol Buffer response from Athena and converts it into JSON.
     *
     * @param athenaResponse Protocol Buffer object containing Athena's reponse
     * @return Response in JSON format
     */
    @SuppressWarnings("unchecked")
    @Override
    protected JSONObject parseToJson(Athena.AthenaResponse athenaResponse) {

        // Extract the transaction response from
        // the athena reponse envelope.
        Athena.TransactionResponse txResponse = athenaResponse.getTransactionResponse();

        JSONObject responseJson = buildTransactionResponseJson(txResponse);

        // Athena EVM has status code '0' for success and other Positive
        // values to denote error. However, for JSON RPC '1' is success
        // and '0' is failure. Here we need to reverse status value of athena
        // response before returning it.
        responseJson.put("status", txResponse.getStatus() == 0 ? 1 : 0);

        return responseJson;
    }
}
